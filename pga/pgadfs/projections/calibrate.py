"""Pinning the simulator to things that are known.

Three quantities have to be right before any of the output means anything,
and none of them should be guessed:

1. **The stroke scale.** A golfer DataGolf rates one stroke per round better
   than another has to actually average one stroke per round better in
   simulation. That fixes `latent_per_stroke`.
2. **The course.** The field has to average the score the event is expected
   to be played in. That fixes `course_shift`.
3. **The spread.** How often the best golfer in the field actually wins is
   not a free parameter -- it is priced, by DataGolf's model and by the
   sportsbooks, and both are published. That fixes the variance.

The third one is the interesting one. DataGolf masks player names on the
finish-odds page outside its top five, so those prices cannot be joined to
a golfer. They do not need to be: what the simulator has to reproduce is the
*shape* of the field's win distribution -- the best player's win
probability, the second's, and so on down -- and that comparison is
identity-free. Sorting both sides and matching them is a legitimate fit, not
a workaround, as long as the ordering itself is not also being fitted. It
isn't: the ordering comes from DataGolf's skill ratings.
"""

from __future__ import annotations

from dataclasses import dataclass, replace

import numpy as np
from scipy.optimize import brentq, minimize

from ..config import FINISH_POINTS, Course, SimConfig
from ..data import datagolf as dg
from ..data.dkpoints import load_sample
from ..sim.engine import finish_points, simulate, simulate_strokes
from ..sim.holes import HoleModel
from ..slate import Slate

_GH_NODES, _GH_WEIGHTS = np.polynomial.hermite.hermgauss(15)


def _expected_round_score(model: HoleModel, edge: np.ndarray, noise_sd: float) -> np.ndarray:
    """E[round score to par] for a talent edge, integrating out round noise.

    The map from talent to score is mildly non-linear, so averaging over the
    noise is not the same as evaluating at the mean. Gauss-Hermite does it in
    fifteen points.
    """
    edge = np.atleast_1d(np.asarray(edge, dtype=float))
    if noise_sd <= 0:
        return model.expected_strokes(edge)
    shifted = edge[:, None] + np.sqrt(2.0) * noise_sd * _GH_NODES[None, :]
    scores = model.expected_strokes(shifted.ravel()).reshape(shifted.shape)
    return (scores * _GH_WEIGHTS).sum(axis=1) / np.sqrt(np.pi)


@dataclass(frozen=True)
class Calibration:
    latent_per_stroke: float
    course_shift: float
    talent_multiplier: float
    week_sd: float
    tie_rule: str
    field_score_to_par: float
    diagnostics: dict

    def model(self, course: Course) -> HoleModel:
        return HoleModel(course, self.latent_per_stroke, self.course_shift)


def fit_course(
    course: Course,
    edges: np.ndarray,
    target_score_to_par: float,
    noise_sd: float,
) -> tuple[float, float]:
    """Solve `latent_per_stroke` and `course_shift` together.

    Two conditions, two unknowns, both monotone: a bigger latent scale
    spreads scores out, a bigger shift moves them all down. Nested bisection
    is exact enough and needs no starting guess.
    """

    def shift_for(mu: float) -> float:
        def gap(shift: float) -> float:
            model = HoleModel(course, mu, shift)
            return float(_expected_round_score(model, edges, noise_sd).mean()) - target_score_to_par

        return brentq(gap, -6.0, 6.0, xtol=1e-8)

    def slope_error(mu: float) -> float:
        model = HoleModel(course, mu, shift_for(mu))
        lo, hi = _expected_round_score(model, np.array([0.5, -0.5]), noise_sd)
        return float(hi - lo) - 1.0

    mu = brentq(slope_error, 0.2, 40.0, xtol=1e-6)
    return mu, shift_for(mu)


def _sorted_probability_vectors(strokes: np.ndarray, markets: dict[str, int]) -> dict[str, np.ndarray]:
    position, _ = finish_points(strokes, "best")
    out = {}
    for name, k in markets.items():
        out[name] = np.sort((position <= k).mean(axis=0))[::-1]
    return out


def fit_variance(
    slate: Slate,
    cfg: SimConfig,
    course: Course,
    targets: dict[str, np.ndarray],
    *,
    n_sims: int = 6000,
    markets: dict[str, int] | None = None,
) -> tuple[float, float, dict]:
    """Fit the talent multiplier and week-to-week spread to market prices.

    Two parameters against four sorted probability curves. They are not
    redundant: scaling talent stretches the whole curve, while week_sd
    flattens the top of it much more than the tail, so win and top-20 pull
    the two apart.

    A talent multiplier above 1 means the market believes this field is more
    spread out than DataGolf's general skill ratings say -- which is the
    normal case for an elite no-cut field, where the ratings are compressed
    by the strength of the opposition everyone has been facing.
    """
    markets = markets or {"win": 1, "top_5": 5, "top_10": 10, "top_20": 20}
    base_talent = slate.talent
    centre = base_talent.mean()
    trace: list[tuple[float, float, float]] = []

    def objective(params: np.ndarray) -> float:
        mult, week_sd = float(params[0]), float(params[1])
        if not (0.3 <= mult <= 3.0 and 0.05 <= week_sd <= 3.0):
            return 1e6
        edges = (base_talent - centre) * mult
        noise_sd = float(np.hypot(week_sd, cfg.round_sd))
        mu, shift = fit_course(course, edges, slate.field_score_to_par, noise_sd)
        scaled = _scaled_slate(slate, edges)
        run_cfg = replace(cfg, n_sims=n_sims, week_sd=week_sd)
        strokes = simulate_strokes(scaled, run_cfg, HoleModel(course, mu, shift))
        sim = _sorted_probability_vectors(strokes, markets)
        err = 0.0
        for name, target in targets.items():
            if name not in sim:
                continue
            # Normalising by the number of places keeps top-20, which has
            # twenty times the total probability mass of win, from drowning
            # the win curve out.
            err += float(((sim[name] - target) ** 2).sum()) / markets[name]
        trace.append((mult, week_sd, err))
        return err

    best = minimize(
        objective,
        x0=np.array([1.0, cfg.week_sd]),
        method="Nelder-Mead",
        options={"xatol": 1e-3, "fatol": 1e-8, "maxiter": 120},
    )
    mult, week_sd = float(best.x[0]), float(best.x[1])
    return mult, week_sd, {"objective": float(best.fun), "n_evaluations": len(trace)}


def _scaled_slate(slate: Slate, edges: np.ndarray) -> Slate:
    """A slate whose `edge` is exactly `edges`, leaving everything else alone."""
    centre = slate.talent.mean()
    golfers = tuple(
        replace(g, skill=float(e) + centre - g.course_fit)
        for g, e in zip(slate.golfers, edges)
    )
    return replace(slate, golfers=golfers)


def apply_calibration(slate: Slate, cal: "Calibration") -> Slate:
    """Stretch the slate's talent spread by the fitted multiplier."""
    edges = (slate.talent - slate.talent.mean()) * cal.talent_multiplier
    return _scaled_slate(slate, edges)


def fit_scoring_level(
    slate: Slate,
    cfg: SimConfig,
    course: Course,
    edges: np.ndarray,
    *,
    n_sims: int = 8000,
    lo: float = -2.0,
    hi: float = 2.0,
) -> tuple[float, dict]:
    """Fit how the course will actually play, from DataGolf's DK projections.

    The obvious source for this is DataGolf's own `predicted_score` on the
    fantasy page, and the obvious reading of it -- the field's expected score
    to par -- turns out to be wrong: taking it at face value produces DK
    projections about thirteen points above DataGolf's own for the same
    golfers, which is a birdie a round too many.

    So the scoring level is fitted against the thing that is unambiguous.
    DataGolf publishes projected DraftKings points for the five golfers it
    does not mask, and those numbers are denominated in exactly what this
    package produces. One parameter, five anchors, and a monotone
    relationship: an easier course means more birdies means more points.
    """
    anchors = {
        i: g.dg_points for i, g in enumerate(slate.golfers) if g.dg_points is not None
    }
    if not anchors:
        return cfg.field_score_to_par, {"reason": "no published projections to fit against"}

    idx = np.fromiter(anchors, dtype=int)
    target = np.fromiter(anchors.values(), dtype=float)
    noise_sd = float(np.hypot(cfg.week_sd, cfg.round_sd))
    scaled = _scaled_slate(slate, edges)
    cache: dict[float, float] = {}

    def error(level: float) -> float:
        if level not in cache:
            mu, shift = fit_course(course, edges, level, noise_sd)
            run = replace(cfg, n_sims=n_sims, field_score_to_par=level)
            got = simulate(scaled, run, HoleModel(course, mu, shift)).mean()[idx]
            cache[level] = float(np.mean(got - target))
        return cache[level]

    if error(lo) * error(hi) > 0:
        best = min((lo, hi), key=lambda x: abs(error(x)))
        return best, {"reason": "anchors outside the bracket", "residual": error(best)}

    level = brentq(error, lo, hi, xtol=1e-3, rtol=1e-4)
    mu, shift = fit_course(course, edges, level, noise_sd)
    run = replace(cfg, n_sims=n_sims, field_score_to_par=level)
    got = simulate(scaled, run, HoleModel(course, mu, shift)).mean()
    return level, {
        "field_score_to_par": level,
        "published": {slate.golfers[i].name: float(v) for i, v in anchors.items()},
        "simulated": {slate.golfers[i].name: float(got[i]) for i in anchors},
        "rmse": float(np.sqrt(np.mean((got[idx] - target) ** 2))),
    }


def tie_rule_from_real_output() -> tuple[str, dict] | None:
    """Read the tie rule straight off a real DraftKings scoring file.

    This is not an inference. The sample event contains golfers who finished
    T2, T4, T8, T17 and so on, next to the finish points DraftKings actually
    awarded them, and every one of them got the points for the top of the
    tie. A T2 is 20, not the 19 that averaging the band would give.
    """
    try:
        rows = [r for r in load_sample() if r.tied and r.position and r.position <= len(FINISH_POINTS)]
    except (OSError, KeyError):
        return None
    if not rows:
        return None
    best = sum(r.finish_pts == FINISH_POINTS[r.position - 1] for r in rows)
    if best != len(rows):
        return None
    return "best", {
        "source": "observed DraftKings output",
        "tied_finishes_checked": len(rows),
        "example": {r.finish_text: r.finish_pts for r in rows[:8]},
    }


def fit_tie_rule(slate: Slate, cfg: SimConfig, model: HoleModel) -> tuple[str, dict]:
    """Decide how DraftKings splits a tie.

    First from real scored output, which settles it outright. Only if that is
    unavailable does it fall back to simulating the leaderboard under both
    conventions and seeing which lands on DataGolf's published expected
    finish points for the five golfers it does not mask.
    """
    observed = tie_rule_from_real_output()
    if observed is not None:
        return observed

    strokes = simulate_strokes(slate, replace(cfg, n_sims=max(cfg.n_sims, 8000)), model)
    published = {
        i: g.dg_finish_points
        for i, g in enumerate(slate.golfers)
        if g.dg_finish_points is not None
    }
    if not published:
        return "best", {"reason": "no published finish points to compare against"}

    errors = {}
    means = {}
    for rule in ("best", "average"):
        _, pts = finish_points(strokes, rule)
        got = pts.mean(axis=0)
        means[rule] = {slate.golfers[i].name: float(got[i]) for i in published}
        errors[rule] = float(
            np.sqrt(np.mean([(got[i] - v) ** 2 for i, v in published.items()]))
        )
    rule = min(errors, key=errors.__getitem__)
    return rule, {
        "rmse": errors,
        "simulated": means,
        "published": {slate.golfers[i].name: v for i, v in published.items()},
    }


def calibrate(
    slate: Slate,
    cfg: SimConfig,
    *,
    market_weight: float = 0.5,
    n_sims: int = 6000,
    offline: bool = False,
    passes: int = 2,
) -> Calibration:
    """Run the whole fit and return the parameters the simulator should use."""
    odds = dg.load_finish_odds(offline=offline)
    model_v = dg.finish_probability_vectors(odds, "model")
    market_v = dg.finish_probability_vectors(odds, "market")
    targets = {
        k: (1 - market_weight) * np.asarray(model_v[k])
        + market_weight * np.asarray(market_v.get(k, model_v[k]))
        for k in model_v
    }

    mult, week_sd = 1.0, cfg.week_sd
    info: dict = {}
    for _ in range(passes):
        mult, week_sd, info = fit_variance(
            slate, replace(cfg, week_sd=week_sd), cfg.course, targets, n_sims=n_sims
        )

    edges = (slate.talent - slate.talent.mean()) * mult
    noise_sd = float(np.hypot(week_sd, cfg.round_sd))

    level, level_info = fit_scoring_level(
        slate, replace(cfg, week_sd=week_sd), cfg.course, edges
    )
    mu, shift = fit_course(cfg.course, edges, level, noise_sd)

    scaled = _scaled_slate(slate, edges)
    tie_rule, tie_info = fit_tie_rule(
        scaled, replace(cfg, week_sd=week_sd), HoleModel(cfg.course, mu, shift)
    )

    return Calibration(
        latent_per_stroke=mu,
        course_shift=shift,
        talent_multiplier=mult,
        week_sd=week_sd,
        tie_rule=tie_rule,
        field_score_to_par=level,
        diagnostics={
            "variance_fit": info,
            "scoring_level": level_info,
            "tie_rule": tie_info,
        },
    )
