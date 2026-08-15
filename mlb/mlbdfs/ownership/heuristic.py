"""Structural ownership projection.

Ownership is not a random process, so it is not simulated here -- it is
modelled. The field responds to a small set of things it can see before
lock: salary, projected points, points per dollar, the Vegas team total,
batting order, and whether a player is part of the obvious stack.

The model is a conditional logit fit per position group rather than a
per-player regression, for one reason that matters: **ownership has to add
up**. Across a slate, total ownership at a position equals one hundred
percent times the number of roster spots there. Independent per-player
predictions violate that and produce incoherent slates where every good
player is 40% owned. The logit form satisfies it by construction, and it
encodes the fact that ownership is competitive -- chalk on one catcher
necessarily suppresses every other catcher.

Version one uses hand-set weights calibrated to plausible priors. The
fitted version swaps in behind the same interface once
``ownership/logger.py`` has accumulated enough real contest data; nothing
downstream needs to change.
"""

from __future__ import annotations

import logging

import numpy as np
import pandas as pd

from ..config import OWNERSHIP, ROSTER, OwnershipConfig
from ..sim.engine import SimResult
from ..slate import Slate

# The chalkiest play on a slate is rarely above sixty percent even in a
# large-field tournament, and never approaches one hundred.
MAX_OWNERSHIP = 0.65
MIN_OWNERSHIP = 0.0015


def primary_position(positions: tuple[str, ...]) -> str:
    """The position group a player is counted against for ownership.

    Multi-eligible players are assigned to their scarcest listed position,
    which is where the field is most likely to use them.
    """
    order = ["C", "SS", "2B", "3B", "1B", "OF", "P"]
    for pos in order:
        if pos in positions:
            return pos
    return positions[0]


def _zscore(x: np.ndarray) -> np.ndarray:
    sd = x.std()
    if sd < 1e-9:
        return np.zeros_like(x)
    return (x - x.mean()) / sd


def _utility(g: pd.DataFrame, position: str, cfg: OwnershipConfig) -> np.ndarray:
    """Fitted utility for one position group.

    Pitchers carry their own coefficients: the field buys ceiling in a
    starter and ceiling plus price in a hitter, and averaging the two into
    one vector describes neither.
    """
    if position == "P":
        return (
            cfg.wp_value * _zscore(g["value"].to_numpy())
            + cfg.wp_points * _zscore(g["proj"].to_numpy())
            + cfg.wp_ceiling * _zscore(g["ceiling"].to_numpy())
            + cfg.wp_team_total * _zscore(g["team_total"].to_numpy())
            + cfg.wp_salary * _zscore(g["salary"].to_numpy())
        )
    return (
        cfg.w_value * _zscore(g["value"].to_numpy())
        + cfg.w_points * _zscore(g["proj"].to_numpy())
        + cfg.w_ceiling * _zscore(g["ceiling"].to_numpy())
        + cfg.w_team_total * _zscore(g["team_total"].to_numpy())
        + cfg.w_salary * _zscore(g["salary"].to_numpy())
        + cfg.w_order_top * (g["batting_order"].to_numpy() <= 5).astype(float)
    )


def build_features(slate: Slate, sim: SimResult) -> pd.DataFrame:
    """Assemble the pre-lock features the field actually reacts to."""
    summary = sim.summary().set_index("player_id")

    rows = []
    for p in slate.players:
        if p.player_id not in summary.index:
            continue
        stats = summary.loc[p.player_id]
        game = slate.game_for(p.team)
        rows.append(
            {
                "player_id": p.player_id,
                "name": p.name,
                "team": p.team,
                "position": primary_position(p.positions),
                "is_pitcher": p.is_pitcher,
                "salary": p.salary,
                "proj": float(stats["mean"]),
                "ceiling": float(stats["p90"]),
                "value": float(stats["mean"]) / (p.salary / 1000.0),
                "team_total": game.implied_total(p.team),
                "batting_order": p.batting_order or 0,
            }
        )
    return pd.DataFrame(rows)


def project_ownership(
    slate: Slate,
    sim: SimResult,
    cfg: OwnershipConfig = OWNERSHIP,
    features: pd.DataFrame | None = None,
) -> pd.DataFrame:
    """Project ownership for every player on the slate.

    Returns a frame with an ``ownership`` column expressed as a fraction of
    entries. Within each position group it sums to the number of roster
    slots that group carries -- 3.0 across outfielders, 2.0 across pitchers,
    1.0 across catchers.
    """
    feats = build_features(slate, sim) if features is None else features.copy()
    slots = dict(ROSTER.slots)

    out = []
    for position, group in feats.groupby("position", sort=False):
        g = group.copy()
        n_slots = float(slots.get(position, 1))

        utility = _utility(g, position, cfg)

        g["utility"] = utility
        g["ownership"] = _logit_shares(utility, n_slots)
        out.append(g)

    result = enforce_salary_feasibility(pd.concat(out, ignore_index=True))
    return result.sort_values("ownership", ascending=False).reset_index(drop=True)


def project_ownership_from_features(
    features: pd.DataFrame, cfg: OwnershipConfig = OWNERSHIP
) -> pd.DataFrame:
    """Hand-set-weight ownership for a prebuilt features frame.

    Same model as ``project_ownership``, entered one step later so a fitted
    model can be scored against it on identical inputs.
    """
    slots = dict(ROSTER.slots)
    out = []
    for position, group in features.groupby("position", sort=False):
        g = group.copy()
        utility = _utility(g, position, cfg)
        if position != "P":
            utility = utility + cfg.w_order_top * (
                g["batting_order"].to_numpy() <= 5
            ).astype(float)
        g["utility"] = utility
        g["ownership"] = _logit_shares(utility, float(slots.get(position, 1)))
        out.append(g)
    return enforce_salary_feasibility(pd.concat(out, ignore_index=True))


def _logit_shares(utility: np.ndarray, n_slots: float) -> np.ndarray:
    """Softmax utilities into ownership summing to ``n_slots``, then cap.

    Capping breaks the sum, so the excess is redistributed over the
    uncapped players and the cap re-checked. In practice this converges in
    two or three passes.
    """
    exp_u = np.exp(utility - utility.max())
    shares = n_slots * exp_u / exp_u.sum()

    for _ in range(8):
        over = shares > MAX_OWNERSHIP
        if not over.any():
            break
        excess = (shares[over] - MAX_OWNERSHIP).sum()
        shares[over] = MAX_OWNERSHIP
        free = ~over
        if not free.any() or shares[free].sum() <= 0:
            break
        shares[free] += excess * shares[free] / shares[free].sum()

    return np.clip(shares, MIN_OWNERSHIP, MAX_OWNERSHIP)


def expected_lineup_salary(ownership: pd.DataFrame) -> float:
    """Salary of an average field lineup, implied by an ownership vector.

    Companion to :func:`implied_field_mean`, and a hard feasibility test
    rather than a soft one. Because ownership sums to the roster slots, this
    sum *is* the expected salary of a field lineup, so a value above the cap
    means no distribution over legal lineups can produce those marginals --
    the projection is describing a field that cannot exist.

    It only started to bite once the fitted weights put a positive
    coefficient on salary. The hand-set weights were price averse, which
    kept the projection cheap by accident.
    """
    return float((ownership["ownership"] * ownership["salary"]).sum())


def enforce_salary_feasibility(
    ownership: pd.DataFrame,
    cap: int = ROSTER.salary_cap,
    headroom: float = 0.97,
    max_iter: int = 60,
) -> pd.DataFrame:
    """Tilt ownership away from salary until an average lineup fits the cap.

    Solves for the single price coefficient that brings expected lineup
    salary to ``headroom * cap``, applied on top of the fitted utilities and
    renormalized per position group so the sum-to-slots property survives.

    One parameter, because the constraint is one number. Real fields spend
    just under the cap rather than exactly at it, hence the headroom.
    """
    target = cap * headroom
    if expected_lineup_salary(ownership) <= target:
        return ownership

    slots = dict(ROSTER.slots)
    salary_z = {
        position: _zscore(g["salary"].to_numpy())
        for position, g in ownership.groupby("position", sort=False)
    }

    def tilted(lam: float) -> pd.DataFrame:
        out = []
        for position, g in ownership.groupby("position", sort=False):
            g = g.copy()
            g["ownership"] = _logit_shares(
                g["utility"].to_numpy() - lam * salary_z[position],
                float(slots.get(position, 1)),
            )
            out.append(g)
        return pd.concat(out, ignore_index=True)

    lo, hi = 0.0, 8.0
    best = tilted(hi)
    for _ in range(max_iter):
        mid = (lo + hi) / 2
        candidate = tilted(mid)
        if expected_lineup_salary(candidate) > target:
            lo = mid
        else:
            hi = mid
            best = candidate
        if hi - lo < 1e-4:
            break
    return best


def team_stack_ownership(ownership: pd.DataFrame) -> pd.DataFrame:
    """Total projected hitter ownership by team.

    The single most useful derived number on a slate: it says which offenses
    the field is piling into, and therefore where leverage is.
    """
    hitters = ownership[~ownership["is_pitcher"]]
    agg = (
        hitters.groupby("team")["ownership"]
        .sum()
        .sort_values(ascending=False)
        .rename("stack_ownership")
        .reset_index()
    )
    return agg


def leverage(ownership: pd.DataFrame, sim: SimResult, threshold: float) -> pd.DataFrame:
    """Ceiling probability relative to ownership.

    A player who reaches a tournament-winning score more often than the
    field rosters him is where tournament equity comes from. This is the
    ratio that identifies them; it is descriptive, not an instruction, since
    the optimizer works this out properly against the simulated field.
    """
    probs = {pid: p for pid, p in zip(sim.player_ids, sim.prob_above(threshold))}
    out = ownership.copy()
    out["p_ceiling"] = out["player_id"].map(probs)
    out["leverage"] = out["p_ceiling"] / out["ownership"].clip(lower=MIN_OWNERSHIP)
    return out.sort_values("leverage", ascending=False).reset_index(drop=True)


# --------------------------------------------------------------------------
# Field strength calibration
# --------------------------------------------------------------------------


def implied_field_mean(ownership: pd.DataFrame) -> float:
    """Ownership-weighted sum of player projections.

    If the ownership vector were achievable by some distribution over legal
    lineups, this would be exactly the expected score of an average field
    lineup, since ownership sums to the number of roster slots.

    Comparing it against the field the generator actually produces is one of
    the more informative checks in the project. A large gap means the
    ownership projection is not jointly feasible: there is no way to fill
    50,000 legal rosters that puts every chalk play at its projected rate,
    because the salary cap will not allow it. The heuristic model in this
    module has exactly that problem -- it prices each player independently
    and nothing enforces that the whole slate can be afforded at once.

    The gap is not signed. It is tempting to reason that infeasibility can
    only cost the field points and treat this as an upper bound, but the
    calibration that reconciles the generator to the target leaves residuals
    in both directions, and the realized field regularly comes out slightly
    above. Use it as a reference point, not as a bound.

    That is worth knowing when reading ROI numbers, and it is the strongest
    argument for replacing this model with one fitted to real contest data,
    which is feasible by construction because it came from real lineups.
    """
    return float((ownership["ownership"] * ownership["proj"]).sum())


def realized_field_mean(
    slate: Slate, ownership: pd.DataFrame, n_lineups: int = 4000, seed: int = 0
) -> float:
    """Mean projection of lineups the field generator actually produces.

    This is the number that governs how strong the simulated opposition is,
    and therefore how believable any absolute ROI figure is.
    """
    from .field import generate_field

    field = generate_field(
        slate, ownership, n_lineups=n_lineups, seed=seed, calibration_rounds=2
    )
    proj = ownership["proj"].to_numpy()[field.lineups].sum(axis=1)
    return float(proj.mean())


def field_strength_curve(
    slate: Slate,
    sim: SimResult,
    temperatures: "np.ndarray | list[float] | None" = None,
    cfg: OwnershipConfig = OWNERSHIP,
    n_lineups: int = 2500,
    seed: int = 0,
) -> pd.DataFrame:
    """Field mean score across a range of ownership temperatures.

    Worth plotting once. The relationship is **not monotonic**, which is
    easy to get wrong: the utility function is dominated by points per
    dollar, so pushing concentration to an extreme piles ownership onto
    cheap high-value players whose absolute projections are low, and field
    strength falls again. It peaks somewhere in the middle.
    """
    if temperatures is None:
        temperatures = np.geomspace(0.1, 12.0, 14)
    features = build_features(slate, sim)

    rows = []
    for t in temperatures:
        own = project_ownership(
            slate, sim, cfg=_with_temperature(cfg, float(t)), features=features
        )
        rows.append(
            {
                "temperature": float(t),
                "implied_mean": implied_field_mean(own),
                "realized_mean": realized_field_mean(slate, own, n_lineups, seed),
                "max_ownership": float(own["ownership"].max()),
            }
        )
    return pd.DataFrame(rows)


def calibrate_to_field_strength(
    slate: Slate,
    sim: SimResult,
    target_mean_score: float,
    cfg: OwnershipConfig = OWNERSHIP,
    n_lineups: int = 2500,
    seed: int = 0,
    warn: bool = True,
) -> tuple[pd.DataFrame, float]:
    """Tune ownership concentration so the field has a given mean score.

    The conditional logit's utilities are divided by a temperature: low
    temperature concentrates ownership onto the model's favourite plays,
    high temperature spreads it out. That single parameter is the honest
    place to encode "how sharp is this contest", and unlike the individual
    feature weights it can be set from something observable -- the average
    score in the contests you enter, which DraftKings reports after every
    slate.

    Calibrating matters: left alone, a diffuse ownership model implies a
    field of roughly league-average lineups, against which any competently
    optimized entry looks extraordinary and every ROI number is inflated.

    Searched over a grid rather than by bisection, because field strength is
    not monotone in temperature -- see ``field_strength_curve``. A target
    above what the model can reach is reported rather than silently pinned
    to the end of the range.

    Returns the calibrated ownership frame and the temperature used.
    """
    curve = field_strength_curve(
        slate, sim, cfg=cfg, n_lineups=n_lineups, seed=seed
    )
    best = curve.iloc[(curve["realized_mean"] - target_mean_score).abs().idxmin()]
    achieved = float(best["realized_mean"])

    if warn and abs(achieved - target_mean_score) > 1.5:
        reachable = curve["realized_mean"]
        logging.getLogger(__name__).warning(
            "field strength target %.1f is outside what this slate's ownership "
            "model can produce (reachable range %.1f to %.1f); using %.1f",
            target_mean_score, reachable.min(), reachable.max(), achieved,
        )

    temperature = float(best["temperature"])
    own = project_ownership(slate, sim, cfg=_with_temperature(cfg, temperature))
    return own, temperature


def _with_temperature(cfg: OwnershipConfig, temperature: float) -> OwnershipConfig:
    """Copy of the config with every utility weight divided by a temperature."""
    from dataclasses import replace

    t = max(temperature, 1e-6)
    return replace(
        cfg,
        w_value=cfg.w_value / t,
        w_points=cfg.w_points / t,
        w_ceiling=cfg.w_ceiling / t,
        w_team_total=cfg.w_team_total / t,
        w_order_top=cfg.w_order_top / t,
        w_salary=cfg.w_salary / t,
    )
