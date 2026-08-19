"""Four rounds of golf, hole by hole, for every golfer at once.

The output is one `(n_sims, n_golfers)` matrix of DraftKings points. Every
layer above this -- ownership, the opponent field, the optimizer, expected
ROI -- consumes only that matrix, so the simulator is the only place that
needs to know what a bogey is worth.

The loop is over holes and rounds, vectorized *across* simulations and
golfers. Chunking over simulations keeps the working set inside cache-ish
memory: the categorical draw needs four cumulative probabilities per golfer
per hole, which is the largest array in the program.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np

from ..config import (
    ALL_ROUNDS_UNDER_70,
    BOGEY_FREE_BONUS,
    DOUBLE_PLUS_STROKES,
    FINISH_POINTS,
    STREAK_BONUS,
    STREAK_LENGTH,
    TIE_RULE,
    UNDER_70_THRESHOLD,
    SimConfig,
)
from ..slate import Slate
from .holes import BIRDIE, BOGEY, DOUBLE_PLUS, EAGLE_PLUS, HoleModel

_CHUNK_ELEMENTS = 2e7


@dataclass
class SimResult:
    """Everything the sim produced, in simulation-major order."""

    points: np.ndarray          # (S, P) total DraftKings points
    strokes: np.ndarray         # (S, P) 72-hole total
    finish_points: np.ndarray   # (S, P) the tournament-finish component
    hole_points: np.ndarray     # (S, P) per-hole scoring component
    bonus_points: np.ndarray    # (S, P) streaks, bogey-free, all-under-70
    position: np.ndarray        # (S, P) 1-indexed finish, ties share the best
    birdies: np.ndarray         # (S, P) birdies or better over the week
    # Event counts, kept because the bonuses they drive are worth 3 and 5
    # points and appear nowhere in the calibration -- so they are the honest
    # check on whether the hole model is producing golf.
    eagles: np.ndarray | None = None
    doubles: np.ndarray | None = None
    aces: np.ndarray | None = None
    streak_rounds: np.ndarray | None = None
    bogey_free_rounds: np.ndarray | None = None
    rounds_under_70: np.ndarray | None = None
    names: tuple[str, ...] = ()

    def mean(self) -> np.ndarray:
        return self.points.mean(axis=0)

    def sd(self) -> np.ndarray:
        return self.points.std(axis=0)

    def prob_top(self, k: int) -> np.ndarray:
        return (self.position <= k).mean(axis=0)


def simulate_strokes(slate: Slate, cfg: SimConfig, model: HoleModel) -> np.ndarray:
    """72-hole totals only, skipping everything DraftKings-specific.

    Calibration only ever looks at the leaderboard, and running it without
    the scoring pass is roughly twice as fast -- which matters when the
    variance fit calls the simulator a few dozen times.
    """
    return _simulate(slate, cfg, model, scoring=False)  # type: ignore[return-value]


def simulate(slate: Slate, cfg: SimConfig, model: HoleModel) -> SimResult:
    return _simulate(slate, cfg, model, scoring=True)  # type: ignore[return-value]


def _simulate(slate: Slate, cfg: SimConfig, model: HoleModel, *, scoring: bool):
    rng = np.random.default_rng(cfg.seed)
    n_players = len(slate)
    n_holes = len(cfg.course.pars)
    par = cfg.course.par
    edge = slate.edge.astype(np.float32)
    waves = slate.waves

    chunk = max(1, int(_CHUNK_ELEMENTS // (n_players * n_holes * 4)))
    tau = model.tau.astype(np.float32)
    hole_points = model.points.astype(np.float32)
    latent_per_hole = np.float32(model.latent_per_stroke / n_holes)

    # Strokes over par by category, as integers. The "double bogey or worse"
    # bucket gets its tail drawn separately -- DraftKings stops counting at -1
    # point but the leaderboard does not stop counting strokes.
    cat_strokes = np.array([-2, -1, 0, 1, 0], dtype=np.int16)
    dbl_cum = np.cumsum([w for _, w in DOUBLE_PLUS_STROKES]).astype(np.float32)
    dbl_vals = np.array([s for s, _ in DOUBLE_PLUS_STROKES], dtype=np.int16)

    total_points = np.empty((cfg.n_sims, n_players), dtype=np.float32)
    total_strokes = np.empty((cfg.n_sims, n_players), dtype=np.int32)
    total_hole_pts = np.empty((cfg.n_sims, n_players), dtype=np.float32)
    total_bonus = np.empty((cfg.n_sims, n_players), dtype=np.float32)
    total_birdies = np.empty((cfg.n_sims, n_players), dtype=np.int16)
    total_eagles = np.empty((cfg.n_sims, n_players), dtype=np.int16)
    total_doubles = np.empty((cfg.n_sims, n_players), dtype=np.int16)
    total_aces = np.empty((cfg.n_sims, n_players), dtype=np.int16)
    total_streaks = np.empty((cfg.n_sims, n_players), dtype=np.int16)
    total_clean = np.empty((cfg.n_sims, n_players), dtype=np.int16)
    total_sub70 = np.empty((cfg.n_sims, n_players), dtype=np.int16)

    hole_idx = np.arange(n_holes)
    par_of_hole = np.asarray(cfg.course.pars)

    for lo in range(0, cfg.n_sims, chunk):
        hi = min(lo + chunk, cfg.n_sims)
        s = hi - lo

        # Persistent form for the week: one draw, shared by all four rounds.
        # This is what makes 72-hole totals spread out more than four
        # independent rounds would, and it is what the win probabilities are
        # actually sensitive to.
        week = rng.normal(0.0, cfg.week_sd, size=(s, n_players)).astype(np.float32)

        strokes = np.zeros((s, n_players), dtype=np.int32)
        hpts = np.zeros((s, n_players), dtype=np.float32)
        bonus = np.zeros((s, n_players), dtype=np.float32)
        birdies = np.zeros((s, n_players), dtype=np.int16)
        eagles = np.zeros((s, n_players), dtype=np.int16)
        doubles = np.zeros((s, n_players), dtype=np.int16)
        aces = np.zeros((s, n_players), dtype=np.int16)
        streaks = np.zeros((s, n_players), dtype=np.int16)
        clean = np.zeros((s, n_players), dtype=np.int16)
        sub70 = np.zeros((s, n_players), dtype=np.int16)
        under_70 = np.ones((s, n_players), dtype=bool)

        for rnd in range(cfg.rounds):
            noise = rng.normal(0.0, cfg.round_sd, size=(s, n_players)).astype(np.float32)
            # Rounds 1 and 2 are played in opposite waves; from round 3 the
            # field is re-paired off the leaderboard and the wave split is no
            # longer the draw split, so the shared shock stops being aligned
            # with it.
            shock = rng.normal(0.0, cfg.wave_sd, size=(s, 2)).astype(np.float32)
            if rnd < 2:
                wave_of = waves if rnd == 0 else 1 - waves
                round_wave = shock[:, wave_of]
                round_wave = round_wave + np.where(wave_of == 0, 1.0, -1.0) * (cfg.wave_edge / 2)
            else:
                round_wave = shock[:, :1]

            q = edge + week + noise + round_wave          # strokes/round, + is better
            z = (q * latent_per_hole)[:, :, None]         # latent shift per hole

            cats = model.sample(z, rng)                   # (s, P, 18) int8

            if scoring:
                hpts += hole_points[hole_idx, cats].sum(axis=-1)
                birdies += (cats <= BIRDIE).sum(axis=-1).astype(np.int16)
                eagle_holes = cats == EAGLE_PLUS
                eagles += eagle_holes.sum(axis=-1).astype(np.int16)
                aces += (eagle_holes & (par_of_hole == 3)).sum(axis=-1).astype(np.int16)
                doubles += (cats == DOUBLE_PLUS).sum(axis=-1).astype(np.int16)

            over = cat_strokes[cats]
            dbl = cats == DOUBLE_PLUS
            if dbl.any():
                u = rng.random(int(dbl.sum())).astype(np.float32)
                over[dbl] = dbl_vals[np.searchsorted(dbl_cum, u, side="right").clip(0, 2)]
            round_strokes = par + over.sum(axis=-1, dtype=np.int32)
            strokes += round_strokes

            if scoring:
                good = cats <= BIRDIE
                streak = good[..., : -(STREAK_LENGTH - 1)]
                for k in range(1, STREAK_LENGTH):
                    end = -(STREAK_LENGTH - 1 - k) or None
                    streak = streak & good[..., k:end]
                had_streak = streak.any(axis=-1)
                bogey_free = ~(cats >= BOGEY).any(axis=-1)
                bonus += STREAK_BONUS * had_streak
                bonus += BOGEY_FREE_BONUS * bogey_free
                streaks += had_streak
                clean += bogey_free
                sub70 += round_strokes < UNDER_70_THRESHOLD
            under_70 &= round_strokes < UNDER_70_THRESHOLD

        bonus += ALL_ROUNDS_UNDER_70 * under_70
        total_strokes[lo:hi] = strokes
        total_hole_pts[lo:hi] = hpts
        total_bonus[lo:hi] = bonus
        total_birdies[lo:hi] = birdies
        total_eagles[lo:hi] = eagles
        total_doubles[lo:hi] = doubles
        total_aces[lo:hi] = aces
        total_streaks[lo:hi] = streaks
        total_clean[lo:hi] = clean
        total_sub70[lo:hi] = sub70

    if not scoring:
        return total_strokes

    position, finish_pts = finish_points(total_strokes, cfg.tie_rule)
    total_points[:] = total_hole_pts + total_bonus + finish_pts

    return SimResult(
        points=total_points,
        strokes=total_strokes,
        finish_points=finish_pts,
        hole_points=total_hole_pts,
        bonus_points=total_bonus,
        position=position,
        birdies=total_birdies,
        eagles=total_eagles,
        doubles=total_doubles,
        aces=total_aces,
        streak_rounds=total_streaks,
        bogey_free_rounds=total_clean,
        rounds_under_70=total_sub70,
        names=tuple(slate.names),
    )


def finish_points(strokes: np.ndarray, tie_rule: str = TIE_RULE) -> tuple[np.ndarray, np.ndarray]:
    """Leaderboard position and finish points from 72-hole totals.

    Ties are the whole difficulty. Under "best" every golfer in a tie takes
    the points for the highest position the tie spans -- three players tied
    for third are 18 points each. Under "average" they split the band that
    the tie occupies, (18 + 16 + 14) / 3. DraftKings does not publish which
    it uses; projections.calibrate settles it against DataGolf's expected
    finish points.

    Totals are integers, so counting is done with one bincount per
    simulation-block rather than a sort.
    """
    n_sims, n_players = strokes.shape
    lo = int(strokes.min())
    width = int(strokes.max()) - lo + 1
    codes = (strokes - lo).astype(np.int64)

    flat = codes + (np.arange(n_sims, dtype=np.int64) * width)[:, None]
    counts = np.bincount(flat.ravel(), minlength=n_sims * width).reshape(n_sims, width)
    better = np.cumsum(counts, axis=1) - counts

    n_better = np.take_along_axis(better, codes, axis=1)
    n_tied = np.take_along_axis(counts, codes, axis=1)
    position = (n_better + 1).astype(np.int32)

    table = np.concatenate([np.asarray(FINISH_POINTS, dtype=np.float32),
                            np.zeros(n_players + 1, dtype=np.float32)])
    if tie_rule == "best":
        pts = table[n_better]
    elif tie_rule == "average":
        prefix = np.concatenate([[np.float32(0)], np.cumsum(table)])
        pts = (prefix[n_better + n_tied] - prefix[n_better]) / n_tied
    else:
        raise ValueError(f"unknown tie rule {tie_rule!r}")
    return position, pts.astype(np.float32)
