"""Projected ownership, and the uncertainty around it.

Ownership is the input that decides whether a lineup is worth entering, and
it is also the one nobody can measure in advance. DataGolf publishes a
projection for every golfer on the slate, which saves modelling the level
from scratch -- but a point estimate is not enough, because a tournament is
decided by which lineups the field actually built. So the projection is
treated as the centre of a distribution, not as the answer.

The constraint that matters is that ownership has to add up: across a
six-golfer slate, total ownership is exactly 600%. Perturbing each golfer
independently breaks that and quietly invents lineups the contest could
never contain. A Dirichlet draw preserves it, and it also gets the sign of
the correlation right -- if the chalk comes in lower than projected, that
ownership went to somebody else.
"""

from __future__ import annotations

import numpy as np

from ..config import ROSTER_SIZE, OwnershipConfig
from ..slate import Slate


def projected_ownership(slate: Slate, cfg: OwnershipConfig, points: np.ndarray | None = None) -> np.ndarray:
    """Ownership as a fraction of entries, one per golfer, summing to 6.

    Where DataGolf has published a number it is used directly. Where it has
    not -- a late addition to the field, or a slate the page does not cover
    -- a conditional logit on projected points and salary fills the gap, and
    the whole vector is renormalised so the roster arithmetic still holds.
    """
    own = slate.ownership.astype(float) / 100.0
    missing = own <= 0
    if missing.any():
        if points is None:
            raise ValueError("need projected points to fill missing ownership")
        util = cfg.points_coef * points + cfg.salary_coef * (slate.salaries / 1000.0)
        weights = np.exp(util - util.max())
        weights /= weights.sum()
        # Give the unprojected golfers the share of the roster spots that the
        # published projections leave unaccounted for.
        remaining = max(ROSTER_SIZE - own[~missing].sum(), 0.0)
        own[missing] = remaining * weights[missing] / weights[missing].sum()
    total = own.sum()
    if total <= 0:
        return np.full(len(slate), ROSTER_SIZE / len(slate))
    return own * (ROSTER_SIZE / total)


def enforce_salary_feasibility(
    own: np.ndarray, salaries: np.ndarray, target_spend: float, *, max_tilt: float = 4.0
) -> tuple[np.ndarray, dict]:
    """Tilt ownership until a field with these marginals could exist.

    Ownership projections are made one golfer at a time, and nothing forces
    them to describe a set of *lineups*. They have to. Six golfers with the
    projected ownerships have an expected combined salary, and that number
    has to be inside the cap -- if it is not, no field of legal entries has
    those marginals, and every downstream consumer of them is modelling
    something that cannot happen.

    DataGolf's BMW projections imply a mean spend of $50,512 against a
    $50,000 cap, so they need about a percent taken out of the expensive end.
    The correction is the smallest one that works: a single exponential tilt
    in salary, which is the Kullback-Leibler projection of the original
    vector onto the constraint and therefore changes the ordering not at all
    and the relative odds as little as possible.
    """
    total = own.sum()
    scaled = salaries / 1000.0

    def spend_at(lam: float) -> tuple[np.ndarray, float]:
        tilted = own * np.exp(-lam * (scaled - scaled.mean()))
        tilted *= total / tilted.sum()
        return tilted, float((tilted * salaries).sum())

    _, spend = spend_at(0.0)
    if spend <= target_spend:
        return own, {"tilt": 0.0, "spend_before": spend, "spend_after": spend}

    lo, hi = 0.0, 0.05
    while spend_at(hi)[1] > target_spend and hi < max_tilt:
        hi *= 2
    for _ in range(60):
        mid = 0.5 * (lo + hi)
        if spend_at(mid)[1] > target_spend:
            lo = mid
        else:
            hi = mid
    tilted, after = spend_at(hi)
    return tilted, {"tilt": hi, "spend_before": spend, "spend_after": after}


def sample_ownership(
    own: np.ndarray, cfg: OwnershipConfig, rng: np.random.Generator, n: int = 1
) -> np.ndarray:
    """`n` draws of the ownership vector, each still summing to 6."""
    alpha = np.clip(own / ROSTER_SIZE, 1e-6, None) * cfg.concentration
    return rng.dirichlet(alpha, size=n) * ROSTER_SIZE
