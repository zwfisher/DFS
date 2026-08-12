"""Uncertainty around the ownership projection.

This is where Monte Carlo genuinely belongs in the ownership problem. The
point estimate is a regression, but being wrong about ownership is a
first-order tournament risk: a leverage play that arrives at thirty percent
owned is not a leverage play, and a lineup built on that assumption is worse
than one built on no assumption at all.

Drawing from a Dirichlet rather than perturbing each player independently
buys two properties that matter:

* The sum constraint survives. Total ownership in a position group stays
  equal to the number of roster slots, so no draw produces an incoherent
  slate.
* The errors are negatively correlated in the right way. If the chalk stack
  comes in under expectation, that ownership went somewhere else -- it did
  not evaporate.

The concentration parameter is the one thing here that wants fitting
against real data; until then it is set from a plausible prior on how far
ownership projections typically miss.
"""

from __future__ import annotations

import numpy as np
import pandas as pd

from ..config import OWNERSHIP, ROSTER, OwnershipConfig


def sample_ownership(
    ownership: pd.DataFrame,
    n_draws: int,
    rng: np.random.Generator,
    cfg: OwnershipConfig = OWNERSHIP,
) -> np.ndarray:
    """Draw ``n_draws`` plausible ownership vectors.

    Returns an ``(n_draws, n_players)`` array aligned to the row order of
    ``ownership``. Each position group in each draw sums to its slot count.
    """
    n_players = len(ownership)
    draws = np.zeros((n_draws, n_players), dtype=np.float32)
    slots = dict(ROSTER.slots)

    for position, idx in ownership.groupby("position", sort=False).groups.items():
        rows = ownership.index.get_indexer(idx)
        p = ownership["ownership"].to_numpy()[rows]
        n_slots = float(slots.get(position, 1))

        # Dirichlet over shares within the group, rescaled to the slot count.
        shares = p / p.sum()
        alpha = np.maximum(cfg.dirichlet_concentration * shares, 1e-3)
        draws[:, rows] = (rng.dirichlet(alpha, size=n_draws) * n_slots).astype(np.float32)

    return np.clip(draws, 1e-4, 1.0)


def ownership_interval(
    ownership: pd.DataFrame,
    n_draws: int = 2000,
    seed: int = 0,
    cfg: OwnershipConfig = OWNERSHIP,
) -> pd.DataFrame:
    """Attach a credible interval to each player's projected ownership."""
    rng = np.random.default_rng(seed)
    draws = sample_ownership(ownership, n_draws, rng, cfg)
    q = np.quantile(draws, [0.10, 0.50, 0.90], axis=0)

    out = ownership.copy()
    out["own_p10"] = q[0]
    out["own_p50"] = q[1]
    out["own_p90"] = q[2]
    return out


def fit_concentration(
    projected: np.ndarray, realized: np.ndarray, n_slots: float
) -> float:
    """Estimate the Dirichlet concentration from historical misses.

    Once ``logger.py`` has real contest ownership, call this with paired
    projected and realized vectors from past slates. Method of moments on
    the Dirichlet: the concentration that reproduces the observed variance
    of the realized shares around the projection.
    """
    p = np.asarray(projected, dtype=np.float64) / n_slots
    r = np.asarray(realized, dtype=np.float64) / n_slots
    p = p / p.sum()
    r = r / r.sum()

    var = np.mean((r - p) ** 2)
    expected_unit_var = np.mean(p * (1.0 - p))
    if var <= 0 or expected_unit_var <= 0:
        return float(OWNERSHIP.dirichlet_concentration)
    return float(max(expected_unit_var / var - 1.0, 1.0))
