"""Matchup adjustment: log5, park, and bullpen blending.

The log5 (odds-ratio) method is the standard way to combine a batter's rate
with a pitcher's rate against a league baseline. For a single binary outcome
it is

    p = (b * q / l) / ((b * q / l) + (1 - b)(1 - q)/(1 - l))

Extended to a multinomial it becomes proportional reweighting followed by
renormalization, which is what ``log5`` implements: the batter's odds of each
outcome are scaled by how much the pitcher inflates or suppresses that
outcome relative to league, then the vector is pushed back onto the simplex.
"""

from __future__ import annotations

import numpy as np

from ..config import OUTCOMES, park_factor
from .rates import league_rate_vector


def log5(batter: np.ndarray, pitcher: np.ndarray, league: np.ndarray | None = None) -> np.ndarray:
    """Combine batter and pitcher rate vectors into matchup rates.

    ``batter`` is ``(n_batters, n_outcomes)``, ``pitcher`` is either the same
    shape or a single ``(n_outcomes,)`` vector broadcast across batters.
    Every returned row sums to 1.
    """
    lg = league_rate_vector() if league is None else np.asarray(league, dtype=np.float64)
    b = np.atleast_2d(np.asarray(batter, dtype=np.float64))
    p = np.atleast_2d(np.asarray(pitcher, dtype=np.float64))

    combined = b * p / np.maximum(lg, 1e-12)
    total = combined.sum(axis=1, keepdims=True)
    return combined / np.maximum(total, 1e-12)


def apply_park(rates: np.ndarray, team: str) -> np.ndarray:
    """Scale outcome rates by the home park's factors and renormalize.

    Only the hit types carry park factors. Renormalizing after scaling means
    a hitter-friendly park converts outs into hits rather than manufacturing
    extra plate appearances.
    """
    factors = park_factor(team)
    if not factors:
        return rates

    mult = np.ones(len(OUTCOMES), dtype=np.float64)
    for name, value in factors.items():
        if name in OUTCOMES:
            mult[OUTCOMES.index(name)] = value

    scaled = np.asarray(rates, dtype=np.float64) * mult[None, :]
    return scaled / scaled.sum(axis=1, keepdims=True)


def blend_bullpen(
    starter_rates: np.ndarray,
    bullpen_rates: np.ndarray,
    starter_weight: float,
) -> np.ndarray:
    """Weighted blend of starter and bullpen rate vectors.

    Used for the non-simulated projection path. The simulator does not need
    this -- it switches pitchers mid-game and gets the mix for free from the
    hook model.
    """
    w = float(np.clip(starter_weight, 0.0, 1.0))
    blended = w * np.asarray(starter_rates) + (1.0 - w) * np.asarray(bullpen_rates)
    return blended / blended.sum(axis=-1, keepdims=True)


def build_matchup_rates(
    batter_rates: np.ndarray,
    pitcher_rates: np.ndarray,
    park_team: str,
) -> np.ndarray:
    """Full batter-vs-pitcher rate construction for one lineup.

    Order matters: log5 first (a talent-on-talent question), park second (an
    environment question applied to the resulting contact).
    """
    matchup = log5(batter_rates, pitcher_rates)
    return apply_park(matchup, park_team)


def scale_to_team_total(rates: np.ndarray, implied_runs: float, league_runs: float) -> np.ndarray:
    """Nudge a lineup's rates so its run environment tracks the Vegas total.

    Vegas implied totals carry information the rate model does not -- weather,
    umpire, bullpen availability, late scratches. Rather than trusting them
    wholesale, on-base and power outcomes are scaled by a damped ratio of
    implied to league runs, which moves the lineup partway toward the market
    without overriding the underlying talent estimates.
    """
    if implied_runs <= 0 or league_runs <= 0:
        return rates

    # Damped: a 20% higher implied total moves offensive rates about 7%.
    ratio = (implied_runs / league_runs) ** 0.35

    mult = np.ones(len(OUTCOMES), dtype=np.float64)
    for name in ("single", "double", "triple", "home_run", "walk"):
        mult[OUTCOMES.index(name)] = ratio

    scaled = np.asarray(rates, dtype=np.float64) * mult[None, :]
    return scaled / scaled.sum(axis=1, keepdims=True)
