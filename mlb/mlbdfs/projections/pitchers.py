"""Starting pitcher workload and win modelling.

Innings pitched are worth 2.25 points each on DraftKings, which makes "how
long does he stay in" comparable in importance to "how well does he pitch".
The hook is modelled per simulation as a pitch-count ceiling drawn before
first pitch, plus a blow-up rule for the starts where the manager stops
caring about the pitch count.

Wins are worth 4 points and are close to a coin flip conditional on the team
winning, so they are modelled explicitly rather than folded into a mean.
"""

from __future__ import annotations

import numpy as np

from ..config import SIM, SimConfig


def draw_pitch_limits(
    n_sims: int,
    rng: np.random.Generator,
    mean: float | None = None,
    sd: float | None = None,
    workload_scale: float = 1.0,
) -> np.ndarray:
    """Per-simulation pitch ceilings for one starter.

    ``workload_scale`` lets a specific pitcher be leashed shorter or longer
    than league default -- an opener sits near 0.4, a workhorse near 1.1.
    """
    mu = (SIM.hook_pitch_limit_mean if mean is None else mean) * workload_scale
    sigma = SIM.hook_pitch_limit_sd if sd is None else sd
    limits = rng.normal(mu, sigma, size=n_sims)
    return np.clip(limits, 35.0, 125.0)


def estimate_pitches(
    outcomes_faced: np.ndarray, cfg: SimConfig = SIM
) -> np.ndarray:
    """Approximate pitch count from batters faced."""
    return outcomes_faced * cfg.pitches_per_pa


def win_qualifies(
    outs_recorded: np.ndarray,
    leading_at_exit: np.ndarray,
    team_won: np.ndarray,
    cfg: SimConfig = SIM,
) -> np.ndarray:
    """Whether a starter is credited with the win.

    The official rule is more intricate -- the scorer can award the win
    elsewhere, and a reliever can vulture it -- but requiring five innings, a
    lead when handing the ball over, and a team win captures the large
    majority of cases.
    """
    min_outs = int(round(cfg.win_min_innings * 3))
    return (outs_recorded >= min_outs) & leading_at_exit & team_won


def workload_scale_from_history(
    avg_pitches: float | None, league_avg: float = 88.0
) -> float:
    """Turn a starter's recent average pitch count into a workload scale."""
    if avg_pitches is None or avg_pitches <= 0:
        return 1.0
    return float(np.clip(avg_pitches / league_avg, 0.4, 1.15))
