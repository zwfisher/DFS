"""Batting order handling and expected plate appearances.

Plate appearances are the most under-appreciated lever in MLB DFS: the
leadoff hitter gets roughly 4.7 per game and the nine hole roughly 4.0, which
is close to a 20% difference in raw opportunity before any talent enters the
picture.

The simulator does not consume the expected-PA numbers here -- PA counts
emerge from the game state, which is more honest than imposing them. These
functions exist to sanity-check that the simulator's emergent PA
distribution matches reality, and to serve the non-simulated projection path.
"""

from __future__ import annotations

import numpy as np

from ..config import LEAGUE_RUNS_PER_GAME, PA_BY_ORDER
from ..slate import Player, Slate


def expected_pa(implied_runs: float = LEAGUE_RUNS_PER_GAME) -> np.ndarray:
    """Expected plate appearances for batting slots 1-9.

    A team that scores more bats more, roughly one extra plate appearance per
    lineup for each additional run of implied total, spread across the order.
    """
    base = np.array(PA_BY_ORDER, dtype=np.float64)
    if implied_runs <= 0:
        return base
    # Each extra run adds about 0.11 PA to every slot, damped at the extremes.
    delta = 0.11 * (implied_runs - LEAGUE_RUNS_PER_GAME)
    return base + delta


def pa_uncertainty(confirmed: bool) -> float:
    """Standard deviation to attach to expected PA.

    An unconfirmed lineup carries slot risk on top of the usual game-length
    variance -- a projected leadoff man who hits sixth loses most of the
    edge the projection gave him.
    """
    return 0.55 if confirmed else 0.95


def resolve_lineup(slate: Slate, team: str, fill_missing: bool = True) -> list[Player]:
    """Return nine hitters in batting order for a team.

    When a lineup is not posted, players carrying a projected order are used
    as-is and any remaining slots are filled by salary, which is a crude but
    surprisingly effective proxy for who a manager writes into the middle of
    the order.
    """
    lineup = slate.lineup_for(team)
    if len(lineup) == 9:
        return lineup
    if not fill_missing:
        raise ValueError(f"{team} has {len(lineup)} hitters with a batting order, need 9")

    taken = {p.batting_order for p in lineup}
    bench = sorted(
        (
            p
            for p in slate.players
            if p.team == team and not p.is_pitcher and p.batting_order is None
        ),
        key=lambda p: -p.salary,
    )

    filled = list(lineup)
    for slot in range(1, 10):
        if slot in taken:
            continue
        if not bench:
            raise ValueError(f"{team} has too few hitters to fill a lineup")
        pick = bench.pop(0)
        pick.batting_order = slot
        filled.append(pick)

    return sorted(filled, key=lambda p: p.batting_order)


def order_weights(lineup: list[Player], implied_runs: float) -> np.ndarray:
    """Expected PA per hitter, aligned to the given lineup order."""
    pa = expected_pa(implied_runs)
    return np.array([pa[p.batting_order - 1] for p in lineup], dtype=np.float64)
