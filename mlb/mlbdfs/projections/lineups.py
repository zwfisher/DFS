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

from ..config import (
    LEAGUE_RUNS_PER_GAME,
    PA_BY_ORDER,
    SALARY_RANK_TO_ORDER,
    START_PROBABILITY_BY_SALARY_RANK,
    START_PROBABILITY_FRINGE,
)
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


def estimate_start_probability(salary_rank: int) -> float:
    """Chance a hitter starts, given his salary rank within his own team.

    Used only before a lineup is posted. Teams carry about thirteen position
    players and start nine, so even the most expensive bat is not certain,
    and a fringe player is closer to a coin flip against himself.
    """
    if salary_rank < len(START_PROBABILITY_BY_SALARY_RANK):
        return START_PROBABILITY_BY_SALARY_RANK[salary_rank]
    return START_PROBABILITY_FRINGE


def resolve_lineup(slate: Slate, team: str, fill_missing: bool = True) -> list[Player]:
    """Return nine hitters in batting order for a team.

    Hitters with a posted batting order are used as given and marked as
    certain to start. Any remaining slots are guessed from salary.

    That guess is genuinely poor and the code should not pretend otherwise.
    Contest results show why: a hitter who does not start scores zero, and
    zeros dominate finishing position -- 60% of the top 200 entries in a
    35,671-entry field carried no zero-scoring player, against 10% of the
    field overall, with each additional zero worth about 13 points. Guessing
    a lineup therefore risks the most expensive mistake available.

    Two things follow, and both are done here rather than left implicit:

    * Guessed starters get a ``start_probability`` below one, so the
      simulator prices the chance they do not play instead of assuming they
      do.
    * Salary rank maps onto batting slots through
      ``SALARY_RANK_TO_ORDER`` rather than descending salary. Leadoff
      hitters are frequently cheap contact-and-speed players while the
      expensive bats hit second through fourth, so sorting the order by
      price misallocates plate appearances at both ends.

    The real remedy is to run after lineups post; ``pipeline.run_pipeline``
    refuses to proceed on a mostly unconfirmed slate unless overridden.
    """
    lineup = slate.lineup_for(team)
    for p in lineup:
        p.start_probability = 1.0

    if len(lineup) > 9:
        # More than nine hitters carry a batting order, which means two
        # players were assigned the same slot -- most often a multi-position
        # player who appeared on the slate twice. Keep one per slot rather
        # than handing the simulator a ten-man lineup, which it cannot
        # represent and which fails deep in a numpy stack with no useful
        # message.
        by_slot: dict[int, Player] = {}
        for p in sorted(lineup, key=lambda p: (-p.salary, p.player_id)):
            by_slot.setdefault(p.batting_order, p)
        lineup = sorted(by_slot.values(), key=lambda p: p.batting_order)
        for p in slate.players:
            if p.team == team and not p.is_pitcher and p not in lineup:
                p.batting_order = None

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

    open_slots = [slot for slot in SALARY_RANK_TO_ORDER if slot not in taken]
    filled = list(lineup)
    for rank, slot in enumerate(open_slots):
        if not bench:
            raise ValueError(f"{team} has too few hitters to fill a lineup")
        pick = bench.pop(0)
        pick.batting_order = slot
        pick.start_probability = estimate_start_probability(rank)
        filled.append(pick)

    return sorted(filled, key=lambda p: p.batting_order)


def confirmed_share(slate: Slate) -> float:
    """Fraction of the slate's hitters that have a posted batting order."""
    hitters = [p for p in slate.players if not p.is_pitcher]
    if not hitters:
        return 1.0
    return sum(1 for p in hitters if p.confirmed) / len(hitters)


def order_weights(lineup: list[Player], implied_runs: float) -> np.ndarray:
    """Expected PA per hitter, aligned to the given lineup order."""
    pa = expected_pa(implied_runs)
    return np.array([pa[p.batting_order - 1] for p in lineup], dtype=np.float64)
