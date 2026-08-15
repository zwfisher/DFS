"""Start probability, unconfirmed lineups, and the guard around them.

These exist because of a real loss. Running the optimizer before batting
orders posted produced lineups full of players who did not play, and the
model had assigned every one of them a 100% chance of starting. Contest
results put a number on it: each zero-scoring player cost about 13 points,
and 60% of the top 200 entries in a 35,671-entry field carried none, against
10% of the field.
"""

from __future__ import annotations

import numpy as np
import pytest

from mlbdfs.config import MIN_CONFIRMED_LINEUP_SHARE, SALARY_RANK_TO_ORDER
from mlbdfs.data.fixtures import make_neutral_game, make_slate
from mlbdfs.pipeline import UnconfirmedLineupError, run_pipeline
from mlbdfs.projections.build import build_sim_slate
from mlbdfs.projections.lineups import (
    confirmed_share,
    estimate_start_probability,
    resolve_lineup,
)
from mlbdfs.sim.engine import simulate_slate


def _unconfirm(slate):
    """Strip batting orders, as a slate looks before lineups post."""
    for p in slate.players:
        if not p.is_pitcher:
            p.batting_order = None
            p.confirmed = False
    return slate


def test_confirmed_lineups_are_certain_starters():
    slate, _ = make_slate(n_games=2, seed=1)
    assert confirmed_share(slate) == 1.0
    lineup = resolve_lineup(slate, slate.teams[0])
    assert all(p.start_probability == 1.0 for p in lineup)


def test_guessed_starters_carry_start_risk():
    slate = _unconfirm(make_slate(n_games=2, seed=1)[0])
    lineup = resolve_lineup(slate, slate.teams[0])

    assert len(lineup) == 9
    assert all(p.start_probability < 1.0 for p in lineup)
    # More expensive players are likelier to be in the lineup.
    by_salary = sorted(lineup, key=lambda p: -p.salary)
    assert by_salary[0].start_probability > by_salary[-1].start_probability


def test_start_probability_declines_with_salary_rank():
    probs = [estimate_start_probability(r) for r in range(12)]
    assert probs == sorted(probs, reverse=True)
    assert probs[0] < 1.0  # even the priciest bat is not guaranteed
    assert probs[-1] < 0.5  # a fringe player is close to a coin flip


def test_guessed_order_is_not_sorted_by_salary():
    """Leadoff hitters are often cheap; the expensive bats hit 2 through 4.

    Assigning the batting order by descending salary misallocates plate
    appearances at both ends of the lineup.
    """
    slate = _unconfirm(make_slate(n_games=2, seed=1)[0])
    lineup = resolve_lineup(slate, slate.teams[0])

    by_salary = sorted(lineup, key=lambda p: -p.salary)
    orders = [p.batting_order for p in by_salary]
    assert orders != sorted(orders), "order should not simply track salary"
    assert orders == list(SALARY_RANK_TO_ORDER)
    # The most expensive hitter should not be leading off.
    assert by_salary[0].batting_order != 1


def test_unconfirmed_lineups_lower_projections_and_add_zero_risk():
    """The behaviour that was missing, stated directly."""
    confirmed_slate, book = make_slate(n_games=3, seed=4)
    confirmed = simulate_slate(
        build_sim_slate(confirmed_slate, book), n_sims=4000, seed=2
    )

    guessed_slate, book2 = make_slate(n_games=3, seed=4)
    guessed = simulate_slate(
        build_sim_slate(_unconfirm(guessed_slate), book2), n_sims=4000, seed=2
    )

    def hitter_stats(slate, sim):
        means, zeros = [], []
        for p in slate.players:
            if p.is_pitcher:
                continue
            col = sim.for_player(p.player_id)
            if col.mean() > 0:
                means.append(col.mean())
                zeros.append((col == 0).mean())
        return float(np.mean(means)), float(np.mean(zeros))

    conf_mean, conf_zero = hitter_stats(confirmed_slate, confirmed)
    guess_mean, guess_zero = hitter_stats(guessed_slate, guessed)

    assert guess_mean < conf_mean, "guessed lineups must project lower"
    assert guess_zero > conf_zero + 0.05, "guessed lineups must carry zero risk"


def test_confirmed_hitter_zero_rate_is_realistic():
    """A league-average starter posts an empty line about a fifth of the time.

    If this drifts, the simulator is mispricing the most common bad outcome
    in the game, and every downside estimate above it is wrong.
    """
    slate, book = make_neutral_game()
    sim = simulate_slate(build_sim_slate(slate, book), n_sims=8000, seed=3)
    rates = [
        (sim.for_player(p.player_id) == 0).mean()
        for p in slate.players
        if not p.is_pitcher
    ]
    assert 0.16 < float(np.mean(rates)) < 0.27


def test_pipeline_refuses_an_unconfirmed_slate():
    slate, book = make_slate(n_games=2, seed=6)
    _unconfirm(slate)
    assert confirmed_share(slate) < MIN_CONFIRMED_LINEUP_SHARE

    with pytest.raises(UnconfirmedLineupError, match="posted batting order"):
        run_pipeline(slate, book, n_sims=200, n_field=100, n_candidates=2,
                     n_lineups=1, verbose=False)


def test_pipeline_runs_unconfirmed_when_explicitly_allowed():
    slate, book = make_slate(n_games=2, seed=6)
    _unconfirm(slate)
    result = run_pipeline(
        slate, book, n_sims=400, n_field=200, n_candidates=3, n_lineups=2,
        verbose=False, allow_unconfirmed=True,
    )
    assert len(result.pool) >= 1


def test_pipeline_accepts_a_confirmed_slate():
    slate, book = make_slate(n_games=2, seed=6)
    result = run_pipeline(
        slate, book, n_sims=400, n_field=200, n_candidates=3, n_lineups=2,
        verbose=False,
    )
    assert len(result.pool) >= 1
