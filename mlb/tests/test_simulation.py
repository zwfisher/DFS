"""Simulator behaviour: league realism, correlation and DraftKings rules.

The realism assertions are deliberately loose. They are not trying to pin
exact values, they are trying to catch the class of change that silently
breaks the run environment -- a rate vector that stops summing to one, an
advancement rule inverted, a hook model that leaves starters in all night.
"""

from __future__ import annotations

import numpy as np
import pytest

from mlbdfs.config import SIM
from mlbdfs.data.fixtures import make_neutral_game, make_slate
from mlbdfs.projections.build import build_sim_slate, validate_rates
from mlbdfs.projections.matchup import apply_park, log5
from mlbdfs.projections.rates import league_rate_vector
from mlbdfs.sim.engine import simulate_slate, stack_distribution


@pytest.fixture(scope="module")
def neutral():
    slate, book = make_neutral_game()
    return simulate_slate(build_sim_slate(slate, book), n_sims=6000, seed=17)


@pytest.fixture(scope="module")
def synthetic():
    slate, book = make_slate(n_games=6, seed=5)
    sim = simulate_slate(build_sim_slate(slate, book), n_sims=5000, seed=11)
    return slate, sim


def test_league_rates_sum_to_one():
    """Everything downstream renormalizes, so a vector that does not sum to
    one silently deflates every non-out outcome."""
    assert league_rate_vector().sum() == pytest.approx(1.0)


def test_log5_and_park_preserve_the_simplex():
    lg = league_rate_vector()
    batter = np.tile(lg, (5, 1)) * np.array([1.3, 1.0, 0.8, 1.1, 0.9])[:, None]
    batter = batter / batter.sum(axis=1, keepdims=True)

    matchup = log5(batter, lg * 1.1)
    validate_rates(matchup)
    validate_rates(apply_park(matchup, "COL"))


def test_log5_against_league_average_is_a_noop():
    """A league-average pitcher must leave a batter's rates alone."""
    lg = league_rate_vector()
    batter = lg * np.array([1.4, 1.0, 0.7, 1.0, 1.2, 1.1, 1.0, 1.5])
    batter = batter / batter.sum()
    assert log5(batter, lg)[0] == pytest.approx(batter, abs=1e-9)


def test_run_environment_matches_the_league(neutral):
    runs = np.stack(list(neutral.team_runs.values())).ravel()
    assert 4.0 < runs.mean() < 4.6
    assert 2.7 < runs.std() < 3.4
    assert 0.05 < (runs == 0).mean() < 0.10
    assert 0.03 < (runs >= 10).mean() < 0.08


def test_starters_pitch_a_realistic_number_of_innings(neutral):
    outs = np.stack(list(neutral.starter_outs.values()))
    assert 4.7 < outs.mean() / 3 < 5.8
    assert 0.25 < (outs >= 18).mean() < 0.55  # six or more innings
    assert (outs <= 27).all()


def test_plate_appearances_follow_the_batting_order(synthetic):
    slate, sim = synthetic
    by_slot = {}
    for p in slate.players:
        if p.batting_order and p.player_id in sim.player_pa:
            by_slot.setdefault(p.batting_order, []).append(
                sim.player_pa[p.player_id].mean()
            )
    means = {k: float(np.mean(v)) for k, v in by_slot.items()}

    assert 4.5 < means[1] < 4.9
    assert 3.7 < means[9] < 4.1
    # Monotone decreasing down the order.
    ordered = [means[i] for i in range(1, 10)]
    assert all(a >= b - 0.02 for a, b in zip(ordered, ordered[1:]))


def test_teammates_are_positively_correlated(synthetic):
    """The property stacking depends on. Modest in size, but it must be
    there and it must be positive."""
    slate, sim = synthetic
    team = slate.teams[0]
    lineup = [p.player_id for p in slate.lineup_for(team)][:5]

    corr = sim.correlation(lineup)
    off_diagonal = corr[np.triu_indices(len(lineup), k=1)]
    assert off_diagonal.mean() > 0.05
    assert (off_diagonal > 0).all()


def test_hitters_are_anticorrelated_with_the_opposing_starter(synthetic):
    slate, sim = synthetic
    hitter = slate.lineup_for(slate.teams[0])[0]
    starter = slate.starter_for(hitter.opponent)
    corr = sim.correlation([hitter.player_id, starter.player_id])[0, 1]
    assert corr < -0.10


def test_a_stack_has_a_fatter_tail_than_the_same_players_decorrelated(synthetic):
    """The whole economic argument for stacking, stated as a test."""
    slate, sim = synthetic
    lineup = [p.player_id for p in slate.lineup_for(slate.teams[0])][:5]
    dist = stack_distribution(sim, lineup)

    assert dist["sd"] > dist["independent_sd"]
    assert dist["p99"] > dist["independent_p99"]


def test_scores_are_finite_and_hitters_never_go_negative(synthetic):
    slate, sim = synthetic
    assert np.isfinite(sim.scores).all()
    for p in slate.players:
        col = sim.for_player(p.player_id)
        if not p.is_pitcher:
            assert col.min() >= 0, f"{p.name} scored negative"


def test_simulation_is_reproducible():
    slate, book = make_slate(n_games=3, seed=2)
    sim_slate = build_sim_slate(slate, book)
    a = simulate_slate(sim_slate, n_sims=800, seed=99)
    b = simulate_slate(sim_slate, n_sims=800, seed=99)
    assert np.array_equal(a.scores, b.scores)


def test_extra_innings_are_bounded(neutral):
    """The extras cap must hold, or a pathological rate vector could hang."""
    runs = np.stack(list(neutral.team_runs.values()))
    assert runs.max() < 40
