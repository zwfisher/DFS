"""Ownership, field generation, DraftKings rule compliance and ROI."""

from __future__ import annotations

import numpy as np
import pandas as pd
import pytest

from mlbdfs.config import OWNERSHIP, ROSTER
from mlbdfs.data.fixtures import make_slate
from mlbdfs.optimize.contest import double_up, large_gpp, single_entry_gpp
from mlbdfs.optimize.milp import LineupOptimizer
from mlbdfs.optimize.portfolio import (
    _rank_against_field,
    evaluate_lineups,
    select_portfolio,
)
from mlbdfs.ownership.field import _FieldContext, generate_field
from mlbdfs.ownership.heuristic import implied_field_mean, project_ownership
from mlbdfs.ownership.uncertainty import sample_ownership
from mlbdfs.projections.build import build_sim_slate
from mlbdfs.sim.engine import simulate_slate


@pytest.fixture(scope="module")
def slate_sim():
    slate, book = make_slate(n_games=6, seed=3)
    sim = simulate_slate(build_sim_slate(slate, book), n_sims=3000, seed=7)
    return slate, sim


@pytest.fixture(scope="module")
def ownership(slate_sim):
    slate, sim = slate_sim
    return project_ownership(slate, sim)


@pytest.fixture(scope="module")
def pool(slate_sim):
    slate, sim = slate_sim
    optimizer = LineupOptimizer(slate)
    return optimizer.generate_pool(sim.scores, sim.player_ids, n_candidates=25, seed=4)


# --------------------------------------------------------------------------
# Ownership
# --------------------------------------------------------------------------


def test_ownership_sums_to_the_roster_slots(ownership):
    """The property a per-player regression cannot give you."""
    slots = dict(ROSTER.slots)
    totals = ownership.groupby("position")["ownership"].sum()
    for position, total in totals.items():
        assert total == pytest.approx(slots[position], abs=0.05), position


def test_ownership_is_bounded(ownership):
    assert ownership["ownership"].between(0, 0.66).all()


def test_ownership_prefers_value(ownership):
    hitters = ownership[~ownership["is_pitcher"]]
    assert hitters["ownership"].corr(hitters["value"]) > 0.3


def test_ownership_draws_preserve_the_sum(ownership):
    rng = np.random.default_rng(0)
    draws = sample_ownership(ownership, 200, rng)
    slots = dict(ROSTER.slots)
    for position, idx in ownership.groupby("position").groups.items():
        rows = ownership.index.get_indexer(idx)
        totals = draws[:, rows].sum(axis=1)
        assert np.allclose(totals, slots[position], atol=1e-3), position


def test_ownership_draws_are_negatively_correlated(ownership):
    """If the chalk comes in low, that ownership went somewhere else."""
    rng = np.random.default_rng(1)
    draws = sample_ownership(ownership, 1500, rng)
    of = ownership.index[ownership["position"] == "OF"][:6]
    rows = ownership.index.get_indexer(of)
    corr = np.corrcoef(draws[:, rows], rowvar=False)
    assert corr[np.triu_indices(len(rows), k=1)].mean() < 0


# --------------------------------------------------------------------------
# Field
# --------------------------------------------------------------------------


def test_field_lineups_obey_draftkings_rules(slate_sim, ownership):
    slate, _ = slate_sim
    field = generate_field(slate, ownership, n_lineups=1500, seed=2)
    ctx = _FieldContext(slate, ownership, OWNERSHIP)

    assert field.size > 1000
    assert (field.salaries <= ROSTER.salary_cap).all()

    for lineup in field.lineups[:400]:
        assert len(set(lineup)) == ROSTER.size
        teams = ctx.player_team[lineup]
        hitters = ~ctx.is_pitcher[lineup]
        counts = np.bincount(teams[hitters])
        assert counts.max() <= ROSTER.max_hitters_per_team
        assert ctx.is_pitcher[lineup].sum() == 2


def test_field_spends_most_of_the_cap(slate_sim, ownership):
    slate, _ = slate_sim
    field = generate_field(slate, ownership, n_lineups=1500, seed=2)
    assert field.salaries.mean() > ROSTER.salary_cap * 0.96


def test_field_marginals_track_the_projection(slate_sim, ownership):
    slate, _ = slate_sim
    field = generate_field(slate, ownership, n_lineups=4000, seed=2)
    error = np.abs(field.realized_ownership - ownership["ownership"].to_numpy())
    assert error.mean() < 0.05


def test_field_scoring_remaps_player_indices(slate_sim, ownership):
    """Field indices address the ownership frame, not the score matrix.

    Indexing the score matrix with raw field indices produces a plausible
    looking answer computed from entirely the wrong players, so the remap is
    pinned here.
    """
    slate, sim = slate_sim
    field = generate_field(slate, ownership, n_lineups=300, seed=2)

    assert list(field.player_ids) != list(sim.player_ids)

    scored = field.score(sim.scores, sim.player_ids, slice(0, 50))
    cols = field.score_columns(sim.player_ids)
    expected = np.array(
        [
            [sum(sim.scores[s, cols[p]] for p in lineup) for lineup in field.lineups]
            for s in range(3)
        ]
    )
    assert np.allclose(scored[:3], expected, atol=1e-3)


def test_implied_field_mean_tracks_the_realized_field(slate_sim, ownership):
    """The ownership-weighted identity should land near the field the
    generator actually builds.

    Not a bound in either direction: ownership projections need not be
    jointly affordable, and the calibration residual that reconciles the
    two leaves error on both sides. What matters is that they do not
    diverge, since a large gap means ROI is being measured against a field
    quite different from the one the ownership model describes.
    """
    slate, _ = slate_sim
    field = generate_field(slate, ownership, n_lineups=2000, seed=2)
    realized = ownership["proj"].to_numpy()[field.lineups].sum(axis=1).mean()
    assert abs(realized - implied_field_mean(ownership)) < 6.0


# --------------------------------------------------------------------------
# Optimizer
# --------------------------------------------------------------------------


def test_optimized_lineups_obey_draftkings_rules(slate_sim, pool):
    slate, _ = slate_sim
    assert pool

    for lineup in pool:
        assert lineup.salary <= ROSTER.salary_cap
        assert len(set(lineup.player_ids)) == ROSTER.size

        players = [slate.player(p) for p in lineup.player_ids]
        pitchers = [p for p in players if p.is_pitcher]
        assert len(pitchers) == 2

        hitters = [p for p in players if not p.is_pitcher]
        counts = pd.Series([h.team for h in hitters]).value_counts()
        assert counts.max() <= ROSTER.max_hitters_per_team

        games = {slate.game_for(p.team).game_id for p in players}
        assert len(games) >= ROSTER.min_games_represented

        for hitter in hitters:
            for pitcher in pitchers:
                assert hitter.opponent != pitcher.team


def test_positions_can_be_filled(slate_sim, pool):
    """A lineup must admit a valid assignment to the roster slots."""
    slate, _ = slate_sim
    from scipy.optimize import linear_sum_assignment

    slots = [pos for pos, n in ROSTER.slots for _ in range(n)]
    for lineup in pool[:10]:
        players = [slate.player(p) for p in lineup.player_ids]
        cost = np.ones((len(players), len(slots)))
        for i, p in enumerate(players):
            for j, slot in enumerate(slots):
                if slot in p.positions or (slot == "P" and p.is_pitcher):
                    cost[i, j] = 0
        rows, cols = linear_sum_assignment(cost)
        assert cost[rows, cols].sum() == 0


def test_requested_stack_shape_is_delivered(slate_sim):
    slate, sim = slate_sim
    optimizer = LineupOptimizer(slate)
    mean = sim.summary()["mean"].to_numpy()
    lookup = {p: i for i, p in enumerate(sim.player_ids)}
    objective = mean[[lookup[p] for p in optimizer.player_ids]]

    lineup = optimizer.solve(objective, stack_shape=(5, 3))
    assert lineup is not None
    sizes = sorted(lineup.stack.values(), reverse=True)
    assert sizes[0] >= 5
    assert sizes[1] >= 3


def test_locks_and_excludes_are_respected(slate_sim):
    slate, sim = slate_sim
    optimizer = LineupOptimizer(slate)
    objective = np.ones(optimizer.n)

    locked = optimizer.player_ids[5]
    excluded = optimizer.player_ids[6]
    lineup = optimizer.solve(objective, locks=[locked], excludes=[excluded])
    assert locked in lineup.player_ids
    assert excluded not in lineup.player_ids


# --------------------------------------------------------------------------
# Contests and ROI
# --------------------------------------------------------------------------


@pytest.mark.parametrize(
    "contest",
    [large_gpp(50_000, 5.0), double_up(10_000, 5.0), single_entry_gpp(5_000, 10.0)],
)
def test_payouts_never_exceed_the_prize_pool(contest):
    gross = contest.entry_fee * contest.n_entries
    assert contest.total_prizes <= gross
    assert 0.10 < contest.rake < 0.20


def test_payout_table_is_monotone():
    contest = large_gpp(50_000, 5.0)
    table = contest.payout_table()[1:]
    assert (np.diff(table) <= 1e-6).all()
    assert table[0] > table[100]


def test_rank_against_field_matches_brute_force():
    rng = np.random.default_rng(0)
    field = rng.normal(100, 25, (12, 40)).astype(np.float32)
    cand = rng.normal(100, 25, (12, 5)).astype(np.float32)

    got = _rank_against_field(cand, field)
    want = np.array(
        [[(field[r] > cand[r, j]).sum() for j in range(5)] for r in range(12)]
    )
    assert (got == want).all()


def test_roi_is_finite_and_ranks_lineups(slate_sim, ownership, pool):
    slate, sim = slate_sim
    field = generate_field(slate, ownership, n_lineups=3000, seed=2)
    contest = large_gpp(20_000, 5.0)

    ev = evaluate_lineups(pool, sim.scores, sim.player_ids, field, contest)
    assert len(ev) == len(pool)
    assert np.isfinite(ev["roi"]).all()
    assert (ev["roi"] >= -1.0).all()  # cannot lose more than the entry fee
    assert ev["p_win"].between(0, 1).all()
    assert ev["p_cash"].between(0, 1).all()
    assert ev["roi"].std() > 0  # the evaluation actually discriminates


def test_portfolio_respects_exposure_and_overlap(slate_sim, ownership, pool):
    slate, sim = slate_sim
    field = generate_field(slate, ownership, n_lineups=2000, seed=2)
    ev = evaluate_lineups(pool, sim.scores, sim.player_ids, field, large_gpp(20_000, 5.0))

    n = 8
    selected = select_portfolio(pool, ev, n_lineups=n)
    assert len(selected) <= n

    chosen = [pool[int(i) - 1] for i in selected["lineup"]]
    counts: dict[str, int] = {}
    for lineup in chosen:
        for pid in lineup.player_ids:
            counts[pid] = counts.get(pid, 0) + 1
    assert max(counts.values()) <= np.floor(0.60 * n)

    for i, a in enumerate(chosen):
        for b in chosen[i + 1 :]:
            assert len(set(a.player_ids) & set(b.player_ids)) <= 7
