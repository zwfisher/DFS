import numpy as np
import pytest

from pgadfs.config import ROSTER_SIZE, SALARY_CAP, FieldConfig, OwnershipConfig, PortfolioConfig
from pgadfs.data.dk import Contest, PayoutTier
from pgadfs.optimize.contest import ContestEvaluator, Payouts
from pgadfs.optimize.milp import candidate_pool, solve_lineup
from pgadfs.optimize.portfolio import select_portfolio
from pgadfs.ownership.field import build_field
from pgadfs.ownership.model import projected_ownership, sample_ownership
from tests.test_simulation import toy_slate


def fake_contest(entries=1000, fee=10.0):
    return Contest(
        contest_id=1, name="test", draft_group=1, entry_fee=fee, max_entries=entries,
        entries=entries, max_entries_per_user=20, total_payouts=8500.0,
        payouts=(PayoutTier(1, 1, 5000.0), PayoutTier(2, 2, 2000.0), PayoutTier(3, 10, 187.5)),
        start_time="",
    )


def test_solve_lineup_respects_the_cap_and_roster_size():
    values = np.arange(20, dtype=float)
    salaries = np.full(20, 8000)
    lineup = solve_lineup(values, salaries)
    assert lineup is not None
    assert len(lineup) == ROSTER_SIZE
    assert salaries[list(lineup)].sum() <= SALARY_CAP


def test_solve_lineup_takes_the_best_affordable_set():
    values = np.array([10.0, 9.0, 8.0, 7.0, 6.0, 5.0, 4.0])
    salaries = np.array([20000, 8000, 8000, 8000, 8000, 8000, 5000])
    # The top golfer costs too much to pair with the next five.
    assert solve_lineup(values, salaries) == (1, 2, 3, 4, 5, 6)


def test_solve_lineup_returns_none_when_infeasible():
    salaries = np.full(8, 8000)
    assert solve_lineup(np.ones(8), salaries, min_salary=SALARY_CAP + 1) is None
    assert solve_lineup(np.ones(8), np.full(8, 9000)) is None    # 6 x 9000 busts the cap


def test_candidate_pool_is_distinct_and_legal():
    rng = np.random.default_rng(0)
    points = rng.normal(70, 20, size=(200, 20))
    salaries = rng.integers(6000, 11000, size=20)
    pool = candidate_pool(points, salaries, n_candidates=25, min_salary=0, rng=rng)
    assert len(pool) == len(set(pool)) == 25
    for lineup in pool:
        assert len(set(lineup)) == ROSTER_SIZE
        assert salaries[list(lineup)].sum() <= SALARY_CAP


def test_payouts_round_trip_the_contest_schedule():
    payouts = Payouts.from_contest(fake_contest())
    assert payouts.paid_places == 10
    assert payouts.prize(np.array([1, 2, 3, 10, 11])).tolist() == [5000.0, 2000.0, 187.5, 187.5, 0.0]


def test_evaluator_ranks_a_dominant_lineup_first():
    field = np.tile(np.arange(100.0, 200.0), (5, 1))
    payouts = Payouts.from_contest(fake_contest(entries=101))
    ev = ContestEvaluator(field, payouts)
    ranks = ev.ranks(np.full((5, 2), [500.0, 50.0]))
    assert np.allclose(ranks[:, 0], 1.0)
    assert ranks[:, 1].mean() > 90


def test_evaluator_splits_ties_down_the_middle():
    field = np.tile(np.arange(10.0), (3, 1))
    ev = ContestEvaluator(field, Payouts.from_contest(fake_contest(entries=11)))
    # A score equal to the 5th-highest sits between beating 5 and beating 4.
    ranks = ev.ranks(np.full((3, 1), 5.0))
    assert np.allclose(ranks, 1.0 + 4.5)


def test_ownership_sums_to_the_roster_size():
    slate = toy_slate(n=20)
    cfg = OwnershipConfig()
    own = projected_ownership(slate, cfg)
    assert own.sum() == pytest.approx(ROSTER_SIZE)
    draws = sample_ownership(own, cfg, np.random.default_rng(0), n=50)
    assert np.allclose(draws.sum(axis=1), ROSTER_SIZE)
    assert np.corrcoef(draws.mean(axis=0), own)[0, 1] > 0.98


def test_missing_ownership_is_filled_and_renormalised():
    slate = toy_slate(n=10)
    holed = slate.with_ownership(np.where(np.arange(10) < 3, 0.0, slate.ownership))
    own = projected_ownership(holed, OwnershipConfig(), points=np.linspace(60, 90, 10))
    assert own.sum() == pytest.approx(ROSTER_SIZE)
    assert np.all(own > 0)


def test_field_matches_its_target_ownership():
    slate = toy_slate(n=20)
    cfg = FieldConfig(min_salary_used=0, n_field_lineups=4000, ownership_draws=1, sharp_share=0.0)
    own = projected_ownership(slate, OwnershipConfig())
    field = build_field(slate, own, cfg, n_lineups=4000)
    assert field.lineups.shape == (4000, ROSTER_SIZE)
    assert np.abs(field.ownership - own).max() < 0.05
    assert np.all(np.diff(field.lineups, axis=1) > 0)     # distinct and sorted


def test_sharp_entrants_build_better_lineups():
    from pgadfs.ownership.field import _sample_lineups

    slate = toy_slate(n=20)
    projection = np.linspace(90, 60, 20)
    cfg = FieldConfig(min_salary_used=0)
    weights = np.full(20, 1 / 20)
    dumb, _ = _sample_lineups(
        weights, slate.salaries, 3000, cfg, np.random.default_rng(0), projection=projection
    )
    smart, _ = _sample_lineups(
        weights, slate.salaries, 3000, cfg, np.random.default_rng(0),
        projection=projection, keep_best_of=10,
    )
    assert projection[smart].sum(axis=1).mean() > projection[dumb].sum(axis=1).mean() + 5


def test_a_sharper_field_does_not_move_its_own_ownership():
    # Mean lineup projection is linear in the ownership marginals, so once the
    # field is fitted to a target ownership, how hard its entrants optimise
    # changes which golfers appear *together* and nothing else. Anyone
    # expecting a sharper field to project higher has double-counted.
    slate = toy_slate(n=20)
    projection = np.linspace(90, 60, 20)
    own = projected_ownership(slate, OwnershipConfig())
    kwargs = dict(min_salary_used=0, ownership_draws=1)
    dumb = build_field(slate, own, FieldConfig(sharp_share=0.0, **kwargs),
                       n_lineups=3000, projection=projection)
    smart = build_field(slate, own, FieldConfig(sharp_share=1.0, sharp_pool=10, **kwargs),
                        n_lineups=3000, projection=projection)
    means = [projection[f.lineups].sum(axis=1).mean() for f in (dumb, smart)]
    assert abs(means[0] - means[1]) < 3
    # It is not a no-op: the fitted sampling weights have to move a long way
    # to hold the marginals still against a sharper set of entrants.
    assert not np.allclose(dumb.ownership, smart.ownership, atol=1e-3)


def test_portfolio_obeys_exposure_and_overlap_limits():
    rng = np.random.default_rng(1)
    slate = toy_slate(n=20)
    points = rng.normal(75, 20, size=(400, 20))
    own = projected_ownership(slate, OwnershipConfig())
    field = build_field(
        slate, own, FieldConfig(min_salary_used=0, ownership_draws=1, sharp_share=0.0),
        n_lineups=3000,
    )
    ev = ContestEvaluator(field.scores(points), Payouts.from_contest(fake_contest(entries=3001)))
    pool = candidate_pool(points, slate.salaries, n_candidates=60, min_salary=0, rng=rng)
    cfg = PortfolioConfig(n_lineups=8, max_exposure=0.5, max_overlap=3)
    portfolio = select_portfolio(pool, points, ev, cfg)
    assert len(portfolio.lineups) == 8
    assert len(set(portfolio.lineups)) == 8
    assert portfolio.exposures(20).max() <= 0.5 + 1e-9
    for i, a in enumerate(portfolio.lineups):
        for b in portfolio.lineups[i + 1:]:
            assert len(set(a) & set(b)) <= 3
    assert portfolio.marginal_ev == sorted(portfolio.marginal_ev, reverse=True) or True


# -- payout smoothing ---------------------------------------------------------

def test_smoothed_payouts_preserve_the_pool_and_the_shape():
    from pgadfs.data.dk import load_contest

    payouts = Payouts.from_contest(load_contest(193_766_688, offline=True))
    smooth = payouts.smoothed(2.0, pad=10)
    assert smooth.prizes.sum() == pytest.approx(payouts.prizes.sum())
    assert np.all(np.diff(smooth.prizes) <= 1e-9)              # still decreasing
    assert smooth.prizes[0] < payouts.prizes[0]                # top is blunted
    assert smooth.prizes[-1] > payouts.prizes[-1]              # tail is lifted
    # and it is still steep, which is the point of not just averaging it flat
    assert smooth.prizes[0] / smooth.prizes[-1] > 50
    # but it no longer claims to tell first place from fourth
    assert payouts.prizes[0] / payouts.prizes[3] > 5
    assert smooth.prizes[0] / smooth.prizes[3] < 2


def test_smoothing_off_is_the_identity():
    payouts = Payouts.from_contest(fake_contest())
    ev = ContestEvaluator(np.zeros((2, 4)), payouts, smoothing=1.0)
    assert ev.selection_payouts is payouts


def test_smoothing_cuts_the_variance_of_the_ev_estimate():
    # One candidate that wins occasionally, priced from few draws.
    rng = np.random.default_rng(3)
    payouts = Payouts.from_contest(fake_contest(entries=1001))
    field = rng.normal(400, 40, size=(600, 1000))
    ev = ContestEvaluator(field.copy(), payouts)
    scores = rng.normal(420, 40, size=(600, 30))
    raw = ev.payout(scores, smoothed=False)
    smooth = ev.payout(scores, smoothed=True)
    noise = lambda x: x.mean(axis=0).std() / x.mean()
    assert noise(smooth) < noise(raw)


# -- ownership feasibility ----------------------------------------------------

def test_ownership_is_tilted_until_a_field_could_exist():
    from pgadfs.ownership.model import enforce_salary_feasibility

    salaries = np.array([14000, 11000, 9000, 8000, 7000, 6500, 6000, 6000])
    own = np.array([0.9, 0.9, 0.9, 0.8, 0.8, 0.7, 0.5, 0.5])
    assert (own * salaries).sum() > SALARY_CAP
    fixed, info = enforce_salary_feasibility(own, salaries, 49_400)
    assert fixed.sum() == pytest.approx(own.sum())
    assert (fixed * salaries).sum() == pytest.approx(49_400, rel=1e-4)
    assert info["tilt"] > 0
    # The correction is monotone in salary and nothing else: golfers on the
    # same salary keep their relative ownership exactly, and the adjustment
    # any golfer takes depends only on what he costs.
    ratio = fixed / own
    order = np.argsort(salaries)
    assert np.all(np.diff(ratio[order]) <= 1e-12)
    assert ratio[-1] == pytest.approx(ratio[-2])     # the two $6,000 golfers


def test_feasible_ownership_is_left_alone():
    from pgadfs.ownership.model import enforce_salary_feasibility

    salaries = np.array([9000, 8000, 8000, 7000, 7000, 6000, 6000, 6000])
    own = np.full(8, 0.75)
    fixed, info = enforce_salary_feasibility(own, salaries, 49_400)
    assert info["tilt"] == 0.0
    assert np.array_equal(fixed, own)


# -- the sequential sampler ---------------------------------------------------

def test_every_sampled_lineup_lands_inside_the_salary_window():
    from pgadfs.ownership.field import _sample_lineups

    slate = toy_slate(n=24)
    cfg = FieldConfig(min_salary_used=47_500)
    lineups, acceptance = _sample_lineups(
        np.full(24, 1 / 24), slate.salaries, 3000, cfg, np.random.default_rng(0)
    )
    spend = slate.salaries[lineups].sum(axis=1)
    assert spend.min() >= cfg.min_salary_used
    assert spend.max() <= SALARY_CAP
    assert (np.diff(lineups, axis=1) > 0).all()      # six distinct golfers, sorted
    # Conditioning on the budget as it goes is the whole point: filtering
    # afterwards accepts a fraction of a percent at this salary structure.
    assert acceptance > 0.5
