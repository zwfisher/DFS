from dataclasses import replace

import numpy as np
import pytest

from pgadfs.config import BELLERIVE, SimConfig
from pgadfs.projections.calibrate import _expected_round_score, fit_course
from pgadfs.sim.engine import simulate, simulate_strokes
from pgadfs.sim.holes import BIRDIE, HoleModel
from pgadfs.slate import Golfer, Slate


def toy_slate(n=12, spread=0.4):
    """A synthetic slate: talent, salary and ownership all decline together."""
    weights = np.linspace(2.0, 0.5, n)
    own = 600.0 * weights / weights.sum()
    golfers = tuple(
        Golfer(
            dk_id=1000 + i,
            name=f"Golfer {i}",
            salary=int(9500 - 250 * i),
            skill=spread * (n / 2 - i) / n * 2,
            course_fit=0.0,
            wave=i % 2,
            ownership=float(own[i]),
        )
        for i in range(n)
    )
    return Slate(event="toy", course="toy", golfers=golfers)


def toy_model(mu=3.19, shift=0.0):
    return HoleModel(BELLERIVE, mu, shift)


def test_sampler_matches_the_analytic_category_probabilities():
    model = toy_model()
    rng = np.random.default_rng(7)
    z = np.zeros((400_000, 1), dtype=np.float32)
    cats = model.sample(z, rng)
    empirical = np.stack([(cats == k).mean(axis=0) for k in range(5)], axis=-1)
    analytic = model.category_probs(np.zeros((1, 1), dtype=np.float32))[0]
    assert np.abs(empirical - analytic).max() < 0.003


def test_expected_strokes_falls_as_talent_rises():
    model = toy_model()
    scores = model.expected_strokes(np.array([-1.0, 0.0, 1.0, 2.0]))
    assert np.all(np.diff(scores) < 0)


def test_fit_course_gives_a_unit_stroke_scale_and_hits_the_target():
    edges = np.linspace(-1.0, 1.5, 30)
    noise = 1.3
    mu, shift = fit_course(BELLERIVE, edges, target_score_to_par=-0.4, noise_sd=noise)
    model = HoleModel(BELLERIVE, mu, shift)
    assert _expected_round_score(model, edges, noise).mean() == pytest.approx(-0.4, abs=1e-6)
    lo, hi = _expected_round_score(model, np.array([0.5, -0.5]), noise)
    assert hi - lo == pytest.approx(1.0, abs=1e-4)


def test_a_stroke_of_talent_is_a_stroke_on_the_leaderboard():
    slate = toy_slate(n=12, spread=1.2)
    edges = slate.edge
    noise = float(np.hypot(0.8, 1.05))
    mu, shift = fit_course(BELLERIVE, edges, -0.5, noise)
    cfg = SimConfig(n_sims=6000, week_sd=0.8, round_sd=1.05, seed=3)
    strokes = simulate_strokes(slate, cfg, HoleModel(BELLERIVE, mu, shift))
    per_round = strokes.mean(axis=0) / cfg.rounds
    fit = np.polyfit(edges, per_round, 1)[0]
    assert fit == pytest.approx(-1.0, abs=0.06)


def test_simulation_is_reproducible_and_internally_consistent():
    slate = toy_slate()
    cfg = SimConfig(n_sims=800, seed=11)
    model = toy_model()
    a = simulate(slate, cfg, model)
    b = simulate(slate, cfg, model)
    assert np.array_equal(a.points, b.points)
    assert np.allclose(a.points, a.hole_points + a.bonus_points + a.finish_points)
    assert a.points.shape == (800, len(slate))


def test_strokes_only_matches_the_full_run():
    slate = toy_slate()
    cfg = SimConfig(n_sims=500, seed=5)
    model = toy_model()
    assert np.array_equal(simulate_strokes(slate, cfg, model), simulate(slate, cfg, model).strokes)


def test_bonuses_only_ever_take_their_documented_values():
    slate = toy_slate()
    sim = simulate(slate, SimConfig(n_sims=1500, seed=2), toy_model())
    # 4 rounds x (3 streak + 3 bogey-free) + 5 for the week, in half-open steps of 3.
    assert sim.bonus_points.min() >= 0.0
    assert sim.bonus_points.max() <= 4 * 6 + 5
    combos = np.unique(sim.bonus_points)
    assert set(np.unique(combos % 1.0)) == {0.0}


def test_every_round_under_70_is_exactly_the_five_point_bonus():
    slate = toy_slate()
    cfg = SimConfig(n_sims=1500, seed=13)
    sim = simulate(slate, cfg, toy_model())
    swept = sim.rounds_under_70 == cfg.rounds
    implied = sim.bonus_points - 3.0 * (sim.streak_rounds + sim.bogey_free_rounds)
    assert np.all(implied[swept] == 5.0)
    assert np.all(implied[~swept] == 0.0)


def test_a_stronger_field_finishes_ahead_more_often():
    slate = toy_slate(n=20, spread=1.5)
    sim = simulate(slate, SimConfig(n_sims=4000, seed=17), toy_model())
    wins = (sim.position == 1).mean(axis=0)
    assert wins.argmax() == int(np.argmax(slate.edge))
    assert wins[np.argmax(slate.edge)] > wins[np.argmin(slate.edge)] * 3


def test_week_variance_flattens_the_win_distribution():
    slate = toy_slate(n=20, spread=1.5)
    model = toy_model()
    tight = simulate(slate, SimConfig(n_sims=4000, seed=19, week_sd=0.2), model)
    loose = simulate(slate, SimConfig(n_sims=4000, seed=19, week_sd=1.6), model)
    best = int(np.argmax(slate.edge))
    assert (tight.position[:, best] == 1).mean() > (loose.position[:, best] == 1).mean()


def test_wave_shock_only_touches_the_first_two_rounds():
    slate = toy_slate(n=12)
    model = toy_model()
    base = SimConfig(n_sims=3000, seed=23, wave_sd=0.0)
    windy = replace(base, wave_sd=1.5)
    calm_sim = simulate(slate, base, model)
    windy_sim = simulate(slate, windy, model)
    # A shared shock adds variance to totals without moving the mean much.
    assert windy_sim.strokes.std(axis=0).mean() > calm_sim.strokes.std(axis=0).mean()
    assert abs(windy_sim.strokes.mean() - calm_sim.strokes.mean()) < 1.0


def test_birdie_counts_are_consistent_with_hole_points():
    slate = toy_slate()
    sim = simulate(slate, SimConfig(n_sims=1000, seed=29), toy_model())
    # Birdies-or-better includes eagles, so it can never be smaller.
    assert np.all(sim.birdies >= sim.eagles)
    assert np.all(sim.aces <= sim.eagles)
