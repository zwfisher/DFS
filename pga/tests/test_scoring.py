import numpy as np
import pytest

from pgadfs import config
from pgadfs.sim.engine import finish_points
from pgadfs.sim.holes import (
    BIRDIE,
    BOGEY,
    DOUBLE_PLUS,
    EAGLE_PLUS,
    PAR,
    cutpoints,
    points_by_category,
    strokes_by_category,
)


def test_finish_table_covers_fifty_places():
    assert len(config.FINISH_POINTS) == 50
    assert config.FINISH_POINTS[0] == 30
    assert config.FINISH_POINTS[9] == 7
    assert config.FINISH_POINTS[10] == 6      # 11th
    assert config.FINISH_POINTS[14] == 6      # 15th
    assert config.FINISH_POINTS[15] == 5      # 16th
    assert config.FINISH_POINTS[30] == 2      # 31st
    assert config.FINISH_POINTS[49] == 1      # 50th
    assert all(a >= b for a, b in zip(config.FINISH_POINTS, config.FINISH_POINTS[1:]))


def test_hole_points_are_monotone_in_score():
    order = [-3, -2, -1, 0, 1, 2]
    values = [config.HOLE_POINTS[k] for k in order]
    assert values == sorted(values, reverse=True)


@pytest.mark.parametrize("par", [3, 4, 5])
def test_category_points_and_strokes_line_up(par):
    pts = points_by_category(par)
    strokes = strokes_by_category(par)
    assert len(pts) == len(strokes) == 5
    assert strokes[EAGLE_PLUS] < strokes[BIRDIE] < strokes[PAR] < strokes[BOGEY] < strokes[DOUBLE_PLUS]
    assert pts[EAGLE_PLUS] > pts[BIRDIE] > pts[PAR] > pts[BOGEY] > pts[DOUBLE_PLUS]


def test_par_three_eagle_carries_the_ace_bonus():
    # The only way to be two under on a par 3 is to hole the tee shot.
    assert points_by_category(3)[EAGLE_PLUS] == config.HOLE_POINTS[-2] + config.HOLE_IN_ONE_BONUS
    assert points_by_category(4)[EAGLE_PLUS] == config.HOLE_POINTS[-2]


@pytest.mark.parametrize("par", [3, 4, 5])
def test_cutpoints_reproduce_the_baseline_frequencies(par):
    tau = cutpoints(par)
    cum = 1.0 / (1.0 + np.exp(-tau))
    probs = np.diff(np.concatenate([[0.0], cum, [1.0]]))
    expected = np.asarray(config.BASELINE_HOLE_PROBS[par])
    expected = expected / expected.sum()
    assert np.allclose(probs, expected, atol=1e-12)


def test_finish_points_best_rule_gives_ties_the_top_of_the_band():
    # Three players tied for third: 3rd is worth 18.
    strokes = np.array([[270, 271, 275, 275, 275, 280]])
    position, pts = finish_points(strokes, "best")
    assert position.tolist() == [[1, 2, 3, 3, 3, 6]]
    assert pts.tolist() == [[30.0, 20.0, 18.0, 18.0, 18.0, 12.0]]


def test_finish_points_average_rule_splits_the_band():
    strokes = np.array([[270, 271, 275, 275, 275, 280]])
    _, pts = finish_points(strokes, "average")
    assert pts[0, 2] == pytest.approx((18 + 16 + 14) / 3)
    assert pts[0, 0] == 30.0


def test_finish_points_beyond_the_table_are_zero():
    strokes = np.arange(60)[None, :] + 270
    _, pts = finish_points(strokes, "best")
    assert pts[0, 49] == 1.0
    assert pts[0, 50] == 0.0


def test_finish_points_handles_a_full_tie():
    strokes = np.full((1, 5), 280)
    position, pts = finish_points(strokes, "best")
    assert position.tolist() == [[1] * 5]
    assert pts.tolist() == [[30.0] * 5]
    _, avg = finish_points(strokes, "average")
    assert avg[0, 0] == pytest.approx((30 + 20 + 18 + 16 + 14) / 5)


def test_unknown_tie_rule_is_rejected():
    with pytest.raises(ValueError):
        finish_points(np.array([[270, 271]]), "nearest")


# -- checks against real DraftKings output -------------------------------------
# DataGolf publishes one event of its historical DFS archive free. Every row is
# DraftKings' own scoring of a real tournament, broken into components, so the
# finish table and the tie rule can be checked against the thing itself.

from pgadfs.data.dkpoints import load_sample, observed_bonus_rates  # noqa: E402


def test_real_output_confirms_the_finish_points_table():
    seen = 0
    for row in load_sample():
        if row.position is None:
            assert row.finish_pts == 0.0, row
            continue
        expected = (
            config.FINISH_POINTS[row.position - 1]
            if row.position <= len(config.FINISH_POINTS)
            else 0.0
        )
        assert row.finish_pts == expected, (row.player, row.finish_text, row.finish_pts)
        seen += 1
    assert seen > 50


def test_real_output_confirms_the_best_of_the_tie_rule():
    # If DraftKings averaged the band, a T2 among two players would be 19, not 20.
    ties = [r for r in load_sample() if r.tied and r.position and r.position <= 50]
    assert ties
    for row in ties:
        assert row.finish_pts == config.FINISH_POINTS[row.position - 1]
    assert any(r.position == 2 and r.finish_pts == 20.0 for r in ties)


def test_real_output_components_add_up_to_the_total():
    for row in load_sample():
        parts = (
            row.hole_score_pts + row.finish_pts + row.streak_pts
            + row.bogey_free_pts + row.hole_in_one_pts + row.sub_70_pts
        )
        assert parts == pytest.approx(row.total_pts, abs=1e-9)


def test_real_bonus_points_come_in_multiples_of_the_bonus():
    for row in load_sample():
        assert row.streak_pts % config.STREAK_BONUS == 0
        assert row.bogey_free_pts % config.BOGEY_FREE_BONUS == 0
        assert row.sub_70_pts in (0.0, config.ALL_ROUNDS_UNDER_70)
        assert row.hole_in_one_pts % config.HOLE_IN_ONE_BONUS == 0


def test_observed_bonus_rates_are_in_a_sane_range():
    rates = observed_bonus_rates()
    assert 0.0 < rates["rounds with a 3-birdie streak"] < 0.25
    assert 0.0 < rates["bogey-free rounds"] < 0.20
    assert 8.0 < rates["hole scoring points per round"] < 22.0
