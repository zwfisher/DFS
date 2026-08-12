"""DraftKings scoring, checked against hand-computed box score lines.

These are the tests most worth having: every layer above them is built on
these constants, and an error here is invisible everywhere else.
"""

from __future__ import annotations

import numpy as np
import pytest

from mlbdfs.config import BB, DOUBLE, HBP, HR, K, SINGLE, TRIPLE
from mlbdfs.scoring import (
    HITTER_PA_POINTS,
    POINTS_PER_OUT,
    HitterLine,
    PitcherLine,
    score_hitter,
    score_pitcher,
)


def test_empty_lines_score_zero():
    assert score_hitter(HitterLine()) == 0
    assert score_pitcher(PitcherLine()) == 0


def test_two_homer_game():
    # 4-for-4: two home runs, a double, a single, 5 RBI, 3 runs, a steal.
    line = HitterLine(
        singles=1, doubles=1, home_runs=2, rbi=5, runs=3, stolen_bases=1
    )
    # 3 + 5 + 20 + 10 + 6 + 5
    assert score_hitter(line) == pytest.approx(49.0)


def test_walk_and_hbp_score_the_same_as_each_other():
    assert score_hitter(HitterLine(walks=1)) == score_hitter(HitterLine(hit_by_pitch=1))
    assert score_hitter(HitterLine(walks=1)) == pytest.approx(2.0)


def test_cycle():
    line = HitterLine(singles=1, doubles=1, triples=1, home_runs=1, rbi=4, runs=4)
    # 3 + 5 + 8 + 10 + 8 + 8
    assert score_hitter(line) == pytest.approx(42.0)


def test_quality_start():
    # Six innings, seven strikeouts, two earned runs, five hits, two walks, win.
    line = PitcherLine(
        outs=18, strikeouts=7, earned_runs=2, hits_against=5, walks_against=2, win=True
    )
    # 13.5 + 14 - 4 - 3.0 - 1.2 + 4
    assert score_pitcher(line) == pytest.approx(23.3)


def test_partial_innings_score_per_out():
    """DraftKings credits each out, so 5.1 innings is not the same as 5.0."""
    five = score_pitcher(PitcherLine(outs=15))
    five_and_one = score_pitcher(PitcherLine(outs=16))
    assert five_and_one - five == pytest.approx(0.75)
    assert POINTS_PER_OUT == pytest.approx(0.75)


def test_blowup_start_scores_negative():
    # Two innings, eight earned runs, ten hits, three walks, one strikeout.
    line = PitcherLine(
        outs=6, strikeouts=1, earned_runs=8, hits_against=10, walks_against=3
    )
    # 4.5 + 2 - 16 - 6.0 - 1.8
    assert score_pitcher(line) == pytest.approx(-17.3)
    assert score_pitcher(line) < 0


def test_complete_game_shutout_bonuses_stack():
    base = PitcherLine(outs=27, strikeouts=10, hits_against=4, win=True)
    cg = PitcherLine(**{**base.__dict__, "complete_game": True})
    cgso = PitcherLine(**{**base.__dict__, "complete_game": True, "shutout": True})
    no_no = PitcherLine(
        outs=27,
        strikeouts=10,
        hits_against=0,
        win=True,
        complete_game=True,
        shutout=True,
        no_hitter=True,
    )

    assert score_pitcher(cg) - score_pitcher(base) == pytest.approx(2.5)
    assert score_pitcher(cgso) - score_pitcher(cg) == pytest.approx(2.5)
    # The no-hitter adds its own 5 on top, and gives back the hit penalties.
    assert score_pitcher(no_no) > score_pitcher(cgso)


def test_bonuses_require_a_complete_game():
    """A shutout flag without a complete game must not pay."""
    line = PitcherLine(outs=21, shutout=True, no_hitter=True)
    assert score_pitcher(line) == pytest.approx(21 * 0.75)


def test_pa_point_vector_matches_the_box_score_function():
    """The simulator's per-event vector must agree with the reference."""
    cases = {
        SINGLE: HitterLine(singles=1),
        DOUBLE: HitterLine(doubles=1),
        TRIPLE: HitterLine(triples=1),
        HR: HitterLine(home_runs=1),
        BB: HitterLine(walks=1),
        HBP: HitterLine(hit_by_pitch=1),
    }
    for code, line in cases.items():
        assert HITTER_PA_POINTS[code] == pytest.approx(score_hitter(line))

    # Outs are worth nothing to a hitter.
    assert HITTER_PA_POINTS[K] == 0
    assert np.count_nonzero(HITTER_PA_POINTS) == len(cases)
