"""DraftKings MLB Classic scoring.

Two entry points that must agree with each other:

``score_hitter`` / ``score_pitcher`` take a box-score line and return points.
They are the readable reference implementation and what the tests pin.

``hitter_pa_points`` and ``pitcher_event_points`` return per-event point
vectors indexed by outcome, which is what the simulator accumulates. Keeping
both in one module makes it obvious when they drift apart.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np

from .config import (
    HITTER_SCORING,
    N_OUTCOMES,
    PITCHER_SCORING,
    BB,
    DOUBLE,
    FIELD_OUT,
    HBP,
    HR,
    K,
    SINGLE,
    TRIPLE,
)


@dataclass
class HitterLine:
    singles: int = 0
    doubles: int = 0
    triples: int = 0
    home_runs: int = 0
    rbi: int = 0
    runs: int = 0
    walks: int = 0
    hit_by_pitch: int = 0
    stolen_bases: int = 0


@dataclass
class PitcherLine:
    outs: int = 0  # outs recorded; innings pitched = outs / 3
    strikeouts: int = 0
    win: bool = False
    earned_runs: int = 0
    hits_against: int = 0
    walks_against: int = 0
    hit_batsmen: int = 0
    complete_game: bool = False
    shutout: bool = False
    no_hitter: bool = False


def score_hitter(line: HitterLine) -> float:
    """DraftKings points for a hitter's box-score line."""
    s = HITTER_SCORING
    return (
        line.singles * s.single
        + line.doubles * s.double
        + line.triples * s.triple
        + line.home_runs * s.home_run
        + line.rbi * s.rbi
        + line.runs * s.run
        + line.walks * s.walk
        + line.hit_by_pitch * s.hit_by_pitch
        + line.stolen_bases * s.stolen_base
    )


def score_pitcher(line: PitcherLine) -> float:
    """DraftKings points for a pitcher's box-score line.

    Innings pitched are scored per out (2.25 / 3 = 0.75) so that partial
    innings score correctly -- DraftKings credits 0.75 for each out recorded,
    not 2.25 only on completed innings.
    """
    s = PITCHER_SCORING
    points = (
        line.outs * (s.inning_pitched / 3.0)
        + line.strikeouts * s.strikeout
        + line.earned_runs * s.earned_run
        + line.hits_against * s.hit_against
        + line.walks_against * s.walk_against
        + line.hit_batsmen * s.hit_batsman
    )
    if line.win:
        points += s.win
    if line.complete_game:
        points += s.complete_game
        if line.shutout:
            points += s.complete_game_shutout
        if line.no_hitter:
            points += s.no_hitter
    return points


def hitter_pa_points() -> np.ndarray:
    """Points a hitter earns for the outcome of a plate appearance itself.

    Indexed by the outcome codes in :mod:`mlbdfs.config`. Runs, RBI and
    stolen bases are context dependent and credited separately by the
    simulator, so they are not included here.
    """
    s = HITTER_SCORING
    pts = np.zeros(N_OUTCOMES, dtype=np.float32)
    pts[SINGLE] = s.single
    pts[DOUBLE] = s.double
    pts[TRIPLE] = s.triple
    pts[HR] = s.home_run
    pts[BB] = s.walk
    pts[HBP] = s.hit_by_pitch
    return pts


def pitcher_event_points() -> np.ndarray:
    """Points the opposing pitcher earns for each plate appearance outcome.

    Outs recorded and earned runs are credited separately by the simulator,
    since an out can come from a plate appearance, a double play or a caught
    stealing, and runs are scored when they cross the plate.
    """
    s = PITCHER_SCORING
    pts = np.zeros(N_OUTCOMES, dtype=np.float32)
    pts[K] = s.strikeout
    pts[BB] = s.walk_against
    pts[HBP] = s.hit_batsman
    pts[SINGLE] = s.hit_against
    pts[DOUBLE] = s.hit_against
    pts[TRIPLE] = s.hit_against
    pts[HR] = s.hit_against
    return pts


def points_per_out() -> float:
    return PITCHER_SCORING.inning_pitched / 3.0


# Precomputed vectors -- the simulator reuses these rather than rebuilding
# them inside its inner loop.
HITTER_PA_POINTS = hitter_pa_points()
PITCHER_EVENT_POINTS = pitcher_event_points()
POINTS_PER_OUT = points_per_out()

# Outcome-code masks the simulator indexes with.
OUT_CODES = np.array([K, FIELD_OUT], dtype=np.int8)
HIT_CODES = np.array([SINGLE, DOUBLE, TRIPLE, HR], dtype=np.int8)
