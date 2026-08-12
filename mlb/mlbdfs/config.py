"""Scoring constants, roster rules and paths.

Everything DraftKings-specific that could change lives here as a one-line edit.
The scoring values below could not be confirmed against DraftKings directly
(the host is unreachable from the build environment) -- verify them against
the live rules page before trusting output, because a wrong constant here
silently corrupts every layer above it.
"""

from __future__ import annotations

import os
from dataclasses import dataclass, field
from pathlib import Path

# --------------------------------------------------------------------------
# Paths
# --------------------------------------------------------------------------

PACKAGE_DIR = Path(__file__).resolve().parent
FIXTURE_DIR = PACKAGE_DIR / "data" / "fixtures"

CACHE_DIR = Path(os.environ.get("MLBDFS_CACHE", Path.home() / ".cache" / "mlbdfs"))

# When set, the data layer refuses to hit the network and serves cache only.
OFFLINE = os.environ.get("MLBDFS_OFFLINE", "0") == "1"


# --------------------------------------------------------------------------
# DraftKings scoring -- MLB Classic
# --------------------------------------------------------------------------


@dataclass(frozen=True)
class HitterScoring:
    single: float = 3.0
    double: float = 5.0
    triple: float = 8.0
    home_run: float = 10.0
    rbi: float = 2.0
    run: float = 2.0
    walk: float = 2.0
    hit_by_pitch: float = 2.0
    stolen_base: float = 5.0


@dataclass(frozen=True)
class PitcherScoring:
    inning_pitched: float = 2.25  # i.e. 0.75 per out recorded
    strikeout: float = 2.0
    win: float = 4.0
    earned_run: float = -2.0
    hit_against: float = -0.6
    walk_against: float = -0.6
    hit_batsman: float = -0.6
    complete_game: float = 2.5
    complete_game_shutout: float = 2.5  # stacks on top of the complete game bonus
    no_hitter: float = 5.0  # stacks on top of both complete game bonuses


HITTER_SCORING = HitterScoring()
PITCHER_SCORING = PitcherScoring()


# --------------------------------------------------------------------------
# Roster rules -- DraftKings MLB Classic
# --------------------------------------------------------------------------


@dataclass(frozen=True)
class RosterRules:
    salary_cap: int = 50_000
    slots: tuple[tuple[str, int], ...] = (
        ("P", 2),
        ("C", 1),
        ("1B", 1),
        ("2B", 1),
        ("3B", 1),
        ("SS", 1),
        ("OF", 3),
    )
    max_hitters_per_team: int = 5
    min_games_represented: int = 2
    # Not a DraftKings rule, but standard GPP practice.
    allow_hitters_vs_own_pitcher: bool = False

    @property
    def size(self) -> int:
        return sum(n for _, n in self.slots)

    @property
    def hitter_slots(self) -> int:
        return sum(n for pos, n in self.slots if pos != "P")

    @property
    def positions(self) -> tuple[str, ...]:
        return tuple(pos for pos, _ in self.slots)


ROSTER = RosterRules()


# --------------------------------------------------------------------------
# Plate appearance outcome space
# --------------------------------------------------------------------------
# Order matters: these are the column indices of every rate matrix in the
# projection and simulation layers.

OUTCOMES: tuple[str, ...] = (
    "strikeout",
    "field_out",
    "walk",
    "hit_by_pitch",
    "single",
    "double",
    "triple",
    "home_run",
)
OUTCOME_INDEX: dict[str, int] = {name: i for i, name in enumerate(OUTCOMES)}
N_OUTCOMES = len(OUTCOMES)

K, FIELD_OUT, BB, HBP, SINGLE, DOUBLE, TRIPLE, HR = range(N_OUTCOMES)

IS_OUT = (K, FIELD_OUT)
IS_HIT = (SINGLE, DOUBLE, TRIPLE, HR)

# League-average per-PA rates, used as the empirical-Bayes prior mean and as
# the league term in the log5 matchup adjustment. Roughly 2023-2025 MLB.
# These must sum to exactly 1.0: everything downstream renormalizes, so a
# vector summing to 1.03 would silently deflate every non-out outcome by 3%
# and cost the simulation about half a run per game.
LEAGUE_RATES: dict[str, float] = {
    "strikeout": 0.2250,
    "field_out": 0.4600,
    "walk": 0.0840,
    "hit_by_pitch": 0.0120,
    "single": 0.1400,
    "double": 0.0440,
    "triple": 0.0040,
    "home_run": 0.0310,
}
assert abs(sum(LEAGUE_RATES.values()) - 1.0) < 1e-9, "LEAGUE_RATES must sum to 1"

# Plate appearances required for a rate to be roughly half-shrunk toward the
# league mean. These are the classic stabilization points; a stat that
# stabilizes slowly (BABIP-driven ones) gets pulled harder at low sample.
STABILIZATION_PA: dict[str, float] = {
    "strikeout": 60.0,
    "field_out": 70.0,
    "walk": 120.0,
    "hit_by_pitch": 240.0,
    "single": 290.0,
    "double": 700.0,
    "triple": 1610.0,
    "home_run": 170.0,
}

# Pitchers' allowed rates stabilize more slowly than hitters' own rates.
PITCHER_STABILIZATION_MULTIPLIER = 2.0

# Exponential decay applied to prior seasons when pooling talent samples.
SEASON_DECAY = 0.6


# --------------------------------------------------------------------------
# Simulation defaults
# --------------------------------------------------------------------------


@dataclass(frozen=True)
class SimConfig:
    n_sims: int = 20_000
    seed: int = 20260812
    max_extra_innings: int = 3
    # Automatic runner on second base in extras (current MLB rule).
    extras_ghost_runner: bool = True
    pitches_per_pa: float = 3.9
    hook_pitch_limit_mean: float = 95.0
    hook_pitch_limit_sd: float = 12.0
    # Pitches a manager assumes the next inning will cost when deciding
    # whether to send the starter back out.
    next_inning_pitches: float = 16.0
    hook_runs_allowed: int = 6  # blow-up hook regardless of pitch count
    win_min_innings: float = 5.0
    dtype: str = "float32"

    # Game-level shared shocks. Without these, the only thing correlating
    # teammates is batting around in the same inning, which produces about
    # +0.09 pairwise correlation -- roughly half of what is actually observed.
    # The dominant real driver is that a starter has a true talent level plus
    # a bad day: when he does not have it, all nine opposing hitters benefit
    # at once. Modelled as a per-simulation multiplier on the opposing
    # lineup's offensive rates.
    pitcher_form_sd: float = 0.155
    n_form_bins: int = 7
    # Weather, umpire and ballpark conditions on the day, shared by both
    # lineups. This is what makes opposing hitters mildly positively
    # correlated and is the statistical basis for game stacks.
    game_env_sd: float = 0.070
    n_env_bins: int = 5
    # Relief corps are an average of several arms, so their collective day
    # varies less than one starter's.
    bullpen_form_damping: float = 0.40


SIM = SimConfig()


# --------------------------------------------------------------------------
# Baserunning advancement probabilities
# --------------------------------------------------------------------------
# Simplified but close to observed MLB advancement frequencies.


# NOTE: reached-on-error is not modelled. It is about 1.4% of plate
# appearances and worth roughly 0.1 runs per team game, which is most of the
# gap between this simulation's 4.28 runs per game and the league's 4.39. It
# is left out because scoring it correctly means tracking which runs are
# unearned, and earned runs are worth -2 to a DraftKings pitcher -- getting
# that wrong would be worse than omitting the play.
@dataclass(frozen=True)
class BaserunningConfig:
    # Runner on first scores on a double.
    first_scores_on_double: float = 0.45
    # Runner on second scores on a single.
    second_scores_on_single: float = 0.65
    # Runner on first reaches third on a single (otherwise second).
    first_to_third_on_single: float = 0.30
    # Runner on third scores on a ball in play out, <2 outs (sac fly / groundout).
    third_scores_on_out: float = 0.35
    # Runner on second advances to third on a ball in play out, <2 outs.
    second_to_third_on_out: float = 0.20
    # Ground into double play, runner on first and <2 outs.
    gidp_rate: float = 0.11
    # Stolen base success rate when an attempt is made.
    sb_success: float = 0.78
    # Per plate appearance with a runner aboard, the chance every runner
    # advances a base without the batter doing anything: wild pitch, passed
    # ball, balk, or a steal of third. Roughly 0.45 such events per team game
    # over the ~13 plate appearances that have a runner on. Without this the
    # simulation scores about 4.1 runs per game instead of 4.4 -- these plays
    # and reached-on-error are most of the missing offense.
    wild_pitch_rate: float = 0.040


BASERUNNING = BaserunningConfig()


# --------------------------------------------------------------------------
# Optimizer defaults
# --------------------------------------------------------------------------


@dataclass(frozen=True)
class OptimizerConfig:
    n_lineups: int = 150
    candidate_pool_multiplier: int = 4  # candidates generated per final lineup
    max_exposure: float = 0.60  # cap on any one player across the portfolio
    max_overlap: int = 7  # max shared players between two final lineups
    stack_shapes: tuple[tuple[int, ...], ...] = (
        (5, 3),
        (5, 2),
        (4, 4),
        (4, 3),
        (4, 2),
    )
    require_consecutive_order: bool = True
    randomize_sigma: float = 1.0  # scale on sampled score noise for the pool
    solver_msg: bool = False


OPTIMIZER = OptimizerConfig()


# --------------------------------------------------------------------------
# Ownership defaults
# --------------------------------------------------------------------------


@dataclass(frozen=True)
class OwnershipConfig:
    # Weights of the structural utility model (see ownership/heuristic.py).
    w_value: float = 1.85  # projected points per $1k
    w_points: float = 0.22  # raw projected points
    w_team_total: float = 0.32  # Vegas implied runs
    w_ceiling: float = 0.14  # p90 of the simulated distribution
    w_order_top: float = 0.30  # batting first through fifth
    w_salary: float = -0.10  # price aversion, net of value
    # Dirichlet concentration controlling ownership uncertainty. Lower is more
    # uncertain; this is the parameter to fit once real ownership data exists.
    dirichlet_concentration: float = 140.0
    # Share of the field that stacks, and the stack size distribution.
    field_stack_rate: float = 0.72
    field_stack_sizes: tuple[int, ...] = (5, 4, 3)
    field_stack_size_weights: tuple[float, ...] = (0.40, 0.35, 0.25)


OWNERSHIP = OwnershipConfig()


# --------------------------------------------------------------------------
# Park factors
# --------------------------------------------------------------------------
# Multiplicative, applied to the relevant outcome rates. 1.0 is neutral.
# Keyed by team abbreviation of the home park.

PARK_FACTORS: dict[str, dict[str, float]] = {
    "COL": {"single": 1.12, "double": 1.14, "triple": 1.45, "home_run": 1.16},
    "BOS": {"single": 1.05, "double": 1.28, "triple": 0.70, "home_run": 1.02},
    "CIN": {"single": 1.00, "double": 0.99, "triple": 0.86, "home_run": 1.20},
    "NYY": {"single": 0.98, "double": 0.96, "triple": 0.72, "home_run": 1.13},
    "PHI": {"single": 0.99, "double": 0.99, "triple": 0.86, "home_run": 1.12},
    "GB_DEFAULT": {},
    "SD": {"single": 0.97, "double": 0.94, "triple": 0.94, "home_run": 0.88},
    "SEA": {"single": 0.95, "double": 0.96, "triple": 0.90, "home_run": 0.94},
    "OAK": {"single": 0.98, "double": 0.94, "triple": 1.02, "home_run": 0.90},
    "MIA": {"single": 0.99, "double": 0.97, "triple": 1.28, "home_run": 0.86},
    "STL": {"single": 1.01, "double": 0.96, "triple": 0.94, "home_run": 0.92},
    "DET": {"single": 1.01, "double": 1.00, "triple": 1.18, "home_run": 0.94},
    "CLE": {"single": 0.99, "double": 0.97, "triple": 0.84, "home_run": 0.98},
    "SF": {"single": 0.99, "double": 0.98, "triple": 1.26, "home_run": 0.90},
    "SFG": {"single": 0.99, "double": 0.98, "triple": 1.26, "home_run": 0.90},
    "SDP": {"single": 0.97, "double": 0.94, "triple": 0.94, "home_run": 0.88},
    "TB": {"single": 0.98, "double": 0.96, "triple": 0.96, "home_run": 0.96},
    "KC": {"single": 1.02, "double": 1.02, "triple": 1.44, "home_run": 0.92},
    "TEX": {"single": 1.01, "double": 1.02, "triple": 0.98, "home_run": 1.04},
    "ATL": {"single": 1.00, "double": 1.02, "triple": 0.90, "home_run": 1.04},
    "BAL": {"single": 1.00, "double": 1.01, "triple": 0.80, "home_run": 1.00},
    "TOR": {"single": 0.99, "double": 1.00, "triple": 0.82, "home_run": 1.06},
    "CHC": {"single": 1.00, "double": 1.03, "triple": 1.02, "home_run": 1.04},
    "CWS": {"single": 1.00, "double": 0.98, "triple": 0.84, "home_run": 1.08},
    "CHW": {"single": 1.00, "double": 0.98, "triple": 0.84, "home_run": 1.08},
    "HOU": {"single": 0.99, "double": 0.98, "triple": 0.82, "home_run": 1.04},
    "LAA": {"single": 0.99, "double": 0.98, "triple": 0.92, "home_run": 1.02},
    "LAD": {"single": 0.98, "double": 0.95, "triple": 0.84, "home_run": 1.06},
    "MIL": {"single": 0.99, "double": 0.96, "triple": 0.82, "home_run": 1.06},
    "MIN": {"single": 0.99, "double": 0.99, "triple": 0.98, "home_run": 1.00},
    "NYM": {"single": 0.99, "double": 0.97, "triple": 1.00, "home_run": 0.96},
    "PIT": {"single": 1.01, "double": 1.01, "triple": 1.30, "home_run": 0.88},
    "WSH": {"single": 1.00, "double": 0.99, "triple": 0.94, "home_run": 1.00},
    "WAS": {"single": 1.00, "double": 0.99, "triple": 0.94, "home_run": 1.00},
    "ARI": {"single": 1.02, "double": 1.03, "triple": 1.16, "home_run": 1.02},
}

NEUTRAL_PARK: dict[str, float] = {}


def park_factor(team: str) -> dict[str, float]:
    """Multiplicative outcome adjustments for a home park."""
    return PARK_FACTORS.get(team.upper(), NEUTRAL_PARK)


# --------------------------------------------------------------------------
# Expected plate appearances by batting order slot
# --------------------------------------------------------------------------
# Mean PA for slots 1-9 in a nine-inning game at a league-average run
# environment. Used as the base rate that lineups.py scales by team total.

PA_BY_ORDER: tuple[float, ...] = (
    4.66,
    4.58,
    4.49,
    4.41,
    4.32,
    4.23,
    4.14,
    4.05,
    3.96,
)

LEAGUE_RUNS_PER_GAME = 4.45
