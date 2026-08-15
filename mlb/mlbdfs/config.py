"""Scoring constants, roster rules and paths.

Everything DraftKings-specific that could change lives here as a one-line
edit.

The **roster rules** below are confirmed against DraftKings' own game-type
endpoint, ``api.draftkings.com/lineups/v1/gametypes/2/rules`` (game type 2 is
MLB Classic): ten roster slots as P/P/C/1B/2B/3B/SS/OF/OF/OF, a $50,000
salary cap, at least two teams and two games, unique players, and a five
hitter per team maximum -- that last one carried in the error codes as
INVALID_TEAM_COUNT_MLB, "For MLB lineups, the max number of hitters from one
team is 5", not in the (null) teamPositionLimits field.

The **scoring point values** are still unconfirmed. DraftKings renders its
scoring table client side and exposes no JSON for it, so the numbers below
come from secondary sources. Check them against the live rules page before
trusting output: a wrong constant here silently corrupts every layer above
it, and nothing in the test suite can catch it.
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
    # Refuse to roster a hitter whose chance of starting is below this.
    #
    # Measured on real data, a projected lineup is right about 7.5 of 9, and
    # start_probability tops out near 0.94 -- there is no such thing as a
    # lock until the card is posted. Since a hitter who does not start
    # scores zero and each zero costs about 13 points, the practical control
    # is to decline the uncertain ones. At 0.80 the survivors start 90.5% of
    # the time and roughly five per team-game remain, which is enough pool
    # to build from. Irrelevant once lineups are confirmed, since everyone
    # is then at 1.0.
    min_start_probability: float = 0.0
    randomize_sigma: float = 1.0  # scale on sampled score noise for the pool
    solver_msg: bool = False
    # Stop branch and bound once within this relative gap. The objective is
    # a sampled draw, so proving optimality against it is precision the
    # input does not have.
    mip_gap: float = 0.005
    solve_time_limit: int = 10  # seconds per lineup, a backstop not a target
    solver_workers: int = 8


OPTIMIZER = OptimizerConfig()


# --------------------------------------------------------------------------
# Ownership defaults
# --------------------------------------------------------------------------


@dataclass(frozen=True)
class OwnershipConfig:
    # Utility weights, fitted by maximum likelihood against a real
    # 35,671-entry contest rather than guessed. Holding out whole position
    # groups reproduces them, so they transfer across positions -- but they
    # come from one slate, and one slate cannot show whether they transfer
    # across slates. Re-fit with `mlbdfs fit-ownership` as contests
    # accumulate.
    #
    # Against the original hand-set guesses on that contest: mean absolute
    # error 0.0222 -> 0.0115, correlation with realized ownership
    # 0.22 -> 0.81, error on the twenty chalkiest plays 0.155 -> 0.081.
    #
    # Two guesses were qualitatively wrong, not merely mis-sized:
    #
    #   salary  was -0.10 on the theory that the field is price averse. The
    #           fitted sign is strongly *positive*. The field does not hunt
    #           bargains, it pays up for good players.
    #   value   was 1.85 and dominated every other term. Fitted at almost
    #           exactly zero. Points per dollar has essentially no influence
    #           once projection and ceiling are in the model -- which is also
    #           why ownership concentration used to pile onto cheap
    #           high-value bats and push the implied field score down.
    #
    # What the field actually buys in a hitter is ceiling and price.
    w_value: float = 0.01  # projected points per $1k
    w_points: float = 0.29  # raw projected points
    # Not fitted: the backtest slate had no Vegas totals, so the feature was
    # constant and its coefficient unidentifiable. A fitted zero would be an
    # artefact rather than evidence, so the original estimate stands.
    w_team_total: float = 0.32  # Vegas implied runs
    w_ceiling: float = 0.46  # p90 of the simulated distribution
    w_order_top: float = 0.05  # batting first through fifth
    w_salary: float = 0.45  # the field pays up for good players

    # Pitchers are weighed differently enough to need their own vector:
    # ceiling swamps everything else. Read these as a set, not one at a
    # time -- a starter's projection, ceiling and value are so collinear
    # that the individual coefficients are unstable and two come out
    # negative while the combination fits well.
    wp_value: float = -0.40
    wp_points: float = -0.62
    wp_team_total: float = 0.0
    wp_ceiling: float = 2.31
    wp_salary: float = 0.06
    # Dirichlet concentration controlling ownership uncertainty. Lower is more
    # uncertain; this is the parameter to fit once real ownership data exists.
    dirichlet_concentration: float = 140.0
    # Share of the field that stacks, and the stack size distribution.
    # Expected mean DraftKings score of a field lineup. This is the knob
    # that sets how concentrated ownership is, and it is set from an
    # observable: the average score in contests you actually enter. See
    # ownership/heuristic.py for why this, and not the field sampler, is
    # what controls field strength.
    # Left unset by default: the right value is the average score in the
    # contests you actually enter, which is a property of your contest
    # selection rather than something with a sensible default. Setting it
    # turns on calibration, which costs about a dozen field generations.
    target_field_mean_score: float | None = None
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


# --------------------------------------------------------------------------
# Starting probability
# --------------------------------------------------------------------------
# A hitter who does not start scores zero, and zeros dominate finishing
# position: in a 35,671-entry contest, 60% of the top 200 entries carried no
# zero-scoring player against 10% of the field, and each additional zero cost
# about 13 points. Treating a projected starter as a certain starter throws
# that risk away entirely, so it is modelled explicitly.
#
# Before lineups post, the best available signal is salary rank within a
# team: teams carry roughly thirteen position players and start nine, and
# the expensive ones are the ones who play.

START_PROBABILITY_BY_SALARY_RANK: tuple[float, ...] = (
    0.95, 0.94, 0.93, 0.92, 0.90, 0.88, 0.84, 0.78, 0.70,
)
# Anyone outside a team's top nine by salary, when no lineup is posted.
START_PROBABILITY_FRINGE = 0.35

# Salary rank -> batting order slot, used only when no lineup is posted.
# Sorting the order by salary (highest bats first) is wrong in a specific
# way: leadoff hitters are frequently cheap contact-and-speed players while
# the expensive bats hit second through fourth. This mapping puts the top
# three salaries in the three-two-four slots, which is how most lineup cards
# are actually written.
SALARY_RANK_TO_ORDER: tuple[int, ...] = (3, 2, 4, 5, 1, 6, 7, 8, 9)

# --------------------------------------------------------------------------
# Projected lineups
# --------------------------------------------------------------------------
# How far back to look when estimating who starts, and how fast that
# evidence decays. Counted in games rather than days so an off day does not
# age a lineup.
LINEUP_LOOKBACK_GAMES = 30
# Tuned on 498 real team-games of walk-forward backtesting. Recency matters
# more than first assumed: a half life of 4.5 games beats 12 by about 6% of
# Brier score. Lineups churn faster than a season-long view suggests.
LINEUP_HALF_LIFE_GAMES = 4.5

# Strength of the shrink from a hitter's start rate against one pitcher hand
# toward his overall rate, measured in games.
#
# Set by sweeping against synthetic history with known platoon structure
# (see tools/tune_lineup_prior.py). The result is lopsided and worth
# knowing: when a team genuinely platoons, a heavy prior is five times worse
# than a light one, while when it does not platoon a heavy prior is only
# marginally better. Missing a real platoon means rostering someone who does
# not play, and the contest data puts that at about 13 points; imagining a
# platoon that is not there only mis-weights two players who both might
# start. The asymmetry says shrink lightly.
#
# The prior does affect *which* nine are projected, not only how confident
# the estimate is. Shrinkage here is toward each player's own overall rate
# rather than a shared constant, so it is not a common monotone transform
# across players: a platoon bat with a strong hand-specific record and a
# weak overall one can trade places with his counterpart as the prior moves.
# The everyday core is stable; the platoon spot is exactly what moves.
#
# Confirmed against 498 real team-games: the prior barely matters between
# 0.5 and 1.0, and gets worse above 3. The synthetic sweep held up.
LINEUP_HAND_PRIOR_STARTS = 1.0
# The same shrink applied to which slot he bats in.
LINEUP_SLOT_PRIOR_STARTS = 6.0

# Below this share of hitters having a posted batting order, the slate is
# treated as unconfirmed and the pipeline refuses to run without an explicit
# override.
MIN_CONFIRMED_LINEUP_SHARE = 0.60
