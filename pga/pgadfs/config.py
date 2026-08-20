"""Scoring constants, roster rules and every model tunable.

DraftKings PGA Classic scoring, the Bellerive setup and the handful of
parameters the simulator is calibrated on all live here so there is exactly
one place to check a number against the live rules page.
"""

from __future__ import annotations

from dataclasses import dataclass, field

# --------------------------------------------------------------------------
# DraftKings PGA Classic scoring
# --------------------------------------------------------------------------
# Verified against two independent secondary sources; DraftKings' own rules
# page is a client-rendered SPA and could not be scraped. Re-check before
# trusting the output with money: https://www.draftkings.com/help/rules/13/6
#
# Hole scoring is keyed on strokes relative to par, clamped to [-3, +2]:
#   -3 double eagle or better, -2 eagle, -1 birdie, 0 par, +1 bogey,
#   +2 double bogey *or worse*.
HOLE_POINTS: dict[int, float] = {
    -3: 20.0,   # double eagle (albatross)
    -2: 8.0,    # eagle
    -1: 3.0,    # birdie
    0: 0.5,     # par
    1: -0.5,    # bogey
    2: -1.0,    # double bogey or worse
}

STREAK_BONUS = 3.0          # 3+ consecutive birdies or better, max 1 per round
BOGEY_FREE_BONUS = 3.0      # no bogey or worse in a round
ALL_ROUNDS_UNDER_70 = 5.0   # every round of the tournament strictly under 70
HOLE_IN_ONE_BONUS = 10.0

STREAK_LENGTH = 3
UNDER_70_THRESHOLD = 70     # strokes, not relative to par

# Tournament finish points. Index 0 is a win.
FINISH_POINTS: list[float] = (
    [30.0, 20.0, 18.0, 16.0, 14.0, 12.0, 10.0, 9.0, 8.0, 7.0]
    + [6.0] * 5      # 11-15
    + [5.0] * 5      # 16-20
    + [4.0] * 5      # 21-25
    + [3.0] * 5      # 26-30
    + [2.0] * 10     # 31-40
    + [1.0] * 10     # 41-50
)

# How DraftKings resolves a tie on the leaderboard. "best" gives every player
# in a tie the points for the highest position it spans (a T3 among three
# players is 18 apiece); "average" splits the band, (18+16+14)/3. DraftKings
# does not document this and it moves projections by around a point at the
# top of the board, so it is resolved empirically in projections.calibrate
# against DataGolf's published expected finish points.
TIE_RULE = "best"

# --------------------------------------------------------------------------
# Roster rules (api.draftkings.com/lineups/v1/gametypes/6/rules)
# --------------------------------------------------------------------------
ROSTER_SIZE = 6
SALARY_CAP = 50_000
ALLOW_LATE_SWAP = False

# --------------------------------------------------------------------------
# Course
# --------------------------------------------------------------------------


@dataclass(frozen=True)
class Course:
    """A course as the simulator needs it: a par for every hole, plus a
    difficulty offset per hole in strokes (positive = plays harder than a
    typical hole of that par)."""

    name: str
    pars: tuple[int, ...]
    hole_offsets: tuple[float, ...]

    @property
    def par(self) -> int:
        return sum(self.pars)

    def __post_init__(self) -> None:
        if len(self.pars) != len(self.hole_offsets):
            raise ValueError("pars and hole_offsets must be the same length")


# Bellerive Country Club, St. Louis. Par 70, 7,448 yards: four par 3s, two
# par 5s (8 and 17), twelve par 4s, as played for the 2018 PGA Championship.
#
# There is no recent hole-by-hole scoring data for Bellerive -- the PGA Tour
# has not been here since 2018 -- so hole_offsets is a *shape*, not a
# measurement: a plausible spread of hard and easy holes that sums to zero.
# The spread matters only through the variance it adds to a round; the level
# is set by course-difficulty calibration, which is anchored on the field
# scoring average. Swap in real hole averages once the event is live.
BELLERIVE = Course(
    name="Bellerive CC",
    pars=(4, 4, 4, 3, 4, 4, 4, 5, 3, 4, 4, 3, 4, 4, 4, 4, 5, 3),
    hole_offsets=(
        0.02, -0.04, 0.08, 0.05, -0.06, 0.03, 0.10, -0.08, 0.04,
        0.01, 0.06, -0.03, -0.05, 0.07, 0.02, 0.09, -0.12, -0.19,
    ),
)

# --------------------------------------------------------------------------
# Hole outcome model
# --------------------------------------------------------------------------
# Baseline probabilities over {eagle or better, birdie, par, bogey, double+}
# for a field-average player on a neutral hole of each par, from PGA Tour
# aggregate hole scoring. The cumulative-logit cutpoints in sim.holes are
# derived from these, so these are the numbers to argue with.
BASELINE_HOLE_PROBS: dict[int, tuple[float, float, float, float, float]] = {
    #      eagle+   birdie    par      bogey   double+
    3: (0.0004, 0.1250, 0.6802, 0.1720, 0.0224),
    4: (0.0009, 0.1560, 0.6589, 0.1620, 0.0222),
    5: (0.0460, 0.3900, 0.4600, 0.0890, 0.0150),
}

# On a par 3, "eagle or better" *is* a hole in one, so the ace rate is not a
# separate parameter -- it is the par-3 eagle bucket above, set to the tour
# rate of roughly one ace per 2,500 par-3 attempts. That bucket therefore
# scores an eagle (+8) and the ace bonus (+10) together.
#
# Share of "eagle or better" on a par 5 that is actually a double eagle.
DOUBLE_EAGLE_SHARE_PAR5 = 0.004

# Strokes over par actually taken when a hole lands in the "double bogey or
# worse" bucket. DraftKings caps the *points* at -1 but the leaderboard does
# not cap the strokes, and finish position is most of a golfer's DK score.
DOUBLE_PLUS_STROKES = ((2, 0.80), (3, 0.15), (4, 0.05))


@dataclass
class ConditionsConfig:
    """How weather turns into strokes.

    The defaults are deliberately conservative. Nothing here is fitted --
    there is no per-event conditions data to fit against -- so each
    coefficient is a documented assumption, and `scale` exists to turn the
    whole layer down or off in one place.
    """

    # Wind does very little to scoring until it has something to work with.
    # Below roughly eight miles an hour the effect is inside the noise of
    # everything else; above it, tour scoring averages rise at something like
    # a twentieth of a stroke per mile per hour.
    wind_threshold: float = 8.0
    wind_coef: float = 0.055

    # Firmness. `firmness_spread` is how many strokes separate the softest
    # and firmest tee time on a day that dries by `firmness_reference` units
    # of the temperature-times-dryness index in `weather.Conditions.drying`.
    # The 0.30 is DataGolf's own published wave constant for this event,
    # which is also roughly where the literature on morning/afternoon splits
    # sits on a calm day.
    firmness_spread: float = 0.30
    firmness_reference: float = 15.0

    # Rain softens greens, and receptive greens score better -- up to the
    # point where it is bad enough to matter for other reasons. At the
    # single-digit probabilities forecast here this term does nothing; it is
    # in place for a wet week.
    rain_coef: float = -0.25

    # The forecast is not the weather. `forecast_error_sd` scales the whole
    # conditions curve up or down per round: if the course firms up harder
    # than forecast it does so for everyone at once, which is a correlation
    # an independent per-golfer error would miss. `round_shock_sd` is the
    # part that hits the whole field equally.
    forecast_error_sd: float = 0.40
    round_shock_sd: float = 0.25

    # How long a twosome takes to play eighteen holes, which sets how much of
    # the day's drying each group is exposed to.
    round_hours: float = 4.25

    # Minutes between tee times, and how many golfers go off together.
    tee_interval_minutes: float = 11.0
    group_size: int = 2

    # Rounds three and four are re-paired off the leaderboard and go off
    # later in the day for television. The exact hour is an assumption; with
    # a fifty-man field in twosomes it barely moves the answer, because the
    # whole weekend tee sheet fits inside four and a half hours.
    weekend_first_tee: str = "10:30"

    # One knob to turn the layer down. 0.0 reproduces a weatherless model.
    scale: float = 1.0


@dataclass
class SimConfig:
    """Everything the tournament simulator is free to be wrong about."""

    course: Course = BELLERIVE
    rounds: int = 4

    # Field scoring average relative to par, per round. DataGolf's model
    # publishes this for the event on the fantasy-projections page
    # ("predicted_score"); the simulator solves for a global difficulty
    # shift that reproduces it given the actual field.
    field_score_to_par: float = -1.0

    # Strokes-per-round talent gets converted into a shift of the hole-level
    # latent variable. talent_scale is solved for so that a player who is
    # x strokes per round better than the field actually averages x strokes
    # better in simulation; see projections.calibrate.fit_talent_scale.
    talent_scale: float = 0.0

    # Variance decomposition of a player's scoring, in strokes per round.
    #   week_sd   persistent form/fit for the week, shared by all 4 rounds
    #   round_sd  round-to-round noise on top of the hole-level multinomial
    # week_sd is what determines who wins, so it is fitted against market and
    # DataGolf finish probabilities rather than guessed. The third source of
    # shared variance -- the weather -- lives in ConditionsConfig, because it
    # has a known structure rather than being noise.
    week_sd: float = 0.90
    round_sd: float = 1.05

    conditions: "ConditionsConfig" = field(default_factory=lambda: ConditionsConfig())

    # How DraftKings splits a leaderboard tie; see TIE_RULE above. Resolved
    # empirically by projections.calibrate and threaded through from there.
    tie_rule: str = TIE_RULE

    n_sims: int = 20_000
    seed: int = 20260820


# --------------------------------------------------------------------------
# Ownership and field
# --------------------------------------------------------------------------


@dataclass
class OwnershipConfig:
    """Ownership is projected, then perturbed.

    DataGolf publishes projected ownership for every golfer on the slate, so
    the level does not have to be modelled from scratch. What does have to be
    modelled is the uncertainty, because a tournament is decided by which
    lineups the field actually built, not by the ownership point estimate.
    """

    # Dirichlet concentration on the normalised ownership vector. Lower =
    # more disagreement between the projection and what the field does.
    concentration: float = 220.0

    # Conditional-logit fallback coefficients, on projected points and salary
    # in thousands. Only used for players with no published ownership.
    points_coef: float = 0.177
    salary_coef: float = -0.591

    # What a real entry spends. DraftKings golf fields leave very little on
    # the table, and the number matters because it is also the constraint
    # that published ownership projections have to be made consistent with.
    target_lineup_spend: int = 49_400

    seed: int = 20260820


@dataclass
class FieldConfig:
    """The opponent field in the contest being entered."""

    entries: int = 35_294
    entry_fee: float = 25.0
    max_entries_per_user: int = 150

    # Opponent lineups are sampled under ownership, then rejection-filtered
    # to obey the salary cap. min_salary_used keeps the field from drafting
    # lineups no human would submit.
    min_salary_used: int = 47_500

    # The field is built at full contest size. A smaller sample would get the
    # median entry right and the top of the leaderboard badly wrong, and the
    # top is where the money is.
    n_field_lineups: int | None = None      # None = the contest's entry count

    # Not everybody in a $25 tournament builds lineups at random out of the
    # golfers they like. This is the share of entries that behave like
    # optimizer output -- same ownership, best combination of it -- and how
    # hard those entrants optimise. It is the single biggest lever on how
    # strong the simulated field is, and therefore on the level (not the
    # ranking) of every ROI this package reports.
    sharp_share: float = 0.35
    sharp_pool: int = 6

    # Blocks of the field are drawn under different samples of the ownership
    # vector, so ownership uncertainty propagates into the field rather than
    # being averaged away before it can matter.
    ownership_draws: int = 8
    seed: int = 20260821


@dataclass
class PortfolioConfig:
    """How the entered lineups are chosen out of the candidate pool."""

    n_lineups: int = 20
    n_candidates: int = 400
    max_exposure: float = 0.60      # share of entered lineups one golfer may appear in
    max_overlap: int = 4            # golfers two entered lineups may share
    min_salary_used: int = 49_000

    # Ranking every candidate against every field lineup in every simulation
    # is the one genuinely large computation here, so the ROI stage runs on a
    # subsample of simulations. The ranking is an average over a smooth
    # payout curve, which converges much faster than the tail probabilities
    # the full simulation is for.
    roi_sims: int = 4_000

    # Candidates are built on one block of simulations, cut down to a
    # shortlist on a second, and finally chosen on a third. Two blocks are
    # not enough: picking the best of several hundred candidates on the same
    # draws that price them is a selection step, and it inflates whatever it
    # selects. `shortlist_factor` sets how many survive the middle stage per
    # lineup entered.
    shortlist_factor: int = 8

    # Width of the multiplicative window the payout curve is smoothed over
    # before lineups are ranked on it; see Payouts.smoothed. 1.0 disables it.
    payout_smoothing: float = 2.0
    seed: int = 20260822


@dataclass
class Config:
    sim: SimConfig = field(default_factory=SimConfig)
    ownership: OwnershipConfig = field(default_factory=OwnershipConfig)
    contest: FieldConfig = field(default_factory=FieldConfig)
    portfolio: PortfolioConfig = field(default_factory=PortfolioConfig)
