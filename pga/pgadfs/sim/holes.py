"""The hole outcome model.

DraftKings golf is not scored on strokes. A birdie is +3 and a par is +0.5,
so two rounds of 68 can differ by six fantasy points depending on how they
were made -- and the bonuses (three birdies in a row, a bogey-free round)
are properties of the *sequence* of holes, not of the total. Anything that
models a golfer's DK score as a distribution over round totals has to bolt
those on with a fudge factor. Simulating the eighteen holes gets them for
free, correlated with the score the way they actually are.

The model is a proportional-odds (cumulative logit) categorical over

    {eagle or better, birdie, par, bogey, double bogey or worse}

with one set of cutpoints per par, a difficulty offset per hole, and a
single latent shift per golfer per round. Proportional odds is the right
shape here for a reason that is easy to check against tour data: a better
golfer does not just convert bogeys into pars uniformly, they make more
birdies *and* fewer doubles, and the effect on the tails is bigger than on
the middle. A shift on the latent scale does exactly that.
"""

from __future__ import annotations

import numpy as np

from ..config import (
    BASELINE_HOLE_PROBS,
    DOUBLE_EAGLE_SHARE_PAR5,
    DOUBLE_PLUS_STROKES,
    HOLE_IN_ONE_BONUS,
    HOLE_POINTS,
    Course,
)

# Category indices, best to worst.
EAGLE_PLUS, BIRDIE, PAR, BOGEY, DOUBLE_PLUS = range(5)
N_CATEGORIES = 5

PARS = (3, 4, 5)

_DOUBLE_PLUS_MEAN = sum(s * w for s, w in DOUBLE_PLUS_STROKES)


def cutpoints(par: int) -> np.ndarray:
    """The four cumulative-logit thresholds for a neutral hole of this par.

    tau[k] is the log-odds that a field-average golfer scores in category k
    or better, so P(category <= k) = sigmoid(tau[k] + z).
    """
    probs = np.asarray(BASELINE_HOLE_PROBS[par], dtype=float)
    probs = probs / probs.sum()
    cum = np.cumsum(probs)[:-1]
    return np.log(cum / (1.0 - cum))


def strokes_by_category(par: int) -> np.ndarray:
    """Expected strokes over par for each category on a hole of this par."""
    eagle = -2.0
    if par == 5:
        # An albatross is a 2 on a par 5: one better than an eagle.
        eagle = -2.0 - DOUBLE_EAGLE_SHARE_PAR5
    return np.array([eagle, -1.0, 0.0, 1.0, _DOUBLE_PLUS_MEAN])


def points_by_category(par: int) -> np.ndarray:
    """DraftKings points for each category on a hole of this par.

    Par 3 folds in the ace bonus: the only way to be under two on a par 3 is
    to hole the tee shot. Par 5 blends in the albatross rate.
    """
    if par == 3:
        eagle = HOLE_POINTS[-2] + HOLE_IN_ONE_BONUS
    elif par == 5:
        share = DOUBLE_EAGLE_SHARE_PAR5
        eagle = (1 - share) * HOLE_POINTS[-2] + share * HOLE_POINTS[-3]
    else:
        eagle = HOLE_POINTS[-2]
    return np.array([eagle, HOLE_POINTS[-1], HOLE_POINTS[0], HOLE_POINTS[1], HOLE_POINTS[2]])


class HoleModel:
    """Per-hole cutpoints for a course, ready to be shifted and sampled.

    `latent_per_stroke` converts strokes into latent units. It is the single
    parameter that ties the golfer's talent, the hole difficulty offsets and
    the course-wide difficulty shift to the same scale, and it is solved for
    in projections.calibrate rather than assumed.
    """

    def __init__(self, course: Course, latent_per_stroke: float, course_shift: float = 0.0):
        self.course = course
        self.latent_per_stroke = float(latent_per_stroke)
        self.course_shift = float(course_shift)

        n = len(course.pars)
        self.pars = np.asarray(course.pars, dtype=np.int64)
        # Neutral cutpoints per par, and the per-hole adjustment that carries
        # this hole's difficulty and the course-wide shift. Keeping them apart
        # is what lets `sample` use one searchsorted per par type.
        self.base_tau = {par: cutpoints(par).astype(np.float32) for par in PARS}
        self.hole_adjust = np.array(
            [self.course_shift - off * self.latent_per_stroke for off in course.hole_offsets],
            dtype=np.float32,
        )
        self.holes_by_par = {
            par: np.flatnonzero(self.pars == par) for par in PARS if (self.pars == par).any()
        }
        # tau[h, k]: cutpoint k on hole h, already carrying that hole's
        # difficulty offset and the course-wide shift. Positive shift = easier.
        self.tau = np.empty((n, N_CATEGORIES - 1))
        self.strokes = np.empty((n, N_CATEGORIES))
        self.points = np.empty((n, N_CATEGORIES))
        for h, par in enumerate(course.pars):
            offset = course.hole_offsets[h] * self.latent_per_stroke
            self.tau[h] = cutpoints(par) - offset + self.course_shift
            self.strokes[h] = strokes_by_category(par)
            self.points[h] = points_by_category(par)

    # -- analytic side, used for calibration -------------------------------

    def category_probs(self, z: np.ndarray) -> np.ndarray:
        """P(category) for latent shift `z`, shape (..., n_holes, 5).

        `z` broadcasts against the hole axis, so a shape (S, P, 1) shift
        against 18 holes gives (S, P, 18, 5).
        """
        cum = _sigmoid(self.tau + z[..., None])
        probs = np.empty(cum.shape[:-1] + (N_CATEGORIES,))
        probs[..., 0] = cum[..., 0]
        probs[..., 1:-1] = np.diff(cum, axis=-1)
        probs[..., -1] = 1.0 - cum[..., -1]
        return probs

    def expected_strokes(self, edge: np.ndarray | float) -> np.ndarray:
        """Expected round score relative to par, for a talent edge in strokes.

        `edge` is strokes per round better than the model's baseline golfer.
        """
        z = np.atleast_1d(np.asarray(edge, dtype=float)) * self.latent_per_stroke / len(self.pars)
        probs = self.category_probs(z[..., None])
        return (probs * self.strokes).sum(axis=(-1, -2))

    def expected_points(self, edge: np.ndarray | float) -> np.ndarray:
        """Expected hole-scoring DK points for a round, excluding bonuses."""
        z = np.atleast_1d(np.asarray(edge, dtype=float)) * self.latent_per_stroke / len(self.pars)
        probs = self.category_probs(z[..., None])
        return (probs * self.points).sum(axis=(-1, -2))

    # -- sampling side -----------------------------------------------------

    def sample(self, z: np.ndarray, rng: np.random.Generator) -> np.ndarray:
        """Draw a category for every hole. `z` broadcasts to (..., n_holes).

        A cumulative logit is a latent logistic variable cut at the
        thresholds, so instead of evaluating four sigmoids per hole and
        comparing a uniform to each, draw the latent directly and look up
        where it falls. Holes are grouped by par -- there are only three
        distinct threshold vectors -- which turns the whole categorical draw
        into three `searchsorted` calls over sorted length-4 arrays.
        """
        n_holes = len(self.hole_adjust)
        shape = np.broadcast_shapes(np.shape(z), (n_holes,))
        latent = rng.logistic(size=shape).astype(np.float32)
        latent -= z
        latent -= self.hole_adjust
        cats = np.empty(shape, dtype=np.int8)
        for par, idx in self.holes_by_par.items():
            cats[..., idx] = np.searchsorted(
                self.base_tau[par], latent[..., idx], side="left"
            ).astype(np.int8)
        return cats


def _sigmoid(x: np.ndarray) -> np.ndarray:
    return 0.5 * (1.0 + np.tanh(0.5 * x))
