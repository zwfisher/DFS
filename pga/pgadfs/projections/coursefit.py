"""What kind of golfer Bellerive rewards.

Two separate questions get confused with each other constantly, so they are
kept apart here.

**Course history** is what a golfer has shot at this venue before. At
Bellerive that is four rounds of the 2018 PGA Championship for seventeen of
the fifty, eight years ago, on a course that has been re-set since. DataGolf
shrinks it to a cap of 0.16 strokes a round and the largest value in this
field is 0.032. It is included because it is free and correctly shrunk, and
it should be expected to do nothing.

**Course fit** is which skills the venue pays for, applied to the skills a
golfer actually has. That is the question worth asking, and it has a
measurable answer: Bellerive weights driving distance well above a typical
tour course and around-the-green play well below it. A golfer's overall
rating already prices what he does on an average course, so the fit is built
from the *difference* between this course's weights and the average, times
each golfer's standardized skill on each axis. That decomposition is the
point -- it says not just that a golfer fits, but which part of his game is
doing it.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np

from ..data import datagolf as dg
from ..data.ids import NameIndex

AXES = ("Driving Distance", "Driving Accuracy", "Approach", "Around Green", "Putting")

# The course-fit tool's per-golfer columns, in the same order.
_COLUMNS = ("dist", "acc", "app", "arg", "putt")


@dataclass(frozen=True)
class ArchetypeFit:
    name: str
    fit: float                       # strokes per round, course fit
    history: float                   # strokes per round, course history
    components: dict[str, float]     # per-axis contribution to `fit`, in strokes
    skills: dict[str, float]         # the golfer's standardized skill on each axis

    @property
    def total(self) -> float:
        return self.fit + self.history


@dataclass(frozen=True)
class CourseProfileSummary:
    """What the venue rewards, and by how much."""

    course: str
    weights: dict[str, float]        # this course
    average: dict[str, float]        # a typical tour course
    percentile: dict[str, float]     # where this course sits among tour courses
    emphasis: dict[str, float]       # weights minus average -- what the fit uses

    def ranked(self) -> list[tuple[str, float, float]]:
        return sorted(
            ((a, self.emphasis[a], self.percentile.get(a, float("nan"))) for a in self.weights),
            key=lambda x: -x[1],
        )


def course_profile(course: str, *, offline: bool = False) -> CourseProfileSummary:
    weights, average, percentile = dg.load_course_coefficients(course, offline=offline)
    emphasis = {a: weights[a] - average.get(a, weights[a]) for a in weights}
    return CourseProfileSummary(course, weights, average, percentile, emphasis)


def build_fits(
    names: list[str],
    course: str,
    *,
    offline: bool = False,
    use_history: bool = True,
) -> dict[str, ArchetypeFit]:
    """Course fit and course history for every golfer named.

    The fit is rescaled so its spread across the field matches DataGolf's own
    fitted `total_comp`. That keeps the magnitude honest -- DataGolf fits the
    weights properly, against results, and this does not -- while leaving the
    decomposition open to inspection. The two agree at a correlation of about
    0.87 across this field, which is what you would want from a
    reconstruction: close enough to trust, different enough to be worth
    printing.
    """
    profile = course_profile(course, offline=offline)
    raw_fits, _ = dg.load_course_fit(course, offline=offline)
    by_name = NameIndex((f.name, f) for f in raw_fits.values())

    history: dict[str, dg.CourseHistory] = {}
    if use_history:
        try:
            hist_raw, _ = dg.load_course_history(offline=offline)
            history_index = NameIndex((h.name, h) for h in hist_raw.values())
        except (KeyError, OSError):
            history_index = NameIndex(())
    else:
        history_index = NameIndex(())

    emphasis = np.array([profile.emphasis[a] for a in AXES])
    rows, present = [], []
    for name in names:
        f = by_name.get(name)
        if f is None:
            continue
        present.append(name)
        rows.append([getattr(f, col) for col in ("distance", "accuracy", "approach", "around_green", "putting")])
    if not rows:
        return {}

    z = np.asarray(rows)
    raw = z @ emphasis
    raw = raw - raw.mean()
    anchor = np.array([by_name.get(n).fit for n in present])   # type: ignore[union-attr]
    scale = (anchor.std() / raw.std()) if raw.std() > 1e-12 else 0.0
    # Contributions are measured against the field, not against zero, so that
    # they sum exactly to the fit. A golfer's components then read as "what
    # this part of his game is worth here relative to the other 49".
    contributions = (z - z.mean(axis=0)) * emphasis * scale
    fits = raw * scale

    out: dict[str, ArchetypeFit] = {}
    for i, name in enumerate(present):
        h = history_index.get(name)
        out[dg.normalize_name(name)] = ArchetypeFit(
            name=name,
            fit=float(fits[i]),
            history=float(h.adjustment) if h else 0.0,
            components={a: float(contributions[i, j]) for j, a in enumerate(AXES)},
            skills={a: float(z[i, j]) for j, a in enumerate(AXES)},
        )
    return out
