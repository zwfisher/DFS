"""The slate: one row per golfer, assembled from DraftKings and DataGolf."""

from __future__ import annotations

from dataclasses import dataclass, replace
from dataclasses import field as dataclasses_field

import numpy as np

from .data import datagolf as dg
from .data import dk
from .data.ids import NameIndex


@dataclass(frozen=True)
class Golfer:
    dk_id: int
    name: str
    salary: int
    skill: float           # true talent, strokes gained per round
    course_fit: float      # strokes per round, course-specific
    wave: int              # 0 / 1 tee wave; rounds 1 and 2 alternate
    ownership: float       # projected, percent of lineups
    dg_score_sd: float | None = None    # DataGolf's projected DK-score SD
    dg_points: float | None = None      # DataGolf's projected DK points, if visible
    dg_finish_points: float | None = None

    @property
    def talent(self) -> float:
        """Everything that shifts this golfer's expected score for the week."""
        return self.skill + self.course_fit


@dataclass(frozen=True)
class Slate:
    event: str
    course: str
    golfers: tuple[Golfer, ...]
    contest: dk.Contest | None = None
    field_score_to_par: float = -1.0
    # Golfers whose join to a DataGolf source came up empty, by source.
    warnings: dict[str, tuple[str, ...]] = dataclasses_field(default_factory=dict)

    def __len__(self) -> int:
        return len(self.golfers)

    @property
    def names(self) -> list[str]:
        return [g.name for g in self.golfers]

    @property
    def salaries(self) -> np.ndarray:
        return np.array([g.salary for g in self.golfers], dtype=np.int64)

    @property
    def talent(self) -> np.ndarray:
        return np.array([g.talent for g in self.golfers], dtype=float)

    @property
    def edge(self) -> np.ndarray:
        """Strokes per round better than the average golfer *in this field*."""
        t = self.talent
        return t - t.mean()

    @property
    def waves(self) -> np.ndarray:
        return np.array([g.wave for g in self.golfers], dtype=np.int8)

    @property
    def ownership(self) -> np.ndarray:
        return np.array([g.ownership for g in self.golfers], dtype=float)

    def index(self, name: str) -> int:
        key = dg.normalize_name(name)
        for i, g in enumerate(self.golfers):
            if dg.normalize_name(g.name) == key:
                return i
        raise KeyError(name)

    def with_ownership(self, own: np.ndarray) -> "Slate":
        golfers = tuple(replace(g, ownership=float(o)) for g, o in zip(self.golfers, own))
        return replace(self, golfers=golfers)


def build_slate(
    contest_id: int,
    course: str,
    *,
    offline: bool = False,
    slate_name: str = "DK - Main Slate",
    salaries_csv: str | None = None,
) -> Slate:
    """Join DraftKings salaries to DataGolf skill, course fit and ownership.

    The join runs on normalized names except for DataGolf's fantasy page,
    which carries DraftKings' own `draftableId` and joins exactly.
    """
    contest = dk.load_contest(contest_id, offline=offline)
    if salaries_csv:
        players = dk.load_salaries_csv(salaries_csv)
    else:
        players = dk.load_draftables(contest.draft_group, offline=offline)

    ranks = NameIndex((r.name, r) for r in dg.load_rankings(offline=offline).values())
    raw_fits, fit_meta = dg.load_course_fit(course, offline=offline)
    fits = NameIndex((f.name, f) for f in raw_fits.values())
    rows, consts = dg.load_fantasy_projections(slate_name, offline=offline)
    by_dk_id = {r.dk_id: r for r in rows}

    # A golfer with no DataGolf ranking is almost always a late qualifier with
    # a thin record. Fall back to the weakest ranked player in the field rather
    # than dropping them: they are still draftable, and pretending they do not
    # exist would leave the ownership vector short.
    ranked = [r.dg_skill for r in (ranks.get(p.name) for p in players) if r is not None]
    fallback_skill = min(ranked) if ranked else 0.0

    golfers = []
    missing_skill, missing_fit, missing_row = [], [], []
    for p in players:
        rank = ranks.get(p.name)
        fit = fits.get(p.name)
        row = by_dk_id.get(p.draftable_id)
        if rank is None:
            missing_skill.append(p.name)
        if fit is None:
            missing_fit.append(p.name)
        if row is None:
            missing_row.append(p.name)
        golfers.append(
            Golfer(
                dk_id=p.draftable_id,
                name=p.name,
                salary=p.salary,
                skill=rank.dg_skill if rank else fallback_skill,
                course_fit=fit.fit if fit else 0.0,
                wave=row.wave if row else 0,
                ownership=row.ownership if row else 0.0,
                dg_score_sd=row.score_sd if row else None,
                dg_points=row.dg_points if row else None,
                dg_finish_points=row.dg_finish_points if row else None,
            )
        )

    return Slate(
        event=str(consts.get("event_name") or fit_meta.get("event_name") or "unknown"),
        course=course,
        golfers=tuple(golfers),
        contest=contest,
        field_score_to_par=float(consts.get("predicted_score", -1.0)),
        warnings={
            "no_skill": tuple(missing_skill),
            "no_course_fit": tuple(missing_fit),
            "no_slate_row": tuple(missing_row),
        },
    )
