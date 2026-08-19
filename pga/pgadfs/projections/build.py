"""The projection table.

One row per golfer: what the simulator says they will score, how often they
win, and what that is worth against the salary they cost.
"""

from __future__ import annotations

import numpy as np
import pandas as pd

from ..sim.engine import SimResult
from ..slate import Slate


def projection_table(
    slate: Slate,
    sim: SimResult,
    ownership: np.ndarray | None = None,
    raw_ownership: np.ndarray | None = None,
) -> pd.DataFrame:
    points = sim.points
    df = pd.DataFrame(
        {
            "golfer": list(sim.names),
            "salary": slate.salaries,
            "talent": slate.talent,
            "edge": slate.edge,
            "course_fit": [g.course_fit for g in slate.golfers],
            "proj": points.mean(axis=0),
            "sd": points.std(axis=0),
            "floor": np.percentile(points, 20, axis=0),
            "ceiling": np.percentile(points, 90, axis=0),
            "win": (sim.position == 1).mean(axis=0),
            "top5": (sim.position <= 5).mean(axis=0),
            "top10": (sim.position <= 10).mean(axis=0),
            "top20": (sim.position <= 20).mean(axis=0),
            "strokes": sim.strokes.mean(axis=0),
            "birdies": sim.birdies.mean(axis=0),
            "finish_pts": sim.finish_points.mean(axis=0),
            "bonus_pts": sim.bonus_points.mean(axis=0),
            "dg_proj": [g.dg_points for g in slate.golfers],
            "dg_sd": [g.dg_score_sd for g in slate.golfers],
        }
    )
    df["value"] = df["proj"] / (df["salary"] / 1000.0)
    if raw_ownership is not None:
        df["own_dg"] = raw_ownership * 100.0
    if ownership is not None:
        df["own"] = ownership * 100.0
        # Leverage is the whole reason to project ownership: a golfer is worth
        # entering not when he is good but when he is better than the field
        # thinks. Expressed as the ratio of the share of a lineup he deserves
        # to the share the field is giving him.
        deserved = df["top20"] / df["top20"].sum() * 6.0
        df["leverage"] = deserved / (ownership + 1e-9)
    return df.sort_values("proj", ascending=False).reset_index(drop=True)
