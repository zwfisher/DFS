"""Real DraftKings golf scoring, for checking the model against.

DataGolf's historical DFS archive is a paid feed, but it serves one event
free as a sample, and that sample is unusually well suited to validating a
model like this one: it does not just give the total, it gives DraftKings'
own decomposition into hole scoring, each bonus, and the finish component,
next to the leaderboard position that produced it.

Two things fall straight out of it. The finish-points table can be checked
against real output rather than against a secondary source, and the tie rule
DraftKings does not document can simply be read off: a T2 is worth 20, which
is the points for second, not the average of second and third.
"""

from __future__ import annotations

import csv
import re
from dataclasses import dataclass
from pathlib import Path

FIXTURE = Path(__file__).parent / "fixtures" / "datagolf_dk_points_2021_pga.csv"

_NOT_A_FINISH = {"CUT", "WD", "DQ", "MDF", "DNS", ""}


@dataclass(frozen=True)
class ScoredRound:
    player: str
    finish_text: str
    position: int | None      # None for CUT/WD/DQ
    tied: bool
    streak_pts: float
    bogey_free_pts: float
    hole_in_one_pts: float
    sub_70_pts: float
    hole_score_pts: float
    finish_pts: float
    total_pts: float


def _position(text: str) -> tuple[int | None, bool]:
    text = (text or "").strip().upper()
    if text in _NOT_A_FINISH:
        return None, False
    match = re.fullmatch(r"(T?)(\d+)", text)
    if not match:
        return None, False
    return int(match.group(2)), match.group(1) == "T"


def load_sample(path: str | Path = FIXTURE) -> list[ScoredRound]:
    out = []
    with open(path, newline="", encoding="utf-8") as fh:
        for row in csv.DictReader(fh):
            position, tied = _position(row["fin_text"])
            out.append(
                ScoredRound(
                    player=row["player_name"],
                    finish_text=row["fin_text"],
                    position=position,
                    tied=tied,
                    streak_pts=float(row["streak_pts"]),
                    bogey_free_pts=float(row["bogey_free_pts"]),
                    hole_in_one_pts=float(row["hole_in_one_pts"]),
                    sub_70_pts=float(row["sub_70_pts"]),
                    hole_score_pts=float(row["hole_score_pts"]),
                    finish_pts=float(row["finish_pts"]),
                    total_pts=float(row["total_pts"]),
                )
            )
    return out


def observed_bonus_rates(rounds: int = 4) -> dict[str, float]:
    """Per-round bonus frequencies among players who went the distance.

    The sample is a major on a hard course, so these are at the low end of
    what to expect -- but they are measured, which is more than can be said
    for the alternatives.
    """
    from ..config import BOGEY_FREE_BONUS, STREAK_BONUS

    played = [r for r in load_sample() if r.position is not None]
    n = len(played) * rounds
    return {
        "rounds with a 3-birdie streak": sum(r.streak_pts for r in played) / STREAK_BONUS / n,
        "bogey-free rounds": sum(r.bogey_free_pts for r in played) / BOGEY_FREE_BONUS / n,
        "hole scoring points per round": sum(r.hole_score_pts for r in played) / n,
    }
