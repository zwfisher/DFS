"""DraftKings bulk-entry CSV.

A lineup is only useful if it can be entered, and DraftKings' upload form
is picky in two ways that are easy to miss until it rejects the file:

**The identifier is the draftable id, not the player id.** The draftables
feed carries three ids per row -- ``playerId``, ``playerDkId`` and
``draftableId`` -- and only the last one is accepted. Worse, a
multi-position player has a *different* draftable id for each roster slot
he is eligible at, so the id depends on where you play him. Exporting
``playerId`` produces a file that looks right and imports as nothing.

**The columns are roster slots, in order.** ``P,P,C,1B,2B,3B,SS,OF,OF,OF``.
The optimizer returns a set of ten players, not an assignment, so the
players have to be matched to slots first -- and with multi-position
eligibility that is a bipartite matching, not a sort.
"""

from __future__ import annotations

import pandas as pd

from ..config import ROSTER

# DraftKings' internal roster slot ids for MLB Classic.
ROSTER_SLOT_IDS = {110: "P", 111: "C", 112: "1B", 113: "2B", 114: "3B",
                   115: "SS", 116: "OF"}

UPLOAD_COLUMNS = ["P", "P", "C", "1B", "2B", "3B", "SS", "OF", "OF", "OF"]


def draftable_slot_ids(draft_group_id: int) -> dict[tuple[str, str], int]:
    """``(player id, slot) -> draftable id`` for one draft group.

    Read from the raw feed rather than the collapsed salary frame, because
    collapsing is exactly what throws away the per-slot ids.
    """
    from .lobby import _get_json

    url = (
        "https://api.draftkings.com/draftgroups/v1/draftgroups/"
        f"{draft_group_id}/draftables"
    )
    payload = _get_json(url)
    out: dict[tuple[str, str], int] = {}
    for row in payload.get("draftables", []):
        slot = ROSTER_SLOT_IDS.get(row.get("rosterSlotId"))
        if slot is None:
            continue
        out[(str(row.get("playerId")), slot)] = int(row["draftableId"])
    return out


def assign_slots(
    positions: dict[str, tuple[str, ...]],
    slots: list[str] | None = None,
) -> dict[str, str] | None:
    """Match players to roster slots, or None if the lineup cannot fill one.

    Backtracking over the scarcest slot first. Ten players and ten slots
    make this trivially small, and being exact matters more than being
    clever: a greedy pass that puts a 1B/OF at first base can strand the
    only remaining outfielder.
    """
    slots = list(slots if slots is not None else UPLOAD_COLUMNS)
    assignment: dict[str, str] = {}
    remaining = dict(positions)

    def eligible(slot: str) -> list[str]:
        return [p for p, pos in remaining.items() if slot in pos]

    def solve(open_slots: list[str]) -> bool:
        if not open_slots:
            return True
        # Fewest candidates first: it fails fast and keeps the search tiny.
        slot = min(open_slots, key=lambda s: len(eligible(s)))
        rest = list(open_slots)
        rest.remove(slot)
        for player in eligible(slot):
            assignment[player] = slot
            saved = remaining.pop(player)
            if solve(rest):
                return True
            remaining[player] = saved
            del assignment[player]
        return False

    return assignment if solve(slots) else None


def upload_frame(
    lineups: list[list[str]],
    slate,
    slot_ids: dict[tuple[str, str], int],
) -> pd.DataFrame:
    """DraftKings-format entries, one row per lineup.

    Raises rather than writing a partial file: a silently dropped lineup is
    worse than an error, because the file still imports and you enter fewer
    lineups than you meant to.
    """
    rows = []
    for n, player_ids in enumerate(lineups, start=1):
        positions = {pid: tuple(slate.player(pid).positions) for pid in player_ids}
        assignment = assign_slots(positions)
        if assignment is None:
            raise ValueError(
                f"lineup {n} cannot be assigned to DraftKings roster slots: "
                + ", ".join(f"{slate.player(p).name} {positions[p]}"
                            for p in player_ids)
            )

        by_slot: dict[str, list[str]] = {}
        for pid, slot in assignment.items():
            by_slot.setdefault(slot, []).append(pid)

        row, used = {}, set()
        for column in UPLOAD_COLUMNS:
            pid = by_slot[column].pop()
            key = (pid, column)
            if key not in slot_ids:
                raise ValueError(
                    f"no draftable id for {slate.player(pid).name} at {column}"
                )
            # Duplicate column names -- build positionally, name after.
            row[len(row)] = slot_ids[key]
            used.add(pid)
        rows.append(row)

    frame = pd.DataFrame(rows)
    frame.columns = UPLOAD_COLUMNS
    return frame


def write_upload_csv(path, lineups, slate, draft_group_id: int) -> int:
    """Write a DraftKings bulk-entry file. Returns the number of lineups."""
    slot_ids = draftable_slot_ids(draft_group_id)
    frame = upload_frame(lineups, slate, slot_ids)
    frame.to_csv(path, index=False)
    return len(frame)
