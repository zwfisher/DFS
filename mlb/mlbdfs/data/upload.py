"""DraftKings entry files.

A lineup is only useful if it can be entered. There are two shapes here and
only one of them is real:

``write_entries`` edits a **DKEntries export** -- the file DraftKings hands
you from the lineups page, one row per entry you already own, carrying its
Entry ID. This is what the upload form actually wants, and it is the only
way to *edit* existing entries. Columns are
``Entry ID, Contest Name, Contest ID, Entry Fee`` and then the roster slots,
with each cell written ``Name (draftableId)``.

``upload_frame`` writes bare roster-slot columns with no entry metadata.
That was written before the real template was in hand and it does not
import; it survives only because the slot-assignment logic underneath it is
shared and correct.

Both are picky in ways that are easy to miss until the form rejects them:

**The identifier is the draftable id, not the player id.** The draftables
feed carries three ids per row -- ``playerId``, ``playerDkId`` and
``draftableId`` -- and only the last one is accepted. Exporting ``playerId``
produces a file that looks right and imports as nothing.

**The columns are roster slots, in order.** ``P,P,C,1B,2B,3B,SS,OF,OF,OF``.
The optimizer returns a set of ten players, not an assignment, so the
players have to be matched to slots first -- and with multi-position
eligibility that is a bipartite matching, not a sort.

**One id per player, not one per slot.** This module used to emit the
draftable id belonging to the *slot* a player was assigned to, on the
reasoning that the feed carries a separate row per eligible slot -- Shohei
Ohtani is 43854283 at first base and 43854284 in the outfield. That is true
of the feed and false of the upload form. DraftKings' own ``DKSalaries.csv``
template lists exactly one id per player and tells you to "paste the ID into
the roster position desired"; checked against draft group 152195, all 61
multi-slot players are listed under the *lowest* of their draftable ids and
the per-slot alternate appears nowhere in the template. So the alternate id
is not an identifier the entry form ever offers, and a file built from
per-slot ids risks being rejected on exactly the flexible players a stacked
lineup leans on. ``canonical_draftable_ids`` returns the one the template
uses.
"""

from __future__ import annotations

import csv
from pathlib import Path

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


def canonical_draftable_ids(draft_group_id: int) -> dict[str, int]:
    """``player id -> the single draftable id the entry form expects``.

    The lowest of a player's draftable ids. That is what DraftKings' own
    salary export lists, verified against every multi-slot player in draft
    group 152195, and it is the id a user pastes into any slot the player is
    eligible for.
    """
    from .lobby import _get_json

    url = (
        "https://api.draftkings.com/draftgroups/v1/draftgroups/"
        f"{draft_group_id}/draftables"
    )
    out: dict[str, int] = {}
    for row in _get_json(url).get("draftables", []):
        if ROSTER_SLOT_IDS.get(row.get("rosterSlotId")) is None:
            continue
        key = str(row.get("playerId"))
        draftable = int(row["draftableId"])
        if key not in out or draftable < out[key]:
            out[key] = draftable
    return out


def read_template_ids(path) -> dict[int, dict[str, str]]:
    """``draftable id -> row`` from a DraftKings ``DKSalaries.csv``.

    The template puts the entry grid in the first ten columns and the player
    list off to the right, starting at the ``Position`` header. Parsed by
    locating that header rather than by a fixed offset, since the block of
    instructions above it is prose and free to change length.
    """
    rows = list(csv.reader(open(path, newline="", encoding="utf-8-sig")))
    header_at = None
    for i, row in enumerate(rows):
        if "Position" in row and "Name + ID" in row:
            header_at = i
            break
    if header_at is None:
        raise ValueError(
            f"{path} does not look like a DKSalaries template: no player-list "
            "header row containing 'Position' and 'Name + ID'"
        )
    header = rows[header_at]
    start = header.index("Position")
    names = header[start:]
    out: dict[int, dict[str, str]] = {}
    for row in rows[header_at + 1:]:
        if len(row) <= start or not row[start]:
            continue
        record = dict(zip(names, row[start:]))
        out[int(record["ID"])] = record
    return out


def fill_template(template_path, out_path, lineups, slate, ids) -> int:
    """Write the user's own template back with the entry grid filled in.

    The template is returned byte-for-byte apart from the first ten columns
    of the lineup rows, which is the safest thing to hand someone: every id
    written is one the file itself already lists, so there is no question of
    which id space the form wants.
    """
    frame = upload_frame(lineups, slate, ids)
    known = read_template_ids(template_path)
    unknown = sorted({int(v) for v in frame.to_numpy().ravel()} - set(known))
    if unknown:
        raise ValueError(
            f"{len(unknown)} draftable id(s) are not in {Path(template_path).name} "
            f"-- e.g. {unknown[:5]}. The template is probably for a different "
            "slate than the one these lineups were built on."
        )

    rows = list(csv.reader(open(template_path, newline="", encoding="utf-8-sig")))
    n_slots = len(UPLOAD_COLUMNS)
    for n, entry in enumerate(frame.to_numpy().tolist(), start=1):
        while n >= len(rows):
            rows.append([""] * n_slots)
        # Pad only far enough to hold the grid; anything already to the right
        # of it -- the prose, the player list -- is left exactly as it was.
        row = rows[n]
        if len(row) < n_slots:
            row = row + [""] * (n_slots - len(row))
        row[:n_slots] = [str(int(v)) for v in entry]
        rows[n] = row

    with open(out_path, "w", newline="", encoding="utf-8") as handle:
        csv.writer(handle, lineterminator="\r\n").writerows(rows)
    return len(frame)


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
    ids: dict[str, int],
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

        row = {}
        for column in UPLOAD_COLUMNS:
            pid = by_slot[column].pop()
            if pid not in ids:
                raise ValueError(
                    f"no draftable id for {slate.player(pid).name}"
                )
            # The slot decides the column; the id does not depend on it.
            # Duplicate column names -- build positionally, name after.
            row[len(row)] = ids[pid]
        rows.append(row)

    frame = pd.DataFrame(rows)
    frame.columns = UPLOAD_COLUMNS
    return frame


def write_upload_csv(
    path, lineups, slate, draft_group_id: int, template=None
) -> int:
    """Write a DraftKings bulk-entry file. Returns the number of lineups.

    With ``template`` pointing at a ``DKSalaries.csv`` downloaded for this
    slate, the ids are checked against it and the template itself is filled
    in and written out, which is the form the entry page accepts directly.
    """
    ids = canonical_draftable_ids(draft_group_id)
    if template is not None:
        return fill_template(template, path, lineups, slate, ids)
    frame = upload_frame(lineups, slate, ids)
    frame.to_csv(path, index=False)
    return len(frame)


ENTRY_COLUMNS = ["Entry ID", "Contest Name", "Contest ID", "Entry Fee"]


def read_entries(path) -> pd.DataFrame:
    """The entry rows out of a DKEntries export.

    The export is two files stapled together: your entries on the left, and
    a copy of the whole player pool starting around column P. Only the rows
    with an Entry ID are entries; everything below is the player list and a
    block of instructions, and reading it as a rectangle picks up hundreds
    of blank rows.

    Note the file is not a rectangle: the entry header is 16 columns wide
    and the player-pool rows below run to 24, so pandas cannot read it
    directly. Rows are taken with the csv module and truncated to the
    columns that belong to an entry.
    """
    import csv

    width = len(ENTRY_COLUMNS + UPLOAD_COLUMNS)
    rows = []
    with open(path, newline="", encoding="utf-8-sig") as handle:
        for row in csv.reader(handle):
            if not row or not row[0].strip():
                continue
            if row[0].strip() == ENTRY_COLUMNS[0]:  # the header
                continue
            padded = (row + [""] * width)[:width]
            rows.append(padded)
    return pd.DataFrame(rows, columns=ENTRY_COLUMNS + UPLOAD_COLUMNS)


def write_entries(
    path,
    entries: pd.DataFrame,
    lineups: list[list[str]],
    slate,
    ids: dict[str, int],
) -> pd.DataFrame:
    """Rewrite an entry export's rosters, keeping each row's Entry ID.

    One lineup per entry, in order. DraftKings matches on Entry ID, so the
    metadata columns are carried through untouched -- lose them and the
    upload creates nothing and edits nothing.

    Only changed entries need to be in the uploaded file, so the player-pool
    block from the original export is dropped. That is DraftKings' own
    advice and it makes the file readable.

    ``ids`` comes from :func:`canonical_draftable_ids` -- one id per player,
    not one per slot.
    """
    if len(lineups) != len(entries):
        raise ValueError(
            f"{len(lineups)} lineups for {len(entries)} entries; "
            "they have to correspond one to one"
        )

    rows = []
    for (_, entry), player_ids in zip(entries.iterrows(), lineups):
        positions = {pid: tuple(slate.player(pid).positions) for pid in player_ids}
        assignment = assign_slots(positions)
        if assignment is None:
            raise ValueError(
                f"entry {entry['Entry ID']} cannot fill the roster slots: "
                + ", ".join(f"{slate.player(p).name} {positions[p]}"
                            for p in player_ids)
            )
        by_slot: dict[str, list[str]] = {}
        for pid, slot in assignment.items():
            by_slot.setdefault(slot, []).append(pid)

        row = [entry[c] for c in ENTRY_COLUMNS]
        for column in UPLOAD_COLUMNS:
            pid = by_slot[column].pop()
            if pid not in ids:
                raise ValueError(
                    f"no draftable id for {slate.player(pid).name}"
                )
            # One id per player, never the per-slot alternate: the slot
            # decides the column, not the id. See the module docstring --
            # writing the alternate produces a file the form rejects, and it
            # is exactly the mistake this signature now makes impossible.
            row.append(f"{slate.player(pid).name} ({ids[pid]})")
        rows.append(row)

    out = pd.DataFrame(rows, columns=ENTRY_COLUMNS + UPLOAD_COLUMNS)
    out.to_csv(path, index=False)
    return out
