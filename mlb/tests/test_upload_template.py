"""Filling DraftKings' own DKSalaries.csv entry template.

The template is the artefact that settles which id space the entry form
wants, and it disagrees with the draftables feed: one id per player, not one
per eligible slot.
"""

from __future__ import annotations

import csv
from types import SimpleNamespace

import pytest

from mlbdfs.data.upload import (
    UPLOAD_COLUMNS,
    fill_template,
    read_template_ids,
)

NAMES = ["p1", "p2", "c", "flex", "second", "third", "short",
         "of1", "of2", "of3"]
POSITIONS = {
    "p1": ("P",), "p2": ("P",), "c": ("C",), "flex": ("1B", "OF"),
    "second": ("2B",), "third": ("3B",), "short": ("SS",),
    "of1": ("OF",), "of2": ("OF",), "of3": ("OF",),
}
IDS = {name: 43854000 + i for i, name in enumerate(NAMES)}


class FakeSlate:
    def player(self, pid):
        return SimpleNamespace(positions=POSITIONS[pid], name=pid)


def write_template(path, ids=IDS, trailing_note=True):
    """A DKSalaries.csv shaped like the real one.

    Ten entry columns, a blank, then prose, then the player list off to the
    right. The prose block is deliberately an awkward length so the parser
    cannot get away with a fixed offset.
    """
    rows = [["P", "P", "C", "1B", "2B", "3B", "SS", "OF", "OF", "OF", "",
             "Instructions"]]
    for line in ("1. Locate the player", "2. Copy the ID", "3. Paste the ID"):
        rows.append([""] * 11 + [line])
    rows.append([" "])
    rows.append([""] * 11 + ["Position", "Name + ID", "Name", "ID",
                             "Roster Position", "Salary", "Game Info",
                             "TeamAbbrev", "AvgPointsPerGame"])
    for name, draftable in ids.items():
        rows.append([""] * 11 + [
            "SP" if name.startswith("p") else "OF",
            f"{name} ({draftable})", name, str(draftable),
            "/".join(POSITIONS[name]), "5000",
            "ARI@BOS 08/17/2026 07:10PM ET", "BOS", "8.10",
        ])
    if trailing_note:
        rows.append([""] * 11 + ["", "", "", "", "", "", "", "", ""])
    with open(path, "w", newline="", encoding="utf-8") as handle:
        csv.writer(handle, lineterminator="\r\n").writerows(rows)
    return path


def test_read_template_finds_the_player_list_below_the_prose(tmp_path):
    path = write_template(tmp_path / "DKSalaries.csv")
    got = read_template_ids(path)
    assert set(got) == set(IDS.values())
    row = got[IDS["flex"]]
    assert row["Name"] == "flex"
    assert row["Roster Position"] == "1B/OF"
    assert row["Salary"] == "5000"


def test_read_template_rejects_a_file_that_is_not_one(tmp_path):
    path = tmp_path / "nope.csv"
    path.write_text("a,b,c\n1,2,3\n")
    with pytest.raises(ValueError, match="DKSalaries template"):
        read_template_ids(path)


def test_fill_template_writes_ids_into_the_entry_grid(tmp_path):
    template = write_template(tmp_path / "DKSalaries.csv")
    out = tmp_path / "filled.csv"
    n = fill_template(template, out, [NAMES, NAMES], FakeSlate(), IDS)
    assert n == 2

    rows = list(csv.reader(open(out, newline="", encoding="utf-8-sig")))
    assert rows[0][:10] == UPLOAD_COLUMNS
    for lineup_row in (rows[1], rows[2]):
        written = [int(v) for v in lineup_row[:10]]
        assert sorted(written) == sorted(IDS.values())
    # flex is needed at first base and carries its single id there.
    assert int(rows[1][3]) == IDS["flex"]


def test_fill_template_preserves_the_player_list_and_the_prose(tmp_path):
    template = write_template(tmp_path / "DKSalaries.csv")
    before = list(csv.reader(open(template, newline="", encoding="utf-8-sig")))
    out = tmp_path / "filled.csv"
    fill_template(template, out, [NAMES], FakeSlate(), IDS)
    after = list(csv.reader(open(out, newline="", encoding="utf-8-sig")))

    assert len(after) == len(before)
    # Only the first ten columns of the filled lineup rows may differ.
    for i, (a, b) in enumerate(zip(before, after)):
        assert a[10:] == b[10:], f"row {i} lost data to the right of the grid"
        if i != 1:
            assert a[:10] == b[:10], f"row {i} should not have been touched"
    assert read_template_ids(out) == read_template_ids(template)


def test_fill_template_refuses_a_template_from_another_slate(tmp_path):
    """The ids would import as the wrong players, or as nothing."""
    other = {name: 99000000 + i for i, name in enumerate(NAMES)}
    template = write_template(tmp_path / "DKSalaries.csv", ids=other)
    with pytest.raises(ValueError, match="different slate"):
        fill_template(template, tmp_path / "filled.csv", [NAMES],
                      FakeSlate(), IDS)


def test_fill_template_grows_the_file_when_there_are_more_lineups_than_rows(
    tmp_path,
):
    """Twenty lineups against a short template must not silently drop any."""
    template = write_template(tmp_path / "DKSalaries.csv", trailing_note=False)
    out = tmp_path / "filled.csv"
    n = fill_template(template, out, [NAMES] * 20, FakeSlate(), IDS)
    assert n == 20
    rows = list(csv.reader(open(out, newline="", encoding="utf-8-sig")))
    for i in range(1, 21):
        assert sorted(int(v) for v in rows[i][:10]) == sorted(IDS.values())
