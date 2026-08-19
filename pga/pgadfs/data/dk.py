"""DraftKings public endpoints: contest metadata, salaries, payout curve.

DraftKings' web app is authenticated but the JSON endpoints behind it are
not, so a slate can be pulled from a contest id alone -- no CSV export, no
login. A `DKSalaries.csv` export is still accepted as a fallback for when
these endpoints change.
"""

from __future__ import annotations

import csv
from dataclasses import dataclass
from pathlib import Path

from .cache import fetch_json, load_fixture

CONTEST_URL = "https://api.draftkings.com/contests/v1/contests/{contest_id}?format=json"
DRAFTABLES_URL = (
    "https://api.draftkings.com/draftgroups/v1/draftgroups/{draft_group}/draftables?format=json"
)


@dataclass(frozen=True)
class PayoutTier:
    lo: int          # best finishing position in the tier, 1-indexed
    hi: int          # worst finishing position in the tier, inclusive
    amount: float


@dataclass(frozen=True)
class Contest:
    contest_id: int
    name: str
    draft_group: int
    entry_fee: float
    max_entries: int
    entries: int
    max_entries_per_user: int
    total_payouts: float
    payouts: tuple[PayoutTier, ...]
    start_time: str

    @property
    def paid_places(self) -> int:
        return max((t.hi for t in self.payouts), default=0)

    def payout_vector(self) -> list[float]:
        """Prize for finishing 1st, 2nd, ... as a dense list."""
        out = [0.0] * self.paid_places
        for tier in self.payouts:
            for pos in range(tier.lo, tier.hi + 1):
                out[pos - 1] = tier.amount
        return out


@dataclass(frozen=True)
class DKPlayer:
    draftable_id: int
    name: str
    salary: int


def _parse_contest(payload: dict) -> Contest:
    d = payload["contestDetail"]
    tiers = []
    for tier in d.get("payoutSummary", []):
        descs = tier.get("payoutDescriptions") or []
        if not descs:
            continue
        tiers.append(
            PayoutTier(int(tier["minPosition"]), int(tier["maxPosition"]), float(descs[0]["value"]))
        )
    return Contest(
        contest_id=int(d["contestKey"]),
        name=d["name"],
        draft_group=int(d["draftGroupId"]),
        entry_fee=float(d["entryFee"]),
        max_entries=int(d["maximumEntries"]),
        entries=int(d.get("entries", 0)),
        max_entries_per_user=int(d.get("maximumEntriesPerUser", 1)),
        total_payouts=float(d["totalPayouts"]),
        payouts=tuple(sorted(tiers, key=lambda t: t.lo)),
        start_time=str(d.get("contestStartTime", "")),
    )


def load_contest(contest_id: int, *, offline: bool = False, max_age: float = 900.0) -> Contest:
    key = f"dk_contest_{contest_id}.json"
    if offline:
        return _parse_contest(load_fixture(key))
    return _parse_contest(fetch_json(CONTEST_URL.format(contest_id=contest_id), key, max_age=max_age))


def _parse_draftables(payload: dict) -> list[DKPlayer]:
    seen: dict[int, DKPlayer] = {}
    for row in payload["draftables"]:
        if row.get("isDisabled"):
            continue
        pid = int(row["draftableId"])
        seen.setdefault(pid, DKPlayer(pid, row["displayName"].strip(), int(row["salary"])))
    return sorted(seen.values(), key=lambda p: (-p.salary, p.name))


def load_draftables(draft_group: int, *, offline: bool = False, max_age: float = 900.0) -> list[DKPlayer]:
    key = f"dk_draftables_{draft_group}.json"
    if offline:
        return _parse_draftables(load_fixture(key))
    return _parse_draftables(
        fetch_json(DRAFTABLES_URL.format(draft_group=draft_group), key, max_age=max_age)
    )


def load_salaries_csv(path: str | Path) -> list[DKPlayer]:
    """Fallback: DraftKings' own `DKSalaries.csv` export.

    Only used when the JSON endpoints are unavailable. The export has no
    stable player key across slates, so draftable_id falls back to the row
    index and joins have to go through the name.
    """
    players: list[DKPlayer] = []
    with open(path, newline="", encoding="utf-8-sig") as fh:
        for i, row in enumerate(csv.DictReader(fh)):
            name = (row.get("Name") or row.get("name") or "").strip()
            salary = row.get("Salary") or row.get("salary")
            if not name or not salary:
                continue
            key = row.get("ID") or row.get("Id") or row.get("id")
            players.append(DKPlayer(int(key) if key else -(i + 1), name, int(float(salary))))
    return sorted(players, key=lambda p: (-p.salary, p.name))
