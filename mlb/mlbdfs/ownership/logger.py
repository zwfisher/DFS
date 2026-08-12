"""Accumulate real contest ownership from DraftKings standings exports.

The heuristic ownership model is the weakest part of this project and the
only way to fix it properly is data. There is no clean free historical
source -- the FantasyLabs scrape in the repository's NFL code is documented
as incomplete, and RotoGrinders closed its endpoint -- but there is one
source that is exact, free and legitimately yours: the contest standings CSV
DraftKings lets you download for any contest you entered. It lists every
entry's lineup, from which true ownership follows by counting.

So the recommendation is to start logging now. Every slate you play adds a
labelled training example, and once enough have accumulated the conditional
logit in ``heuristic.py`` can be fit for real and swapped in behind the same
interface.
"""

from __future__ import annotations

import re
from datetime import date
from pathlib import Path

import pandas as pd

from ..config import CACHE_DIR

OWNERSHIP_STORE = CACHE_DIR / "ownership_history.parquet"

# Entries in a standings export look like:
#   "P Zack Wheeler C Will Smith 1B Freddie Freeman ..."
LINEUP_TOKEN_RE = re.compile(
    r"\b(P|SP|RP|C|1B|2B|3B|SS|OF)\b\s+(.+?)(?=\s+\b(?:P|SP|RP|C|1B|2B|3B|SS|OF)\b|$)"
)


def parse_standings(path: str | Path) -> pd.DataFrame:
    """Parse a DraftKings contest standings CSV into one row per roster spot."""
    raw = pd.read_csv(path)

    lineup_col = next(
        (c for c in ("Lineup", "lineup", "Roster") if c in raw.columns), None
    )
    if lineup_col is None:
        raise ValueError(
            f"no lineup column in standings export; got {list(raw.columns)}"
        )

    rows = []
    for entry_no, lineup in enumerate(raw[lineup_col].dropna().astype(str)):
        for position, name in LINEUP_TOKEN_RE.findall(lineup):
            rows.append(
                {
                    "entry": entry_no,
                    "position": "P" if position in ("SP", "RP") else position,
                    "name": name.strip(),
                }
            )
    return pd.DataFrame(rows)


def realized_ownership(standings: pd.DataFrame) -> pd.DataFrame:
    """Exact ownership from parsed standings.

    This is ground truth, not an estimate: it is the count of entries
    holding each player divided by the number of entries.
    """
    n_entries = standings["entry"].nunique()
    if n_entries == 0:
        return pd.DataFrame(columns=["name", "position", "entries", "ownership"])

    counts = (
        standings.groupby(["name", "position"])
        .size()
        .rename("entries")
        .reset_index()
    )
    counts["ownership"] = counts["entries"] / n_entries
    return counts.sort_values("ownership", ascending=False).reset_index(drop=True)


def log_slate(
    standings_path: str | Path,
    slate_date: date,
    contest_name: str,
    projected: pd.DataFrame | None = None,
    store: Path = OWNERSHIP_STORE,
) -> pd.DataFrame:
    """Append one slate's realized ownership to the training store.

    When a projection is supplied it is joined alongside, which turns the
    store into paired data: exactly what ``uncertainty.fit_concentration``
    needs, and what a fitted ownership model would train on.
    """
    realized = realized_ownership(parse_standings(standings_path))
    realized["slate_date"] = pd.to_datetime(slate_date)
    realized["contest"] = contest_name

    if projected is not None and not projected.empty:
        from ..data.dk import _match_key

        proj = projected.copy()
        proj["_key"] = proj["name"].map(_match_key)
        realized["_key"] = realized["name"].map(_match_key)
        realized = realized.merge(
            proj[["_key", "ownership", "salary", "proj"]].rename(
                columns={"ownership": "projected_ownership"}
            ),
            on="_key",
            how="left",
        ).drop(columns=["_key"])

    store.parent.mkdir(parents=True, exist_ok=True)
    if store.exists():
        combined = pd.concat([pd.read_parquet(store), realized], ignore_index=True)
        combined = combined.drop_duplicates(
            subset=["slate_date", "contest", "name", "position"], keep="last"
        )
    else:
        combined = realized

    combined.to_parquet(store, index=False)
    return realized


def load_history(store: Path = OWNERSHIP_STORE) -> pd.DataFrame:
    """Everything logged so far."""
    if not store.exists():
        return pd.DataFrame(
            columns=["slate_date", "contest", "name", "position", "ownership"]
        )
    return pd.read_parquet(store)


def accuracy_report(store: Path = OWNERSHIP_STORE) -> pd.DataFrame:
    """How the ownership projection has performed on logged slates.

    Mean absolute error is the headline, but the bias column matters more
    for tournament play: a model that is systematically low on chalk makes
    every leverage play look better than it is.
    """
    history = load_history(store)
    if history.empty or "projected_ownership" not in history.columns:
        return pd.DataFrame(columns=["slate_date", "contest", "n", "mae", "bias"])

    paired = history.dropna(subset=["projected_ownership"])
    if paired.empty:
        return pd.DataFrame(columns=["slate_date", "contest", "n", "mae", "bias"])

    paired = paired.assign(error=paired["projected_ownership"] - paired["ownership"])
    return (
        paired.groupby(["slate_date", "contest"])
        .agg(
            n=("name", "size"),
            mae=("error", lambda e: e.abs().mean()),
            bias=("error", "mean"),
        )
        .reset_index()
    )
