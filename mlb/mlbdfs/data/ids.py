"""Player identity across DraftKings, Statcast and the Stats API.

Three systems, three sets of identifiers, and the only thing DraftKings
gives you is a display name. The Chadwick Bureau register is the standard
crosswalk between MLBAM, FanGraphs, Baseball Reference and Retrosheet ids,
and pybaseball ships a loader for it.

Name matching is the fragile step and it fails quietly, which is the danger:
an unmatched star silently falls back to league-average talent and looks
like an ordinary projection. ``match_report`` exists so that never happens
without being seen.
"""

from __future__ import annotations

import logging

import pandas as pd

from .cache import cached_frame
from .dk import _match_key, normalize_team

log = logging.getLogger(__name__)


def chadwick_register(force: bool = False) -> pd.DataFrame:
    """The Chadwick people table, trimmed to active-era players."""

    def fetch() -> pd.DataFrame:
        from .sources import _require_pybaseball

        pb = _require_pybaseball()
        people = pb.chadwick_register()
        people = people[people["mlb_played_last"].fillna(0) >= 2015]
        return people[
            [
                "key_mlbam",
                "key_fangraphs",
                "key_bbref",
                "name_first",
                "name_last",
                "mlb_played_last",
            ]
        ].reset_index(drop=True)

    return cached_frame("chadwick_register", fetch, force=force)


def build_crosswalk(force: bool = False) -> pd.DataFrame:
    """Register with a normalized match key attached."""
    people = chadwick_register(force=force).copy()
    full = (
        people["name_first"].fillna("") + " " + people["name_last"].fillna("")
    ).str.strip()
    people["match_key"] = full.map(_match_key)
    people["full_name"] = full
    # Prefer the most recently active player when a name key collides.
    return people.sort_values("mlb_played_last", ascending=False)


def map_dk_to_mlbam(
    salaries: pd.DataFrame, crosswalk: pd.DataFrame | None = None
) -> dict[str, int]:
    """DraftKings player id -> MLBAM id.

    Ambiguous names resolve to the most recently active player, which is
    almost always right and is at least a stable, inspectable rule.
    """
    cw = build_crosswalk() if crosswalk is None else crosswalk
    lookup = cw.drop_duplicates("match_key").set_index("match_key")["key_mlbam"]

    out: dict[str, int] = {}
    for row in salaries.itertuples():
        key = _match_key(row.name)
        if key in lookup.index:
            value = lookup.loc[key]
            if pd.notna(value):
                out[str(row.dk_id)] = int(value)
    return out


def match_report(
    salaries: pd.DataFrame, mapping: dict[str, int]
) -> pd.DataFrame:
    """Players on the slate with no MLBAM id.

    Check this before trusting a slate. An unmatched player is not an error
    anywhere downstream -- he simply gets league-average rates and a wide
    interval -- so nothing will complain, and a missing star will quietly
    distort a whole team's projection.
    """
    missing = salaries[~salaries["dk_id"].astype(str).isin(mapping)]
    return (
        missing[["name", "team", "salary", "is_pitcher"]]
        .sort_values("salary", ascending=False)
        .reset_index(drop=True)
    )


def lineups_to_dk_ids(
    lineups: pd.DataFrame, salaries: pd.DataFrame
) -> pd.DataFrame:
    """Re-key a Stats API lineup frame onto DraftKings ids."""
    sal = salaries.copy()
    sal["_key"] = sal["name"].map(_match_key)
    sal["_team"] = sal["team"].map(normalize_team)

    lu = lineups.copy()
    lu["_key"] = lu["name"].map(_match_key)
    lu["_team"] = lu["team"].map(normalize_team)

    merged = lu.merge(
        sal[["_key", "_team", "dk_id"]], on=["_key", "_team"], how="inner"
    )
    return merged[["dk_id", "team", "batting_order", "name"]]
