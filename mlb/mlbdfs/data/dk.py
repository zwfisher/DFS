"""DraftKings salary file parsing and slate assembly.

The salary export is the authoritative statement of who is playable, what
they cost and which positions they fill, so it is the spine of a slate.
Everything else -- talent rates, lineups, Vegas totals -- is joined onto it.
"""

from __future__ import annotations

import re
from pathlib import Path

import pandas as pd

from ..config import LEAGUE_RUNS_PER_GAME
from ..slate import Game, Player, Slate

# DraftKings exports have moved column names around over the years; accept
# the variants rather than breaking on a rename.
COLUMN_ALIASES = {
    "name": ["Name", "Player", "Nickname"],
    "dk_id": ["ID", "Id", "Player ID"],
    "positions": ["Roster Position", "Position", "Pos"],
    "salary": ["Salary"],
    "game_info": ["Game Info", "GameInfo", "Game"],
    "team": ["TeamAbbrev", "Team", "teamAbbrev"],
}

# DraftKings uses a few abbreviations that differ from the ones Statcast and
# the Stats API use.
TEAM_FIXES = {
    "WSH": "WSH", "WAS": "WSH", "CWS": "CWS", "CHW": "CWS",
    "SD": "SD", "SDP": "SD", "SF": "SF", "SFG": "SF",
    "TB": "TB", "TBR": "TB", "KC": "KC", "KCR": "KC",
    "LA": "LAD", "LAD": "LAD",
    # Statcast says AZ where DraftKings says ARI. Left unmapped, Arizona
    # silently gets no lineup history and falls back to the salary guess --
    # a whole team quietly downgraded on every slate they play.
    "AZ": "ARI", "ARI": "ARI",
    "OAK": "ATH", "ATH": "ATH",
}

GAME_INFO_RE = re.compile(r"^\s*([A-Z]{2,3})@([A-Z]{2,3})")


def normalize_team(abbr: str) -> str:
    if not isinstance(abbr, str):
        return ""
    return TEAM_FIXES.get(abbr.strip().upper(), abbr.strip().upper())


def _resolve_columns(frame: pd.DataFrame) -> dict[str, str]:
    resolved = {}
    for canonical, options in COLUMN_ALIASES.items():
        for option in options:
            if option in frame.columns:
                resolved[canonical] = option
                break
    missing = [k for k in ("name", "positions", "salary", "team") if k not in resolved]
    if missing:
        raise ValueError(
            f"DraftKings file is missing columns for {missing}; got {list(frame.columns)}"
        )
    return resolved


def parse_salaries(path: str | Path) -> pd.DataFrame:
    """Read a DraftKings salary export into a tidy frame."""
    raw = pd.read_csv(path)
    cols = _resolve_columns(raw)

    out = pd.DataFrame(
        {
            "name": raw[cols["name"]].astype(str).str.strip(),
            "salary": raw[cols["salary"]].astype(int),
            "team": raw[cols["team"]].map(normalize_team),
            "positions_raw": raw[cols["positions"]].astype(str),
        }
    )
    if "dk_id" in cols:
        out["dk_id"] = raw[cols["dk_id"]].astype(str)
    else:
        out["dk_id"] = out["name"]

    if "game_info" in cols:
        info = raw[cols["game_info"]].astype(str)
        parsed = info.str.extract(GAME_INFO_RE)
        out["away"] = parsed[0].map(normalize_team)
        out["home"] = parsed[1].map(normalize_team)
    else:
        out["away"] = ""
        out["home"] = ""

    out["positions"] = out["positions_raw"].map(_split_positions)
    out["is_pitcher"] = out["positions"].map(lambda ps: "P" in ps or "SP" in ps)
    # DraftKings lists starters as SP and relievers as RP; the Classic roster
    # has a single P slot that both fill.
    out["positions"] = out["positions"].map(
        lambda ps: tuple(dict.fromkeys("P" if p in ("SP", "RP") else p for p in ps))
    )
    out["opponent"] = [
        h if t == a else a for t, a, h in zip(out["team"], out["away"], out["home"])
    ]
    return _collapse_multi_position(out.drop(columns=["positions_raw"]))


def _collapse_multi_position(frame: pd.DataFrame) -> pd.DataFrame:
    """One row per player, with every eligible position unioned together.

    DraftKings lists a multi-eligible player once per roster slot -- a
    second baseman who also qualifies at shortstop appears on two rows with
    the same player id. Left alone, that becomes two Player objects, and
    downstream it means a team resolves to ten or eleven hitters and the
    optimizer can roster the same person twice while still believing every
    slot is filled by someone different.
    """
    if frame["dk_id"].is_unique:
        return frame

    how: dict[str, object] = {}
    for col in frame.columns:
        if col == "dk_id":
            continue
        if col == "positions":
            how[col] = lambda vals: tuple(dict.fromkeys(p for row in vals for p in row))
        elif frame[col].dtype == bool:
            how[col] = "any"
        else:
            how[col] = "first"
    merged = frame.groupby("dk_id", as_index=False).agg(how)
    return merged[frame.columns]


def _split_positions(value: str) -> tuple[str, ...]:
    parts = re.split(r"[/,]", value.upper())
    return tuple(p.strip() for p in parts if p.strip())


def build_slate(
    salaries: pd.DataFrame,
    lineups: pd.DataFrame | None = None,
    starters: dict[str, str] | None = None,
    team_totals: dict[str, float] | None = None,
    name: str = "slate",
) -> Slate:
    """Assemble a Slate from a salary frame plus whatever else is known.

    ``lineups`` is an optional frame with ``name``/``team``/``batting_order``
    columns; matching is by normalized name within team, which is reliable
    enough within a single team's roster. Unmatched hitters are left without
    a batting order and filled in by ``projections.lineups.resolve_lineup``.
    """
    order_lookup: dict[tuple[str, str], int] = {}
    if lineups is not None and not lineups.empty:
        for row in lineups.itertuples():
            order_lookup[(normalize_team(row.team), _match_key(row.name))] = int(
                row.batting_order
            )

    players: list[Player] = []
    for row in salaries.itertuples():
        key = (row.team, _match_key(row.name))
        players.append(
            Player(
                player_id=str(row.dk_id),
                name=row.name,
                team=row.team,
                opponent=row.opponent,
                positions=row.positions,
                salary=int(row.salary),
                is_pitcher=bool(row.is_pitcher),
                batting_order=order_lookup.get(key),
                confirmed=key in order_lookup,
                dk_id=str(row.dk_id),
            )
        )

    totals = team_totals or {}
    games: list[Game] = []
    pairs = (
        salaries[["away", "home"]].dropna().drop_duplicates().itertuples(index=False)
    )
    for away, home in pairs:
        if not away or not home:
            continue
        games.append(
            Game(
                game_id=f"{away}@{home}",
                away=away,
                home=home,
                away_total=totals.get(away, LEAGUE_RUNS_PER_GAME),
                home_total=totals.get(home, LEAGUE_RUNS_PER_GAME),
                away_starter=(starters or {}).get(away),
                home_starter=(starters or {}).get(home),
            )
        )

    return Slate(players=players, games=games, name=name)


def _match_key(name: str) -> str:
    """Normalized name for joining across sources.

    Accents, punctuation and suffixes are the usual culprits: "Jose Ramirez"
    against "José Ramírez", "Vladimir Guerrero Jr." against "Vladimir
    Guerrero Jr". Stripping all of it and lowercasing resolves nearly
    everything within a single team's roster.
    """
    import unicodedata

    text = unicodedata.normalize("NFKD", str(name))
    text = "".join(c for c in text if not unicodedata.combining(c))
    text = re.sub(r"\b(jr|sr|ii|iii|iv)\b", "", text.lower())
    return re.sub(r"[^a-z]", "", text)


def infer_starters(salaries: pd.DataFrame, probables: pd.DataFrame) -> dict[str, str]:
    """Map team -> DraftKings player id for each announced starter."""
    by_team: dict[str, str] = {}
    if probables is None or probables.empty:
        return by_team

    pitchers = salaries[salaries["is_pitcher"]]
    for row in probables.itertuples():
        if not row.pitcher_name:
            continue
        team = normalize_team(row.team)
        key = _match_key(row.pitcher_name)
        match = pitchers[
            (pitchers["team"] == team)
            & (pitchers["name"].map(_match_key) == key)
        ]
        if not match.empty:
            by_team[team] = str(match.iloc[0]["dk_id"])
    return by_team


def unmatched_report(
    salaries: pd.DataFrame, lineups: pd.DataFrame | None
) -> pd.DataFrame:
    """Hitters on the slate with no posted batting order.

    Worth glancing at before every slate: a name-matching failure looks
    exactly like an unposted lineup, and quietly costs you the batting order
    for a player you meant to roster.
    """
    if lineups is None or lineups.empty:
        return salaries[~salaries["is_pitcher"]][["name", "team", "salary"]]

    keys = {
        (normalize_team(r.team), _match_key(r.name)) for r in lineups.itertuples()
    }
    hitters = salaries[~salaries["is_pitcher"]].copy()
    hitters["matched"] = [
        (t, _match_key(n)) in keys for t, n in zip(hitters["team"], hitters["name"])
    ]
    return hitters[~hitters["matched"]][["name", "team", "salary"]].reset_index(
        drop=True
    )
