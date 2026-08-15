"""Baseball data fetchers.

The only module in the project that touches the network. Everything else
consumes the canonical counts frames produced here, which is what lets the
whole pipeline be developed and tested offline against fixtures.

The canonical hitter and pitcher frame is one row per player-season:

    player_id, season, pa, strikeout, field_out, walk, hit_by_pitch,
    single, double, triple, home_run, sb, cs

Note that these hosts are commonly blocked on locked-down networks --
Baseball Savant, the MLB Stats API and FanGraphs all sit behind egress
policies in many managed environments. If a fetch fails with a proxy error,
that is what happened; pull the data somewhere with open egress and point
``MLBDFS_CACHE`` at the result.
"""

from __future__ import annotations

import logging
from datetime import date, timedelta

import numpy as np
import pandas as pd

from ..config import OUTCOMES
from .cache import cached_frame

log = logging.getLogger(__name__)

# Statcast event codes mapped onto our outcome space. Anything not listed
# resolves to a ball in play out, which is the right default: the events
# left over are fielder's choices, sacrifices and the various out types.
EVENT_MAP = {
    "strikeout": "strikeout",
    "strikeout_double_play": "strikeout",
    "walk": "walk",
    "intent_walk": "walk",
    "hit_by_pitch": "hit_by_pitch",
    "single": "single",
    "double": "double",
    "triple": "triple",
    "home_run": "home_run",
}


def _require_pybaseball():
    try:
        import pybaseball
    except ImportError as exc:  # pragma: no cover - depends on install extras
        raise ImportError(
            "pybaseball is required for live data: pip install 'mlbdfs[data]'"
        ) from exc
    return pybaseball


def statcast_range(start: date, end: date, force: bool = False) -> pd.DataFrame:
    """Raw Statcast pitch-level data for a date range."""

    def fetch() -> pd.DataFrame:
        pb = _require_pybaseball()
        pb.cache.enable()
        return pb.statcast(
            start_dt=start.isoformat(), end_dt=end.isoformat(), verbose=False
        )

    return cached_frame(
        "statcast", fetch, force=force, start=start.isoformat(), end=end.isoformat()
    )


def _events_to_counts(
    pbp: pd.DataFrame, id_column: str, season: int | None = None
) -> pd.DataFrame:
    """Collapse pitch-level Statcast rows into per-player outcome counts."""
    # One row per plate appearance: the pitch that ended it carries `events`.
    pa = pbp[pbp["events"].notna() & (pbp["events"] != "")].copy()
    if pa.empty:
        return pd.DataFrame(columns=["player_id", "season", "pa", *OUTCOMES])

    pa["outcome"] = pa["events"].map(EVENT_MAP).fillna("field_out")
    pa["player_id"] = pa[id_column].astype("Int64")
    if season is None:
        pa["season"] = pd.to_datetime(pa["game_date"]).dt.year
    else:
        pa["season"] = season

    counts = (
        pa.pivot_table(
            index=["player_id", "season"],
            columns="outcome",
            aggfunc="size",
            fill_value=0,
        )
        .reset_index()
    )
    for name in OUTCOMES:
        if name not in counts.columns:
            counts[name] = 0
    counts["pa"] = counts[list(OUTCOMES)].sum(axis=1)

    stolen = _steal_counts(pbp)
    counts = counts.merge(stolen, on="player_id", how="left")
    counts[["sb", "cs"]] = counts[["sb", "cs"]].fillna(0)

    return counts[["player_id", "season", "pa", *OUTCOMES, "sb", "cs"]]


def _steal_counts(pbp: pd.DataFrame) -> pd.DataFrame:
    """Stolen bases and caught stealings per runner.

    Statcast records these on the ``events`` field of the pitch they
    occurred on, with the runner identified by the base-occupancy columns.
    Only steals of second are counted, which is what the simulator models.
    """
    if "events" not in pbp.columns or "on_1b" not in pbp.columns:
        return pd.DataFrame(columns=["player_id", "sb", "cs"])

    rows = []
    for event, column in (("stolen_base_2b", "sb"), ("caught_stealing_2b", "cs")):
        sub = pbp[pbp["events"] == event]
        if sub.empty:
            continue
        counts = sub["on_1b"].dropna().astype("Int64").value_counts()
        rows.append(counts.rename(column))

    if not rows:
        return pd.DataFrame(columns=["player_id", "sb", "cs"])

    out = pd.concat(rows, axis=1).fillna(0).reset_index()
    out = out.rename(columns={"index": "player_id", "on_1b": "player_id"})
    for c in ("sb", "cs"):
        if c not in out.columns:
            out[c] = 0
    return out[["player_id", "sb", "cs"]]


def batter_counts(season: int, force: bool = False) -> pd.DataFrame:
    """Per-batter outcome counts for a season."""

    def fetch() -> pd.DataFrame:
        pbp = statcast_range(date(season, 3, 1), date(season, 11, 15))
        return _events_to_counts(pbp, "batter", season)

    return cached_frame("batter_counts", fetch, force=force, season=season)


def pitcher_counts(season: int, force: bool = False) -> pd.DataFrame:
    """Per-pitcher allowed-outcome counts for a season."""

    def fetch() -> pd.DataFrame:
        pbp = statcast_range(date(season, 3, 1), date(season, 11, 15))
        return _events_to_counts(pbp, "pitcher", season)

    return cached_frame("pitcher_counts", fetch, force=force, season=season)


def probable_pitchers(game_date: date, force: bool = False) -> pd.DataFrame:
    """Announced starting pitchers for a date, from the MLB Stats API."""

    def fetch() -> pd.DataFrame:
        import requests

        url = (
            "https://statsapi.mlb.com/api/v1/schedule"
            f"?sportId=1&date={game_date.isoformat()}"
            "&hydrate=probablePitcher,team,linescore"
        )
        payload = requests.get(url, timeout=30).json()

        rows = []
        for day in payload.get("dates", []):
            for game in day.get("games", []):
                for side in ("away", "home"):
                    team = game["teams"][side]
                    pitcher = team.get("probablePitcher") or {}
                    rows.append(
                        {
                            "game_pk": game["gamePk"],
                            "game_date": game_date,
                            "side": side,
                            "team": team["team"]["abbreviation"],
                            "opponent": game["teams"][
                                "home" if side == "away" else "away"
                            ]["team"]["abbreviation"],
                            "pitcher_id": pitcher.get("id"),
                            "pitcher_name": pitcher.get("fullName"),
                        }
                    )
        return pd.DataFrame(rows)

    return cached_frame(
        "probable_pitchers",
        fetch,
        ttl=timedelta(hours=3),
        force=force,
        game_date=game_date.isoformat(),
    )


def confirmed_lineups(game_date: date, force: bool = False) -> pd.DataFrame:
    """Posted batting orders for a date, from the MLB Stats API.

    Lineups are the highest-value late input in MLB DFS and the main reason
    projections move in the hour before lock. Cached briefly rather than
    permanently, since they are revised until first pitch.
    """

    def fetch() -> pd.DataFrame:
        import requests

        sched = (
            "https://statsapi.mlb.com/api/v1/schedule"
            f"?sportId=1&date={game_date.isoformat()}"
        )
        games = requests.get(sched, timeout=30).json()
        game_pks = [
            g["gamePk"] for d in games.get("dates", []) for g in d.get("games", [])
        ]

        rows = []
        for pk in game_pks:
            box = requests.get(
                f"https://statsapi.mlb.com/api/v1/game/{pk}/boxscore", timeout=30
            ).json()
            for side in ("away", "home"):
                team_box = box.get("teams", {}).get(side, {})
                abbr = team_box.get("team", {}).get("abbreviation")
                for order, pid in enumerate(team_box.get("battingOrder", []), start=1):
                    person = team_box.get("players", {}).get(f"ID{pid}", {})
                    rows.append(
                        {
                            "game_pk": pk,
                            "team": abbr,
                            "player_id": pid,
                            "name": person.get("person", {}).get("fullName"),
                            "batting_order": order,
                        }
                    )
        return pd.DataFrame(rows)

    return cached_frame(
        "confirmed_lineups",
        fetch,
        ttl=timedelta(minutes=20),
        force=force,
        game_date=game_date.isoformat(),
    )


def bullpen_counts(season: int, force: bool = False) -> pd.DataFrame:
    """Team-level relief pitching outcome counts.

    Relievers throw a large share of modern innings and the simulator hands
    the ball to them once the starter is hooked, so a team-level composite
    is worth having rather than falling back on a league-average arm.
    """

    def fetch() -> pd.DataFrame:
        pbp = statcast_range(date(season, 3, 1), date(season, 11, 15))
        # A pitcher is relieving when he is not the one who started the game
        # for his side; Statcast does not flag this directly, so it is
        # inferred from the first pitcher each team used in each game.
        starters = (
            pbp.sort_values(["game_pk", "at_bat_number"])
            .groupby(["game_pk", "inning_topbot"])["pitcher"]
            .first()
            .reset_index()
            .rename(columns={"pitcher": "starter"})
        )
        merged = pbp.merge(starters, on=["game_pk", "inning_topbot"], how="left")
        relief = merged[merged["pitcher"] != merged["starter"]].copy()
        relief["team"] = np.where(
            relief["inning_topbot"] == "Top",
            relief["home_team"],
            relief["away_team"],
        )

        out = []
        for team, group in relief.groupby("team"):
            counts = _events_to_counts(group, "pitcher", season)
            totals = counts[["pa", *OUTCOMES]].sum()
            totals["team"] = team
            totals["season"] = season
            out.append(totals)
        return pd.DataFrame(out).reset_index(drop=True)

    return cached_frame("bullpen_counts", fetch, force=force, season=season)


def _extract_lineups(pbp: pd.DataFrame) -> pd.DataFrame:
    """Recover starting lineups and opposing starter handedness from play-by-play.

    No extra endpoint is needed for this. The first nine distinct batters a
    team sends up, ordered by when they first appear, *are* that team's
    starting lineup in batting order -- a pinch hitter necessarily shows up
    later. The opposing starter is the first pitcher that team faced, and
    Statcast carries his throwing hand on every pitch.

    Returns one row per team-game-hitter:

        game_pk, game_date, team, opponent, player_id, batting_order, opp_hand
    """
    needed = {"game_pk", "game_date", "at_bat_number", "batter", "pitcher",
              "inning_topbot", "home_team", "away_team", "p_throws"}
    missing = needed - set(pbp.columns)
    if missing:
        raise ValueError(f"play-by-play is missing columns: {sorted(missing)}")

    df = pbp[list(needed)].copy()
    top = df["inning_topbot"].astype(str).str.lower().str.startswith("top")
    df["team"] = np.where(top, df["away_team"], df["home_team"])
    df["opponent"] = np.where(top, df["home_team"], df["away_team"])

    # Batting order: rank each hitter by the first plate appearance he takes.
    first_pa = (
        df.groupby(["game_pk", "team", "batter"], as_index=False)
        .agg(first_ab=("at_bat_number", "min"))
    )
    first_pa["batting_order"] = (
        first_pa.groupby(["game_pk", "team"])["first_ab"]
        .rank(method="first")
        .astype(int)
    )
    starters = first_pa[first_pa["batting_order"] <= 9].copy()

    # The opposing starter is whoever threw the team's first plate appearance.
    opener = (
        df.sort_values("at_bat_number")
        .groupby(["game_pk", "team"], as_index=False)
        .first()[["game_pk", "team", "opponent", "game_date", "p_throws"]]
        .rename(columns={"p_throws": "opp_hand"})
    )

    out = starters.merge(opener, on=["game_pk", "team"], how="left")
    out = out.rename(columns={"batter": "player_id"})
    out["game_date"] = pd.to_datetime(out["game_date"])
    out["opp_hand"] = out["opp_hand"].astype(str).str.upper().str[0]
    return out[
        ["game_pk", "game_date", "team", "opponent", "player_id",
         "batting_order", "opp_hand"]
    ].sort_values(["game_date", "team", "batting_order"])


def lineup_history(season: int, force: bool = False) -> pd.DataFrame:
    """Every starting lineup of a season, with the opposing starter's hand.

    This is what the projected-lineup model trains on. It is derived from the
    Statcast pull the projections already need, so it costs no additional
    network access.
    """

    def fetch() -> pd.DataFrame:
        pbp = statcast_range(date(season, 3, 1), date(season, 11, 15))
        return _extract_lineups(pbp)

    return cached_frame("lineup_history", fetch, force=force, season=season)


def batter_counts_by_hand(season: int, force: bool = False) -> pd.DataFrame:
    """Per-batter outcome counts split by the pitcher's throwing hand.

    The platoon split is the other half of accounting for handedness: which
    hitters are in the lineup is one question, how they hit the arm they are
    facing is another, and the second is worth more per plate appearance.
    Returns the canonical counts frame with an extra ``vs_hand`` column.
    """

    def fetch() -> pd.DataFrame:
        pbp = statcast_range(date(season, 3, 1), date(season, 11, 15))
        frames = []
        for hand in ("L", "R"):
            subset = pbp[pbp["p_throws"].astype(str).str.upper().str[0] == hand]
            if subset.empty:
                continue
            counts = _events_to_counts(subset, "batter", season)
            counts["vs_hand"] = hand
            frames.append(counts)
        if not frames:
            return pd.DataFrame(columns=["player_id", "season", "pa", *OUTCOMES, "vs_hand"])
        return pd.concat(frames, ignore_index=True)

    return cached_frame("batter_counts_by_hand", fetch, force=force, season=season)


def pitcher_handedness(season: int, force: bool = False) -> pd.DataFrame:
    """Throwing hand for every pitcher who appeared in a season.

    Derived from the Statcast pull rather than a roster endpoint, so it
    costs nothing extra. Handedness is what the platoon model keys on, both
    for who is in the lineup and for how they hit, so it has to be populated
    from data -- defaulting every starter to right-handed would silently
    disable the entire platoon adjustment for left-handers.
    """

    def fetch() -> pd.DataFrame:
        pbp = statcast_range(date(season, 3, 1), date(season, 11, 15))
        hands = (
            pbp.groupby("pitcher")["p_throws"]
            .agg(lambda s: s.astype(str).str.upper().str[0].mode().iloc[0])
            .reset_index()
        )
        return hands.rename(columns={"pitcher": "player_id", "p_throws": "throws"})

    return cached_frame("pitcher_handedness", fetch, force=force, season=season)
