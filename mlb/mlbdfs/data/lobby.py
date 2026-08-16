"""DraftKings lobby: what contests exist, and what they actually pay.

Two endpoints, both public and both JSON:

``www.draftkings.com/lobby/getcontests?sport=MLB``
    Every live contest for a sport, plus the draft groups (slates) they
    belong to. Gives entry fee, prize pool, field size, current entries and
    the max-entries-per-user rule -- everything needed to compute rake and
    overlay, but *not* the payout curve.

``api.draftkings.com/contests/v1/contests/{id}``
    One contest's payout table as ``(minPosition, maxPosition, prize)``
    bands, which is exactly the shape :class:`~mlbdfs.optimize.contest.Contest`
    wants.

The lobby's field names are terse and two of them are easy to misread:
``m`` is the *maximum* field size and ``nt`` is how many entries are in so
far. Reading ``m`` as current entries makes every rake calculation wrong,
so the mapping is done once, here, and everything downstream uses names.
"""

from __future__ import annotations

import json
import logging
from datetime import date, datetime, timedelta

import pandas as pd

from ..config import CACHE_DIR, OFFLINE
from ..optimize.contest import Contest
from .cache import OfflineError, cached_frame

log = logging.getLogger(__name__)

LOBBY_URL = "https://www.draftkings.com/lobby/getcontests?sport={sport}"
CONTEST_URL = "https://api.draftkings.com/contests/v1/contests/{contest_id}"

# Lobby field -> what it means. Left as a table because the abbreviations
# are not guessable and getting one wrong is silent.
_CONTEST_FIELDS = {
    "id": "contest_id",
    "n": "name",
    "a": "entry_fee",
    "m": "max_entries",       # field size cap, NOT entries so far
    "nt": "current_entries",  # entries so far
    "po": "prize_pool",
    "mec": "max_entries_per_user",
    "dg": "draft_group_id",
    "gameTypeId": "game_type_id",
    "gameType": "game_type",
    "sdstring": "start_time_text",
    "tix": "ticket_prize",
    "isSnakeDraft": "snake",
}


# Player-game attribute ids in the draftables feed. Only one is documented
# by its own name; 112 is the opposing probable starter, rendered the way
# the lobby shows it -- "Cease (R)".
_ATTR_OPPOSING_STARTER = 112

_PROBABLE_RE = None


def _hand(value: str | None) -> str:
    """"Right"/"Left"/"Switch" -> R/L/S, blank when unknown."""
    if not value:
        return ""
    return {"right": "R", "left": "L", "switch": "S"}.get(str(value).lower(), "")


def _parse_probable(value: str | None) -> tuple[str, str]:
    """Split ``"Cease (R)"`` into name and throwing hand."""
    global _PROBABLE_RE
    if _PROBABLE_RE is None:
        import re

        _PROBABLE_RE = re.compile(r"^(.*?)\s*\(([RLS])\)\s*$")
    if not value:
        return "", ""
    match = _PROBABLE_RE.match(str(value))
    if not match:
        return str(value).strip(), ""
    return match.group(1).strip(), match.group(2)


def _get_json(url: str, timeout: float = 60.0) -> dict:
    import requests

    resp = requests.get(
        url,
        timeout=timeout,
        headers={"Accept": "application/json", "User-Agent": "mlbdfs/0.1"},
    )
    resp.raise_for_status()
    return resp.json()


def fetch_lobby(sport: str = "MLB", ttl_minutes: float = 20.0) -> dict:
    """The raw lobby payload, cached briefly.

    Short TTL because contest fill changes minute to minute and fill is
    what overlay is computed from. Cached as JSON rather than parquet --
    the payload is nested and the draft-group list is wanted alongside the
    contests.
    """
    path = CACHE_DIR / f"lobby_{sport.lower()}.json"
    if path.exists():
        age = datetime.now() - datetime.fromtimestamp(path.stat().st_mtime)
        if age < timedelta(minutes=ttl_minutes):
            return json.loads(path.read_text())
    if OFFLINE:
        raise OfflineError(f"lobby for {sport} not cached and offline mode is on")
    payload = _get_json(LOBBY_URL.format(sport=sport))
    CACHE_DIR.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload))
    return payload


def draft_groups(lobby: dict) -> pd.DataFrame:
    """Slates on offer, one row each, sorted by start time.

    ``game_type_id`` 2 is Classic. The rest are Showdown (114), Tiers (45),
    Snake (178/179) and the various single-stat games, none of which the
    optimizer's roster rules apply to.
    """
    rows = []
    for g in lobby.get("DraftGroups", []):
        rows.append(
            {
                "draft_group_id": g["DraftGroupId"],
                "start_et": pd.Timestamp(g["StartDateEst"]),
                "games": g["GameCount"],
                "game_type_id": g["GameTypeId"],
                "tag": g.get("DraftGroupTag") or "",
                "suffix": (g.get("ContestStartTimeSuffix") or "").strip(),
            }
        )
    df = pd.DataFrame(rows)
    if df.empty:
        return df
    return df.sort_values(["start_et", "draft_group_id"]).reset_index(drop=True)


def lobby_contests(lobby: dict, draft_group_id: int | None = None) -> pd.DataFrame:
    """Contests as a frame with readable column names.

    ``guaranteed`` matters for overlay: only a guaranteed prize pool is paid
    out whether or not the contest fills.
    """
    rows = []
    for c in lobby.get("Contests", []):
        if draft_group_id is not None and c.get("dg") != draft_group_id:
            continue
        row = {name: c.get(key) for key, name in _CONTEST_FIELDS.items()}
        attr = c.get("attr") or {}
        row["guaranteed"] = str(attr.get("IsGuaranteed", "")).lower() == "true"
        rows.append(row)
    df = pd.DataFrame(rows)
    if df.empty:
        return df
    for col in ("entry_fee", "prize_pool"):
        df[col] = pd.to_numeric(df[col], errors="coerce")
    for col in ("max_entries", "current_entries", "max_entries_per_user"):
        df[col] = pd.to_numeric(df[col], errors="coerce").astype("Int64")
    return df.sort_values("prize_pool", ascending=False).reset_index(drop=True)


def main_slate(lobby: dict, on: date | None = None) -> pd.Series | None:
    """The largest Classic draft group on a date -- the "main slate".

    Ties go to the earlier start, which is the convention DraftKings itself
    follows when it labels one group "Featured".
    """
    df = draft_groups(lobby)
    if df.empty:
        return None
    df = df[df["game_type_id"] == 2]
    if on is not None:
        df = df[df["start_et"].dt.date == on]
    if df.empty:
        return None
    df = df.sort_values(["games", "start_et"], ascending=[False, True])
    return df.iloc[0]


def fetch_payouts(contest_id: int, ttl_hours: float = 12.0) -> pd.DataFrame:
    """Payout bands for one contest: ``rank_from, rank_to, prize``.

    Only cash prizes are returned. Contests paying tickets come back with a
    ``$0.00`` cash description, so they land here as an empty frame rather
    than as a free tournament -- check for that before treating the result
    as a payout curve.
    """

    def fetch() -> pd.DataFrame:
        payload = _get_json(CONTEST_URL.format(contest_id=contest_id))
        detail = payload.get("contestDetail") or {}
        rows = []
        for band in detail.get("payoutSummary") or []:
            tiers = band.get("tierPayoutDescriptions") or {}
            cash = tiers.get("Cash")
            value = 0.0
            for desc in band.get("payoutDescriptions") or []:
                if desc.get("payoutDescriptionType") == "Text":
                    value = float(desc.get("value") or 0.0)
                    break
            else:
                if cash:
                    value = float(str(cash).replace("$", "").replace(",", ""))
            if value <= 0:
                continue
            rows.append(
                {
                    "rank_from": int(band["minPosition"]),
                    "rank_to": int(band["maxPosition"]),
                    "prize": value,
                }
            )
        return pd.DataFrame(rows, columns=["rank_from", "rank_to", "prize"])

    return cached_frame(
        "dk_payouts",
        fetch,
        ttl=timedelta(hours=ttl_hours),
        contest_id=int(contest_id),
    )


def fetch_draftables(draft_group_id: int, ttl_minutes: float = 20.0) -> pd.DataFrame:
    """The salary list for a slate, in the shape ``parse_salaries`` returns.

    Saves exporting the CSV from the lobby by hand, and carries two things
    the export does not: a ``status`` field that flags scratches and
    injuries before the batting order posts, and the roster-slot split that
    :func:`~mlbdfs.data.dk._collapse_multi_position` needs -- DraftKings
    lists a multi-eligible player once per slot here too.
    """
    from .dk import _collapse_multi_position, normalize_team

    def fetch() -> pd.DataFrame:
        url = (
            "https://api.draftkings.com/draftgroups/v1/draftgroups/"
            f"{draft_group_id}/draftables"
        )
        payload = _get_json(url)
        rows = []
        for d in payload.get("draftables", []):
            comp = d.get("competition") or {}
            teams = (comp.get("name") or "").replace(" ", "").split("@")
            away = normalize_team(teams[0]) if len(teams) == 2 else ""
            home = normalize_team(teams[1]) if len(teams) == 2 else ""
            team = normalize_team(d.get("teamAbbreviation") or "")
            attrs = {
                a["name"]: a.get("value")
                for a in (d.get("playerAttributes") or [])
            }
            game_attrs = {
                a["id"]: a.get("value")
                for a in (d.get("playerGameAttributes") or [])
            }
            slots = [p for p in str(d.get("position") or "").split("/") if p]
            opponent_starter, opponent_starter_hand = _parse_probable(
                game_attrs.get(_ATTR_OPPOSING_STARTER, "")
            )
            rows.append(
                {
                    "dk_id": str(d.get("playerId")),
                    "name": (d.get("displayName") or "").strip(),
                    "salary": int(d.get("salary") or 0),
                    "team": team,
                    "positions": "/".join(
                        dict.fromkeys("P" if p in ("SP", "RP") else p for p in slots)
                    ),
                    "is_pitcher": any(p in ("SP", "RP", "P") for p in slots),
                    "is_starter": "SP" in slots,
                    "away": away,
                    "home": home,
                    "opponent": home if team == away else away,
                    # "None" is DraftKings' way of saying nothing is wrong.
                    "status": "" if d.get("status") == "None" else (d.get("status") or ""),
                    "throws": _hand(attrs.get("Handedness")),
                    "bats": _hand(attrs.get("Bat-Handedness")),
                    "opponent_starter": opponent_starter,
                    "opponent_starter_hand": opponent_starter_hand,
                    "game_start": comp.get("startTime") or "",
                }
            )
        return pd.DataFrame(rows)

    frame = cached_frame(
        "dk_draftables",
        fetch,
        ttl=timedelta(minutes=ttl_minutes),
        draft_group_id=int(draft_group_id),
    )
    if frame.empty:
        return frame
    # Parquet cannot store tuples, so positions round-trips as a string.
    frame = frame.copy()
    frame["positions"] = frame["positions"].map(
        lambda s: tuple(p for p in str(s).split("/") if p)
    )
    ordered = [c for c in frame.columns if c not in ("is_pitcher",)] + ["is_pitcher"]
    return _collapse_multi_position(frame[ordered]).reset_index(drop=True)


def probable_starters(draftables: pd.DataFrame) -> pd.DataFrame:
    """Each team's probable starter, read out of the draftables feed.

    DraftKings tags every player with the *opposing* probable starter, so a
    team's own starter is recovered from the other side of its game. Only a
    surname is published, which is matched against starting-pitcher-eligible
    players on that team; ties go to the highest salary, since a slate
    almost never carries two same-named starters and when it does the
    expensive one is the one starting.

    Returns ``team, dk_id, name, throws`` -- and the hand comes from
    DraftKings rather than from a Statcast pull, which is one fewer fetch
    standing between you and a slate.
    """
    from .dk import _match_key

    if draftables.empty:
        return pd.DataFrame(columns=["team", "dk_id", "name", "throws"])

    surname_of = {}
    for row in draftables.itertuples():
        if row.opponent_starter and row.opponent:
            surname_of[row.opponent] = (row.opponent_starter, row.opponent_starter_hand)

    pitchers = draftables[draftables["is_pitcher"]]
    rows = []
    for team, (surname, hand) in sorted(surname_of.items()):
        key = _match_key(surname)
        pool = pitchers[pitchers["team"] == team]
        match = pool[pool["name"].map(lambda n: _match_key(n).endswith(key))]
        # Prefer an SP-eligible match, but do not require one: a bullpen
        # game or a converted reliever is listed RP and is still starting,
        # and dropping him costs the whole *opposing* lineup its projection,
        # because that projection is conditioned on this pitcher's hand.
        flagged = match[match["is_starter"]]
        if not flagged.empty:
            match = flagged
        if match.empty:
            log.warning("no draftable pitcher matches probable %s (%s)", surname, team)
            continue
        best = match.sort_values("salary", ascending=False).iloc[0]
        rows.append(
            {
                "team": team,
                "dk_id": str(best["dk_id"]),
                "name": best["name"],
                "throws": hand or best["throws"],
            }
        )
    return pd.DataFrame(rows)


def to_contest(row: pd.Series, payouts: pd.DataFrame) -> Contest:
    """Build the optimizer's :class:`Contest` from a lobby row and its bands."""
    bands = [
        (int(r.rank_from), int(r.rank_to), float(r.prize))
        for r in payouts.itertuples()
    ]
    return Contest(
        name=str(row["name"]),
        entry_fee=float(row["entry_fee"]),
        n_entries=int(row["max_entries"]),
        payouts=bands,
        max_entries_per_user=int(row["max_entries_per_user"] or 1),
    )
