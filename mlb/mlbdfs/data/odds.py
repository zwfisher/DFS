"""Vegas lines from The Odds API, turned into implied team totals.

The team total is the single most informative public number about how many
runs a lineup will score, and it carries things the rate model cannot see:
weather, wind, umpire, bullpen availability, a late scratch the market has
already priced. ``projections.matchup.scale_to_team_total`` has been ready
for it since the beginning; this module supplies it.

**The key never appears in code.** Set ``ODDS_API_KEY`` in the environment.

Two paths to a team total, in order of preference:

1. **The ``team_totals`` market**, when the account's plan exposes it for an
   event. This is the real thing -- a bookmaker's own number, no inference.
2. **Derived from the game total and the moneyline.** Every plan has these.
   The game total says how many runs the two teams combine for and the
   moneyline says who is better; splitting the first by the second gives a
   team total. This is an approximation and is labelled as one, because the
   mapping from win probability to run share is fitted folklore rather than
   an identity -- see ``FAVOURITE_RUN_SHARE``.
"""

from __future__ import annotations

import logging
import os
from datetime import date, timedelta

import pandas as pd

from .cache import cached_frame

log = logging.getLogger(__name__)

ODDS_URL = "https://api.the-odds-api.com/v4/sports/baseball_mlb/odds"
EVENT_ODDS_URL = (
    "https://api.the-odds-api.com/v4/sports/baseball_mlb/events/{event_id}/odds"
)

# How much of a game's total the favourite is expected to score, per unit of
# win probability above even. A coin-flip game splits 50/50; at a 65% favourite
# this puts roughly 53.5% of the total on the favourite, which is about a
# 0.6-run edge on a 9-run total and matches the run lines MLB books post.
#
# It is a linear approximation to something that is not linear, and it is the
# weakest link in this module. When `team_totals` is available it is not used
# at all.
FAVOURITE_RUN_SHARE = 0.235

# The Odds API names teams in full ("Arizona Diamondbacks"); DraftKings uses
# abbreviations. Only the mapping is needed, not the nicknames.
TEAM_CODES = {
    "Arizona Diamondbacks": "ARI", "Atlanta Braves": "ATL",
    "Baltimore Orioles": "BAL", "Boston Red Sox": "BOS",
    "Chicago Cubs": "CHC", "Chicago White Sox": "CWS",
    "Cincinnati Reds": "CIN", "Cleveland Guardians": "CLE",
    "Colorado Rockies": "COL", "Detroit Tigers": "DET",
    "Houston Astros": "HOU", "Kansas City Royals": "KC",
    "Los Angeles Angels": "LAA", "Los Angeles Dodgers": "LAD",
    "Miami Marlins": "MIA", "Milwaukee Brewers": "MIL",
    "Minnesota Twins": "MIN", "New York Mets": "NYM",
    "New York Yankees": "NYY", "Athletics": "ATH", "Oakland Athletics": "ATH",
    "Philadelphia Phillies": "PHI", "Pittsburgh Pirates": "PIT",
    "San Diego Padres": "SD", "San Francisco Giants": "SF",
    "Seattle Mariners": "SEA", "St. Louis Cardinals": "STL",
    "Tampa Bay Rays": "TB", "Texas Rangers": "TEX",
    "Toronto Blue Jays": "TOR", "Washington Nationals": "WSH",
}


class MissingOddsKey(RuntimeError):
    """Raised when ODDS_API_KEY is not set."""


def _api_key() -> str:
    key = os.environ.get("ODDS_API_KEY", "").strip()
    if not key:
        raise MissingOddsKey(
            "ODDS_API_KEY is not set. Add it to this environment's variables "
            "rather than passing it on the command line, so it stays out of "
            "shell history and transcripts."
        )
    return key


def american_to_probability(price: float) -> float:
    """Implied probability from an American moneyline, vig included."""
    price = float(price)
    if price < 0:
        return -price / (-price + 100.0)
    return 100.0 / (price + 100.0)


def devig(home: float, away: float) -> tuple[float, float]:
    """Strip the bookmaker's margin, proportionally.

    Two implied probabilities from a two-way market sum to more than one --
    that excess is the vig. Normalizing is the standard first-order removal;
    it slightly overcorrects the favourite, which is a known and small bias.
    """
    total = home + away
    if total <= 0:
        return 0.5, 0.5
    return home / total, away / total


def implied_team_totals(events: list[dict]) -> pd.DataFrame:
    """``team, total, game_total, win_probability, source`` per team.

    Bookmakers are averaged rather than picked, which is a cheap and robust
    way to avoid one stale book dragging a number around.
    """
    rows = []
    for event in events:
        home = TEAM_CODES.get(event.get("home_team", ""))
        away = TEAM_CODES.get(event.get("away_team", ""))
        if not home or not away:
            log.warning("unmapped team in %s @ %s",
                        event.get("away_team"), event.get("home_team"))
            continue

        totals, home_prices, away_prices = [], [], []
        direct: dict[str, list[float]] = {}
        for book in event.get("bookmakers", []):
            for market in book.get("markets", []):
                key = market.get("key")
                outcomes = market.get("outcomes", [])
                if key == "totals" and outcomes:
                    point = outcomes[0].get("point")
                    if point:
                        totals.append(float(point))
                elif key == "h2h":
                    for out in outcomes:
                        code = TEAM_CODES.get(out.get("name", ""))
                        if code == home:
                            home_prices.append(american_to_probability(out["price"]))
                        elif code == away:
                            away_prices.append(american_to_probability(out["price"]))
                elif key == "team_totals":
                    for out in outcomes:
                        code = TEAM_CODES.get(out.get("description") or
                                              out.get("name", ""))
                        point = out.get("point")
                        if code and point and out.get("name") == "Over":
                            direct.setdefault(code, []).append(float(point))

        if not totals:
            log.warning("no game total for %s @ %s", away, home)
            continue
        game_total = sum(totals) / len(totals)

        if direct.get(home) and direct.get(away):
            for code in (away, home):
                rows.append({
                    "team": code,
                    "total": sum(direct[code]) / len(direct[code]),
                    "game_total": game_total,
                    "win_probability": float("nan"),
                    "source": "team_totals",
                })
            continue

        if home_prices and away_prices:
            p_home, p_away = devig(
                sum(home_prices) / len(home_prices),
                sum(away_prices) / len(away_prices),
            )
        else:
            p_home = p_away = 0.5

        home_share = 0.5 + FAVOURITE_RUN_SHARE * (p_home - 0.5)
        rows.append({"team": away, "total": game_total * (1.0 - home_share),
                     "game_total": game_total, "win_probability": p_away,
                     "source": "derived"})
        rows.append({"team": home, "total": game_total * home_share,
                     "game_total": game_total, "win_probability": p_home,
                     "source": "derived"})

    return pd.DataFrame(rows)


def fetch_team_totals(
    game_date: date | None = None,
    ttl_minutes: float = 30.0,
    markets: str = "totals,h2h",
    force: bool = False,
) -> pd.DataFrame:
    """Implied team totals for the day's MLB games.

    Cached with a short TTL: lines move, and the ones that move most are the
    ones a late scratch just repriced.
    """

    def fetch() -> pd.DataFrame:
        import requests

        response = requests.get(
            ODDS_URL,
            params={
                "apiKey": _api_key(),
                "regions": "us",
                "markets": markets,
                "oddsFormat": "american",
                "dateFormat": "iso",
            },
            timeout=30,
        )
        response.raise_for_status()
        remaining = response.headers.get("x-requests-remaining")
        if remaining is not None:
            log.info("odds api requests remaining: %s", remaining)
        events = response.json()
        if game_date is not None:
            events = [
                e for e in events
                if str(e.get("commence_time", ""))[:10] == game_date.isoformat()
            ]
        return implied_team_totals(events)

    return cached_frame(
        "odds_team_totals",
        fetch,
        ttl=timedelta(minutes=ttl_minutes),
        force=force,
        game_date=game_date.isoformat() if game_date else "all",
        markets=markets,
    )


def totals_map(frame: pd.DataFrame) -> dict[str, float]:
    """``{team: implied runs}``, the shape ``build_slate`` wants."""
    if frame.empty:
        return {}
    return dict(zip(frame["team"], frame["total"].astype(float)))
