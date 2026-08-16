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

   ``implied_team_totals`` reads this market, but nothing currently feeds it
   one: The Odds API serves ``team_totals`` only from the per-event endpoint
   (``EVENT_ODDS_URL``), and ``fetch_team_totals`` calls the bulk
   ``/odds`` endpoint, which silently ignores the market. So in practice
   every run takes path 2. The market *is* available on this key -- it is
   what ``FAVOURITE_RUN_SHARE`` was calibrated against -- at a cost of one
   request per event rather than one per slate.
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
from datetime import date, datetime, timedelta
from zoneinfo import ZoneInfo

import pandas as pd

from .cache import cached_frame

log = logging.getLogger(__name__)

# A slate date is an Eastern-time calendar date -- that is what DraftKings
# means by it and what the user types. The Odds API stamps `commence_time` in
# UTC, where a night game rolls over to the next day: a 20:05 ET first pitch
# is 00:05Z tomorrow. Comparing the UTC prefix to the slate date therefore
# drops every game starting at or after 8pm ET, which is most of a west-coast
# slate and, measured on draft group 152195, 2 of 7 games on an ordinary
# evening one. Nothing looks wrong when it happens -- those teams just quietly
# fall back to the league-average run environment.
EASTERN = ZoneInfo("America/New_York")

ODDS_URL = "https://api.the-odds-api.com/v4/sports/baseball_mlb/odds"
EVENT_ODDS_URL = (
    "https://api.the-odds-api.com/v4/sports/baseball_mlb/events/{event_id}/odds"
)

# How much of a game's total the favourite is expected to score, per unit of
# win probability above even. A coin-flip game splits 50/50; at a 65% favourite
# this puts roughly 53.5% of the total on the favourite.
#
# It is a linear approximation to something that is not linear, and it is the
# weakest link in this module. When `team_totals` is available it is not used
# at all.
#
# **Measured against real published team totals, this is about half of what
# the market prices, and it has not been changed yet.** On 14 games of
# 2026-08-16 with two-sided `team_totals` quotes, inverting the de-vigged
# over/under at each posted line for the mean that implies it (negative
# binomial, var/mean 1.5) and regressing the recovered run share on the
# de-vigged win probability gives 0.488, with a correlation of 0.992 and a
# share MAE of 0.008 against 0.024 here. The estimate is insensitive to the
# dispersion assumption -- Poisson gives 0.508, var/mean 2.0 gives 0.471 --
# so the range is 0.47 to 0.51, not 0.235.
#
# Six of those 14 games were already ~30 minutes old at the snapshot, which
# attenuates the estimate: a team leading in the third carries an inflated
# win probability against a run share it has partly already banked. Splitting
# on it confirms the direction -- the 8 games not yet started fit 0.512 with
# a correlation of 0.996, the 6 live ones 0.481. The clean subset is the
# higher number, so 0.235 is if anything further off than the pooled fit says.
#
# The practical effect is that the run gap between favourite and underdog
# comes out roughly half its market value, worst on the lopsided games where
# it matters most: CWS at DET priced a 2.35-run gap against 1.27 here, BOS at
# PIT 1.64 against 0.79. Raising it is a change to every projection on every
# slate, so it wants its own measurement on more than one day's card before
# it lands -- and if the `team_totals` path below were reachable through the
# bulk endpoint the constant would matter far less. See docs/DESIGN.md.
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


def eastern_date(commence_time: str) -> date | None:
    """The slate date an event belongs to, from its ISO UTC start time."""
    if not commence_time:
        return None
    try:
        stamp = datetime.fromisoformat(str(commence_time).replace("Z", "+00:00"))
    except ValueError:
        log.warning("unparseable commence_time %r", commence_time)
        return None
    if stamp.tzinfo is None:
        stamp = stamp.replace(tzinfo=ZoneInfo("UTC"))
    return stamp.astimezone(EASTERN).date()


def _utc_stamp(moment: datetime) -> str:
    """The API wants ``YYYY-MM-DDTHH:MM:SSZ``, unpunctuated by offsets."""
    return moment.astimezone(ZoneInfo("UTC")).strftime("%Y-%m-%dT%H:%M:%SZ")


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

        params = {
            "apiKey": _api_key(),
            "regions": "us",
            "markets": markets,
            "oddsFormat": "american",
            "dateFormat": "iso",
        }
        if game_date is not None:
            # Ask for the Eastern day explicitly rather than trusting the
            # endpoint's default "upcoming" window, which is capped and would
            # truncate a late slate without saying so.
            start = datetime.combine(
                game_date, datetime.min.time(), tzinfo=EASTERN
            )
            params["commenceTimeFrom"] = _utc_stamp(start)
            params["commenceTimeTo"] = _utc_stamp(start + timedelta(days=1))

        response = requests.get(ODDS_URL, params=params, timeout=30)
        response.raise_for_status()
        remaining = response.headers.get("x-requests-remaining")
        if remaining is not None:
            log.info("odds api requests remaining: %s", remaining)
        events = response.json()
        if game_date is not None:
            events = [
                e for e in events
                if eastern_date(e.get("commence_time", "")) == game_date
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
