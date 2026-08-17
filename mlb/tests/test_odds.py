"""Vegas lines to implied team totals."""

from __future__ import annotations

import pandas as pd
import pytest

from mlbdfs.data import odds

EVENT = {
    "home_team": "Toronto Blue Jays",
    "away_team": "New York Yankees",
    "commence_time": "2026-08-16T17:37:00Z",
    "bookmakers": [
        {"key": "a", "markets": [
            {"key": "totals", "outcomes": [
                {"name": "Over", "point": 8.5}, {"name": "Under", "point": 8.5}]},
            {"key": "h2h", "outcomes": [
                {"name": "Toronto Blue Jays", "price": -150},
                {"name": "New York Yankees", "price": 130}]},
        ]},
        {"key": "b", "markets": [
            {"key": "totals", "outcomes": [
                {"name": "Over", "point": 9.5}, {"name": "Under", "point": 9.5}]},
            {"key": "h2h", "outcomes": [
                {"name": "Toronto Blue Jays", "price": -140},
                {"name": "New York Yankees", "price": 120}]},
        ]},
    ],
}


def test_american_to_probability():
    assert odds.american_to_probability(-150) == pytest.approx(0.6, abs=1e-6)
    assert odds.american_to_probability(150) == pytest.approx(0.4, abs=1e-6)
    assert odds.american_to_probability(100) == pytest.approx(0.5, abs=1e-6)


def test_devig_sums_to_one():
    home, away = odds.devig(0.60, 0.45)
    assert home + away == pytest.approx(1.0)
    assert home > away


def test_team_totals_split_the_game_total():
    frame = odds.implied_team_totals([EVENT])
    assert set(frame["team"]) == {"TOR", "NYY"}
    # Books are averaged: 8.5 and 9.5 -> 9.0.
    assert frame["game_total"].unique().tolist() == [9.0]
    # The two team totals must add back to the game total exactly.
    assert frame["total"].sum() == pytest.approx(9.0)
    tor = frame.set_index("team").loc["TOR"]
    nyy = frame.set_index("team").loc["NYY"]
    # Toronto is the favourite, so it gets the larger share.
    assert tor["total"] > nyy["total"]
    assert tor["win_probability"] > 0.5
    assert frame["source"].unique().tolist() == ["derived"]


def test_a_pick_em_game_splits_evenly():
    event = {
        "home_team": "Chicago Cubs", "away_team": "St. Louis Cardinals",
        "bookmakers": [{"key": "a", "markets": [
            {"key": "totals", "outcomes": [{"name": "Over", "point": 9.0}]},
            {"key": "h2h", "outcomes": [
                {"name": "Chicago Cubs", "price": -110},
                {"name": "St. Louis Cardinals", "price": -110}]},
        ]}],
    }
    frame = odds.implied_team_totals([event]).set_index("team")
    assert frame.loc["CHC", "total"] == pytest.approx(4.5, abs=1e-6)
    assert frame.loc["STL", "total"] == pytest.approx(4.5, abs=1e-6)


def test_a_published_team_total_beats_the_derived_one():
    """When a book posts team totals, no inference is needed or wanted."""
    event = dict(EVENT)
    event["bookmakers"] = EVENT["bookmakers"] + [{
        "key": "c", "markets": [{"key": "team_totals", "outcomes": [
            {"name": "Over", "description": "Toronto Blue Jays", "point": 5.2},
            {"name": "Over", "description": "New York Yankees", "point": 3.9},
        ]}]},
    ]
    frame = odds.implied_team_totals([event]).set_index("team")
    assert frame["source"].unique().tolist() == ["team_totals"]
    assert frame.loc["TOR", "total"] == pytest.approx(5.2)
    assert frame.loc["NYY", "total"] == pytest.approx(3.9)


def test_an_event_with_no_total_is_skipped_not_guessed():
    event = {"home_team": "Chicago Cubs", "away_team": "St. Louis Cardinals",
             "bookmakers": [{"key": "a", "markets": [
                 {"key": "h2h", "outcomes": [
                     {"name": "Chicago Cubs", "price": -110},
                     {"name": "St. Louis Cardinals", "price": -110}]}]}]}
    assert odds.implied_team_totals([event]).empty


def test_unmapped_team_names_are_skipped():
    event = {"home_team": "Springfield Isotopes", "away_team": "Chicago Cubs",
             "bookmakers": []}
    assert odds.implied_team_totals([event]).empty


def test_missing_key_names_the_environment_variable():
    import os

    saved = os.environ.pop("ODDS_API_KEY", None)
    try:
        with pytest.raises(odds.MissingOddsKey, match="ODDS_API_KEY"):
            odds._api_key()
    finally:
        if saved is not None:
            os.environ["ODDS_API_KEY"] = saved


def test_totals_map_is_the_shape_build_slate_wants():
    frame = odds.implied_team_totals([EVENT])
    mapping = odds.totals_map(frame)
    assert set(mapping) == {"TOR", "NYY"}
    assert all(isinstance(v, float) for v in mapping.values())
    assert odds.totals_map(pd.DataFrame()) == {}


def test_a_night_game_belongs_to_the_eastern_day_it_is_played_on():
    """A 20:05 ET first pitch is stamped 00:05Z the next day.

    Comparing the UTC date prefix to the slate date silently drops those
    games, and their teams then get the league-average run environment with
    no warning. Measured on draft group 152195 it cost 2 of 7 games.
    """
    from datetime import date

    assert odds.eastern_date("2026-08-18T00:06:00Z") == date(2026, 8, 17)
    assert odds.eastern_date("2026-08-18T00:41:00Z") == date(2026, 8, 17)
    # An afternoon game stays on its own UTC day.
    assert odds.eastern_date("2026-08-17T17:37:00Z") == date(2026, 8, 17)
    # And the small hours really are the next day.
    assert odds.eastern_date("2026-08-18T13:05:00Z") == date(2026, 8, 18)


def test_eastern_date_tolerates_junk():
    assert odds.eastern_date("") is None
    assert odds.eastern_date("not a timestamp") is None


def test_the_requested_window_covers_the_whole_eastern_day():
    """The window must reach past midnight UTC or it reintroduces the bug."""
    from datetime import date, datetime, timedelta
    from zoneinfo import ZoneInfo

    start = datetime.combine(date(2026, 8, 17), datetime.min.time(),
                             tzinfo=odds.EASTERN)
    assert odds._utc_stamp(start) == "2026-08-17T04:00:00Z"
    assert odds._utc_stamp(start + timedelta(days=1)) == "2026-08-18T04:00:00Z"
    # Every game on the 17th ET falls inside it.
    for stamp in ("2026-08-17T22:40:00Z", "2026-08-18T00:41:00Z"):
        moment = datetime.fromisoformat(stamp.replace("Z", "+00:00"))
        assert start <= moment < start + timedelta(days=1)
        assert odds.eastern_date(stamp) == date(2026, 8, 17)


def test_eastern_date_rejects_a_missing_or_broken_timestamp():
    assert odds.eastern_date("") is None
    assert odds.eastern_date("not a time") is None


def test_eastern_date_puts_an_afternoon_game_on_its_own_day():
    """The mirror of the night-game case: no off-by-one the other way."""
    from datetime import date

    assert odds.eastern_date("2026-08-18T17:05:00Z") == date(2026, 8, 18)
