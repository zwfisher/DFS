"""Contest screening and the DraftKings lobby parser.

The lobby fixture below is trimmed from a real payload. Two of its field
names are load bearing and easy to get backwards -- ``m`` is the field size
cap and ``nt`` is entries so far -- so the first test pins the mapping by
checking that rake comes out at DraftKings' published number. If someone
swaps them, every rake in the screen inverts and nothing else notices.
"""

from __future__ import annotations

import pandas as pd
import pytest

from mlbdfs.data import lobby
from mlbdfs.optimize import screen
from mlbdfs.optimize.contest import Contest, double_up, large_gpp

LOBBY = {
    "DraftGroups": [
        {
            "DraftGroupId": 152178,
            "StartDateEst": "2026-08-16T13:35:00.0000000",
            "GameCount": 8,
            "GameTypeId": 2,
            "DraftGroupTag": "Featured",
            "ContestStartTimeSuffix": None,
        },
        {
            "DraftGroupId": 152185,
            "StartDateEst": "2026-08-16T16:05:00.0000000",
            "GameCount": 4,
            "GameTypeId": 2,
            "DraftGroupTag": None,
            "ContestStartTimeSuffix": " (Afternoon)",
        },
        {
            "DraftGroupId": 152188,
            "StartDateEst": "2026-08-16T19:20:00.0000000",
            "GameCount": 1,
            "GameTypeId": 114,
            "DraftGroupTag": "Featured",
            "ContestStartTimeSuffix": " (SEA @ HOU)",
        },
    ],
    "Contests": [
        {
            "id": 193832694,
            "n": "MLB $15K mini-MAX [150 Entry Max]",
            "a": 0.5,
            "m": 35671,
            "nt": 571,
            "po": 15000.0,
            "mec": 150,
            "dg": 152178,
            "gameTypeId": 2,
            "gameType": "Classic",
            "sdstring": "Sun 1:35PM",
            "tix": False,
            "isSnakeDraft": False,
            "attr": {"IsGuaranteed": "true"},
        },
        {
            "id": 193832709,
            "n": "MLB $2 Double Up",
            "a": 2.0,
            "m": 862,
            "nt": 63,
            "po": 1500.0,
            "mec": 20,
            "dg": 152178,
            "gameTypeId": 2,
            "gameType": "Classic",
            "sdstring": "Sun 1:35PM",
            "tix": False,
            "isSnakeDraft": False,
            "attr": {"IsGuaranteed": "true"},
        },
        {
            "id": 193832800,
            "n": "MLB $100K Heavy Hitter [Must Fill]",
            "a": 26200.0,
            "m": 4,
            "nt": 4,
            "po": 100000.0,
            "mec": 1,
            "dg": 152185,
            "gameTypeId": 2,
            "gameType": "Classic",
            "sdstring": "Sun 4:05PM",
            "tix": False,
            "isSnakeDraft": False,
            "attr": {},
        },
    ],
}


def test_draft_groups_sorted_by_start():
    groups = lobby.draft_groups(LOBBY)
    assert list(groups["draft_group_id"]) == [152178, 152185, 152188]
    assert groups.iloc[0]["games"] == 8


def test_main_slate_is_the_largest_classic_group():
    import datetime as dt

    picked = lobby.main_slate(LOBBY, dt.date(2026, 8, 16))
    assert picked["draft_group_id"] == 152178

    # The one-game Showdown group is larger by contest count but is not a
    # Classic slate, so it must never be picked.
    assert picked["game_type_id"] == 2


def test_lobby_field_mapping_reproduces_published_rake():
    contests = lobby.lobby_contests(LOBBY, 152178)
    mini = contests[contests["contest_id"] == 193832694].iloc[0]
    # 35,671 x $0.50 = $17,835.50 collected against a $15,000 pool.
    assert mini["max_entries"] == 35671
    assert mini["current_entries"] == 571
    scored = screen.screen(contests)
    rake = scored.loc[scored["contest_id"] == 193832694, "rake"].iloc[0]
    assert rake == pytest.approx(0.159, abs=0.001)


def test_breakeven_share_is_the_reciprocal_of_the_kept_fraction():
    scored = screen.screen(lobby.lobby_contests(LOBBY, 152178))
    row = scored[scored["contest_id"] == 193832694].iloc[0]
    assert row["breakeven_share"] == pytest.approx(1.0 / (1.0 - row["rake"]))


def test_overlay_only_counts_guaranteed_contests():
    contests = lobby.lobby_contests(LOBBY)
    scored = screen.screen(contests)
    guaranteed = scored[scored["contest_id"] == 193832709].iloc[0]
    assert guaranteed["overlay_now"] == pytest.approx(1500.0 - 2.0 * 63)
    unguaranteed = scored[scored["contest_id"] == 193832800].iloc[0]
    assert unguaranteed["overlay_now"] == 0.0


def test_overlay_never_goes_negative_on_an_overcollected_contest():
    row = pd.Series(
        {"guaranteed": True, "entry_fee": 1.0, "current_entries": 5000,
         "prize_pool": 4000.0}
    )
    assert screen.overlay(row) == 0.0


def test_payout_shape_separates_a_double_up_from_a_tournament():
    flat = screen.payout_shape(double_up(10_000, 5.0))
    gpp = screen.payout_shape(large_gpp(100_000, 5.0))

    assert flat["pay_rate"] > 0.4 and gpp["pay_rate"] < 0.25
    assert flat["min_cash_multiple"] > 1.7
    assert gpp["top_share"] > 10 * flat["top_share"]
    assert gpp["payout_cv"] > flat["payout_cv"]


def test_edge_return_rises_with_the_edge():
    contest = large_gpp(50_000, 5.0)
    returns = [screen.edge_return(contest, k) for k in (1.0, 0.9, 0.5, 0.2)]
    assert returns == sorted(returns)
    # No edge at all leaves you paying exactly the rake.
    assert returns[0] == pytest.approx(-contest.rake, abs=0.01)


def test_breakeven_is_harder_in_a_top_heavy_contest():
    """The headline claim of the module, on curves with equal rake."""
    pool = 100_000 * 5.0 * 0.85
    flat = Contest("flat", 5.0, 100_000, [(1, 44_000, pool / 44_000)])
    steep = Contest(
        "steep",
        5.0,
        100_000,
        [(1, 1, pool * 0.5), (2, 100, pool * 0.5 / 99)],
    )
    assert flat.rake == pytest.approx(steep.rake, abs=1e-6)
    assert screen.breakeven_rank_factor(steep) < screen.breakeven_rank_factor(flat)


def test_breakeven_rank_factor_is_a_root_of_edge_return():
    contest = large_gpp(20_000, 3.0)
    k = screen.breakeven_rank_factor(contest)
    assert screen.edge_return(contest, k) == pytest.approx(0.0, abs=0.02)


def test_edge_sharpe_is_far_worse_in_the_top_heavy_contest():
    """Same edge, same rake: the tournament needs a vastly larger sample."""
    flat = double_up(10_000, 5.0)
    gpp = large_gpp(10_000, 5.0)
    assert screen.edge_sharpe(gpp, 0.5) < screen.edge_sharpe(flat, 0.5)


def test_screen_of_an_empty_frame_is_empty():
    assert screen.screen(pd.DataFrame()).empty


def test_probable_starters_reads_the_opposing_tag():
    draftables = pd.DataFrame(
        [
            {"dk_id": "1", "name": "Dylan Cease", "team": "NYY", "opponent": "TOR",
             "salary": 10800, "is_starter": True, "is_pitcher": True, "throws": "R",
             "opponent_starter": "Weathers", "opponent_starter_hand": "L"},
            {"dk_id": "2", "name": "Ryan Weathers", "team": "TOR", "opponent": "NYY",
             "salary": 7600, "is_starter": True, "is_pitcher": True, "throws": "L",
             "opponent_starter": "Cease", "opponent_starter_hand": "R"},
            {"dk_id": "3", "name": "Aaron Judge", "team": "NYY", "opponent": "TOR",
             "salary": 6000, "is_starter": False, "is_pitcher": False, "throws": "R",
             "opponent_starter": "Weathers", "opponent_starter_hand": "L"},
        ]
    )
    got = lobby.probable_starters(draftables).set_index("team")
    assert got.loc["NYY", "name"] == "Dylan Cease"
    assert got.loc["NYY", "throws"] == "R"
    assert got.loc["TOR", "name"] == "Ryan Weathers"
    assert got.loc["TOR", "throws"] == "L"


def test_probable_starters_prefers_an_sp_over_a_same_named_reliever():
    draftables = pd.DataFrame(
        [
            {"dk_id": "1", "name": "Zack Wheeler", "team": "PHI", "opponent": "MIN",
             "salary": 10200, "is_starter": True, "is_pitcher": True, "throws": "R",
             "opponent_starter": "Kremer", "opponent_starter_hand": "R"},
            {"dk_id": "2", "name": "Dean Kremer", "team": "MIN", "opponent": "PHI",
             "salary": 7000, "is_starter": True, "is_pitcher": True, "throws": "R",
             "opponent_starter": "Wheeler", "opponent_starter_hand": "R"},
            {"dk_id": "3", "name": "Jake Kremer", "team": "MIN", "opponent": "PHI",
             "salary": 9000, "is_starter": False, "is_pitcher": True, "throws": "L",
             "opponent_starter": "Wheeler", "opponent_starter_hand": "R"},
        ]
    )
    got = lobby.probable_starters(draftables).set_index("team")
    assert got.loc["MIN", "name"] == "Dean Kremer"


def test_probable_starters_accepts_a_reliever_starting_a_bullpen_game():
    """DraftKings lists the opener as RP; he is still today's starter.

    Dropping him is not a small loss -- the *opposing* team's projected
    lineup is conditioned on this pitcher's hand, so an unmatched starter
    costs nine hitters their batting order.
    """
    draftables = pd.DataFrame(
        [
            {"dk_id": "1", "name": "Cam Schlittler", "team": "NYY", "opponent": "TOR",
             "salary": 11000, "is_starter": True, "is_pitcher": True, "throws": "R",
             "opponent_starter": "Cease", "opponent_starter_hand": "R"},
            {"dk_id": "2", "name": "Dylan Cease", "team": "TOR", "opponent": "NYY",
             "salary": 10800, "is_starter": True, "is_pitcher": True, "throws": "R",
             "opponent_starter": "Weathers", "opponent_starter_hand": "L"},
            {"dk_id": "3", "name": "Ryan Weathers", "team": "NYY", "opponent": "TOR",
             "salary": 8500, "is_starter": False, "is_pitcher": True, "throws": "L",
             "opponent_starter": "Cease", "opponent_starter_hand": "R"},
        ]
    )
    got = lobby.probable_starters(draftables).set_index("team")
    assert got.loc["NYY", "name"] == "Ryan Weathers"
    assert got.loc["NYY", "throws"] == "L"


def test_to_contest_carries_the_published_bands():
    contests = lobby.lobby_contests(LOBBY, 152178)
    row = contests[contests["contest_id"] == 193832709].iloc[0]
    bands = pd.DataFrame(
        [{"rank_from": 1, "rank_to": 375, "prize": 4.0}]
    )
    contest = lobby.to_contest(row, bands)
    assert contest.n_entries == 862
    assert contest.max_entries_per_user == 20
    assert contest.total_prizes == pytest.approx(1500.0)


def test_compare_contests_prices_one_portfolio_into_several_curves():
    """The same lineups are worth different amounts in different contests."""
    from mlbdfs.data.fixtures import make_slate
    from mlbdfs.pipeline import run_pipeline

    slate, book = make_slate(n_games=4, seed=3)
    result = run_pipeline(
        slate, book, n_sims=400, n_field=600, n_candidates=12, n_lineups=4,
        seed=3, verbose=False, allow_unconfirmed=True,
    )
    table = screen.compare_contests(
        result, [double_up(2_000, 5.0), large_gpp(2_000, 5.0)]
    )

    assert list(table["contest"]) != []
    assert set(table["contest"]) == {"double_up", "large_gpp"}
    # Only the portfolio, not the whole candidate pool. Exposure caps can
    # cut the portfolio short of the requested count, so compare to what
    # was actually selected.
    assert len(result.selected) < len(result.pool)
    assert (table["cost"] == len(result.selected) * 5.0).all()
    # A flat contest cashes far more often than a top-heavy one.
    flat = table.set_index("contest").loc["double_up"]
    gpp = table.set_index("contest").loc["large_gpp"]
    assert flat["p_cash"] > gpp["p_cash"]
