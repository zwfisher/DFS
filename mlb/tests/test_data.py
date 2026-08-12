"""Data layer: rate shrinkage, DraftKings parsing and standings logging.

Nothing here touches the network.
"""

from __future__ import annotations

import numpy as np
import pandas as pd
import pytest

from mlbdfs.config import OUTCOMES, STABILIZATION_PA
from mlbdfs.data.dk import _match_key, build_slate, normalize_team, parse_salaries
from mlbdfs.ownership.logger import parse_standings, realized_ownership
from mlbdfs.projections.rates import league_rate_vector, pool_seasons, shrink


def _counts(player_id, pa, **outcomes):
    row = {"player_id": player_id, "season": 2026, "pa": pa}
    row.update({name: 0 for name in OUTCOMES})
    row.update(outcomes)
    row["field_out"] = pa - sum(v for k, v in outcomes.items())
    return row


def test_shrinkage_pulls_small_samples_toward_league():
    """Four home runs in forty plate appearances is not a 10% home run rate."""
    small = pd.DataFrame([_counts("hot", 40, home_run=4)])
    rates = shrink(small)
    league = league_rate_vector()[OUTCOMES.index("home_run")]
    observed = 4 / 40

    estimate = rates["home_run"].iloc[0]
    assert league < estimate < observed
    assert estimate < 0.05  # much closer to the league than to the sample


def test_large_samples_move_off_the_prior():
    big = pd.DataFrame([_counts("slugger", 3000, home_run=210)])
    rates = shrink(big)
    league = league_rate_vector()[OUTCOMES.index("home_run")]
    assert rates["home_run"].iloc[0] > league * 1.7


def test_fast_stabilizing_stats_shrink_less():
    """Strikeout rate stabilizes far faster than home run rate, so at equal
    sample it should sit closer to what was observed."""
    assert STABILIZATION_PA["strikeout"] < STABILIZATION_PA["home_run"]

    sample = pd.DataFrame([_counts("x", 150, strikeout=60, home_run=15)])
    rates = shrink(sample).iloc[0]
    lg = league_rate_vector()

    k_pull = abs(rates["strikeout"] - 60 / 150) / abs(
        lg[OUTCOMES.index("strikeout")] - 60 / 150
    )
    hr_pull = abs(rates["home_run"] - 15 / 150) / abs(
        lg[OUTCOMES.index("home_run")] - 15 / 150
    )
    assert k_pull < hr_pull


def test_shrunk_rates_are_valid_distributions():
    frame = pd.DataFrame(
        [
            _counts("a", 500, strikeout=110, walk=50, single=70, home_run=20),
            _counts("b", 12, strikeout=6, home_run=2),
            _counts("c", 0),
        ]
    )
    rates = shrink(frame)
    totals = rates[list(OUTCOMES)].sum(axis=1)
    assert np.allclose(totals, 1.0)
    assert (rates[list(OUTCOMES)] >= 0).all().all()


def test_empty_sample_returns_league_average():
    rates = shrink(pd.DataFrame([_counts("rookie", 0)]))
    assert np.allclose(
        rates[list(OUTCOMES)].iloc[0].to_numpy(), league_rate_vector(), atol=1e-9
    )


def test_recent_seasons_are_weighted_more_heavily():
    frame = pd.DataFrame(
        [
            _counts("p", 600, home_run=10) | {"season": 2024},
            _counts("p", 600, home_run=40) | {"season": 2026},
        ]
    )
    pooled = pool_seasons(frame)
    # The recent, higher-power season should dominate the pooled rate.
    assert pooled["home_run"].iloc[0] / pooled["pa"].iloc[0] > 25 / 600


def test_missing_columns_are_reported():
    with pytest.raises(ValueError, match="missing columns"):
        shrink(pd.DataFrame({"player_id": ["a"], "pa": [10]}))


# --------------------------------------------------------------------------
# DraftKings parsing
# --------------------------------------------------------------------------

DK_CSV = """Position,Name + ID,Name,ID,Roster Position,Salary,Game Info,TeamAbbrev,AvgPointsPerGame
SP,Ace Pitcher (100),Ace Pitcher,100,P,10500,NYY@BOS 07/04/2026 07:10PM ET,BOS,19.2
C,Backstop Guy (101),Backstop Guy,101,C,4200,NYY@BOS 07/04/2026 07:10PM ET,NYY,8.1
2B/SS,Middle Infield (102),Middle Infield,102,2B/SS,5100,NYY@BOS 07/04/2026 07:10PM ET,NYY,9.4
OF,Outfield One (103),Outfield One,103,OF,3800,NYY@BOS 07/04/2026 07:10PM ET,BOS,7.7
RP,Relief Arm (104),Relief Arm,104,P,5200,NYY@BOS 07/04/2026 07:10PM ET,NYY,6.0
"""


def test_parse_salaries(tmp_path):
    path = tmp_path / "dk.csv"
    path.write_text(DK_CSV)
    frame = parse_salaries(path)

    assert len(frame) == 5
    assert set(frame["team"]) == {"NYY", "BOS"}
    assert (frame["away"] == "NYY").all()
    assert (frame["home"] == "BOS").all()

    # Opponent is derived from the game, not the file.
    bos = frame[frame["team"] == "BOS"].iloc[0]
    assert bos["opponent"] == "NYY"

    # Starters and relievers both fill the single P slot.
    assert frame[frame["name"] == "Ace Pitcher"]["positions"].iloc[0] == ("P",)
    assert frame[frame["name"] == "Relief Arm"]["positions"].iloc[0] == ("P",)
    assert frame["is_pitcher"].sum() == 2

    # Multi-eligibility survives parsing.
    assert set(frame[frame["name"] == "Middle Infield"]["positions"].iloc[0]) == {
        "2B",
        "SS",
    }


def test_build_slate_attaches_batting_order(tmp_path):
    path = tmp_path / "dk.csv"
    path.write_text(DK_CSV)
    salaries = parse_salaries(path)
    lineups = pd.DataFrame(
        [{"name": "Backstop Guy", "team": "NYY", "batting_order": 3}]
    )

    slate = build_slate(salaries, lineups=lineups, starters={"BOS": "100"})
    catcher = slate.player("101")
    assert catcher.batting_order == 3
    assert catcher.confirmed
    assert slate.games[0].home_starter == "100"
    assert not slate.player("103").confirmed


def test_name_matching_handles_accents_and_suffixes():
    assert _match_key("José Ramírez") == _match_key("Jose Ramirez")
    assert _match_key("Vladimir Guerrero Jr.") == _match_key("Vladimir Guerrero Jr")
    assert _match_key("Ronald Acuña Jr.") == _match_key("Ronald Acuna Jr")
    assert _match_key("Mike Trout") != _match_key("Mike Piazza")


def test_team_abbreviations_are_normalized():
    assert normalize_team("WAS") == normalize_team("WSH")
    assert normalize_team("CHW") == normalize_team("CWS")
    assert normalize_team("SFG") == normalize_team("SF")


# --------------------------------------------------------------------------
# Ownership logging
# --------------------------------------------------------------------------

STANDINGS_CSV = """Rank,EntryId,EntryName,TimeRemaining,Points,Lineup
1,1,alice,0,210.5,P Ace Pitcher P Second Arm C Backstop Guy 1B First Sacker 2B Middle Infield 3B Hot Corner SS Shortstop OF Outfield One OF Outfield Two OF Outfield Three
2,2,bob,0,205.0,P Ace Pitcher P Third Arm C Other Catcher 1B First Sacker 2B Middle Infield 3B Hot Corner SS Shortstop OF Outfield One OF Outfield Four OF Outfield Five
"""


def test_parse_standings_and_ownership(tmp_path):
    path = tmp_path / "standings.csv"
    path.write_text(STANDINGS_CSV)

    parsed = parse_standings(path)
    assert parsed["entry"].nunique() == 2
    assert len(parsed) == 20  # two full rosters

    own = realized_ownership(parsed).set_index("name")["ownership"]
    assert own["Ace Pitcher"] == pytest.approx(1.0)  # in both entries
    assert own["Backstop Guy"] == pytest.approx(0.5)  # in one
    assert own["Outfield One"] == pytest.approx(1.0)

    # Names must not absorb the following position token.
    assert "Second Arm" in set(parsed["name"])
    assert not any("P " in n for n in parsed["name"])
