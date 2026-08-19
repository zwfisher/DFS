import numpy as np
import pytest

from pgadfs.data import datagolf as dg
from pgadfs.data import dk
from pgadfs.data.ids import NameIndex
from pgadfs.slate import build_slate

CONTEST = 193_766_688
COURSE = "Bellerive CC"


@pytest.mark.parametrize(
    "raw,expected",
    [
        ("Højgaard, Nicolai", "nicolai hojgaard"),
        ("Ludvig Åberg", "ludvig aberg"),
        ("J.T. Poston", "j t poston"),
        ("Kim, Si Woo", "si woo kim"),
        ("  Rory McIlroy ", "rory mcilroy"),
        ("Davis Love III", "davis love"),
    ],
)
def test_normalize_name(raw, expected):
    assert dg.normalize_name(raw) == expected


def test_name_index_falls_back_through_nicknames():
    index = NameIndex([("Echavarria, Nico", 1), ("McCarty, Matt", 2), ("Scheffler, Scottie", 3)])
    assert index.get("Nicolas Echavarria") == 1
    assert index.get("Matthew McCarty") == 2
    assert index.get("Scottie Scheffler") == 3
    assert index.get("Nobody At All") is None


def test_name_index_refuses_ambiguous_last_names():
    index = NameIndex([("Kim, Si Woo", 1), ("Kim, Tom", 2)])
    assert index.get("Kim") is None
    assert index.get("Si Woo Kim") == 1


def test_js_payload_extraction():
    html = """<script>
      var reload_data = JSON.parse('{"a": 1, "b": "O\\'Neill", "c": [1,2,3], "pad": "%s"}')
    </script>""" % ("x" * 300)
    out = dg.extract_payloads(html)
    assert out["reload_data"]["a"] == 1
    assert out["reload_data"]["b"] == "O'Neill"


def test_extraction_skips_unparseable_payloads():
    assert dg.extract_payloads("var x = JSON.parse('{not json%s}')" % ("y" * 300)) == {}


def test_contest_fixture_parses():
    contest = dk.load_contest(CONTEST, offline=True)
    assert contest.contest_id == CONTEST
    assert contest.entry_fee > 0
    assert contest.paid_places > 0
    prizes = contest.payout_vector()
    assert len(prizes) == contest.paid_places
    assert prizes[0] >= prizes[-1] > 0
    assert all(a >= b for a, b in zip(prizes, prizes[1:]))
    assert sum(prizes) == pytest.approx(contest.total_payouts, rel=1e-9)


def test_draftables_fixture_parses():
    contest = dk.load_contest(CONTEST, offline=True)
    players = dk.load_draftables(contest.draft_group, offline=True)
    assert len(players) == 50
    assert len({p.draftable_id for p in players}) == 50
    assert all(p.salary >= 0 for p in players)


def test_finish_odds_are_devigged_to_the_number_of_places():
    odds = dg.load_finish_odds(offline=True)
    assert len(odds) == 50
    for source in ("model", "market"):
        vectors = dg.finish_probability_vectors(odds, source)
        for market, places in (("win", 1), ("top_5", 5), ("top_10", 10), ("top_20", 20)):
            assert sum(vectors[market]) == pytest.approx(places, rel=1e-6)
            assert vectors[market] == sorted(vectors[market], reverse=True)


def test_fantasy_page_keeps_ownership_and_ids_behind_the_mask():
    rows, consts = dg.load_fantasy_projections(offline=True)
    assert len(rows) == 50
    assert consts["event_name"]
    assert sum(r.ownership for r in rows) == pytest.approx(600.0, abs=1.0)
    assert all(r.dk_id > 0 for r in rows)
    assert all(r.score_sd > 0 for r in rows)
    # Names are masked outside DataGolf's visible top five, ids are not.
    assert 0 < sum(r.name is not None for r in rows) < 50


def test_slate_joins_every_golfer():
    slate = build_slate(CONTEST, COURSE, offline=True)
    assert len(slate) == 50
    assert not any(slate.warnings.values()), slate.warnings
    assert slate.edge.mean() == pytest.approx(0.0, abs=1e-9)
    assert slate.ownership.sum() == pytest.approx(600.0, abs=1.0)
    assert set(np.unique(slate.waves)) <= {0, 1}
    assert slate.golfers[slate.index("Rory McIlroy")].name == "Rory McIlroy"
