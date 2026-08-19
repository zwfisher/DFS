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


# -- the course profile -------------------------------------------------------

def test_course_table_gives_measured_bellerive_scoring():
    profile = dg.load_course_table(COURSE, offline=True)
    assert profile.par == 70
    assert 6800 < profile.yardage < 7800
    # The par-type scores have to reconstruct the course's overall average.
    from pgadfs.config import BELLERIVE
    by_par = {3: profile.par_3_score, 4: profile.par_4_score, 5: profile.par_5_score}
    implied = sum(by_par[p] for p in BELLERIVE.pars)
    assert implied == pytest.approx(profile.score_to_par, abs=0.02)
    # And Bellerive's rough is genuinely among the most punishing on tour.
    assert profile.rough_penalty > 0.3
    assert profile.rough_penalty_rank < 25


def test_par_type_offsets_change_the_shape_not_the_level():
    from pgadfs.config import BELLERIVE
    from pgadfs.sim.holes import par_type_offsets

    profile = dg.load_course_table(COURSE, offline=True)
    measured = {3: profile.par_3_score, 4: profile.par_4_score, 5: profile.par_5_score}
    offsets = par_type_offsets(measured, BELLERIVE.pars)
    assert np.mean([offsets[p] for p in BELLERIVE.pars]) == pytest.approx(0.0, abs=1e-12)
    # Bellerive's par 5s are hard for par 5s -- that is the headline.
    assert offsets[5] > 0.05
    assert offsets[5] > offsets[4] > offsets[3]


def test_course_history_is_present_and_heavily_shrunk():
    history, cap = dg.load_course_history(offline=True)
    assert len(history) == 50
    played = [h for h in history.values() if h.rounds]
    assert 5 < len(played) < 50           # only some of the field has been here
    assert max(abs(h.adjustment) for h in history.values()) < cap
    # Four rounds eight years ago is not information, and the shrinkage knows it.
    assert max(abs(h.adjustment) for h in history.values()) < 0.05


def test_course_fit_matches_datagolf_without_copying_it():
    from pgadfs.data.ids import NameIndex
    from pgadfs.projections.coursefit import build_fits, course_profile

    profile = course_profile(COURSE, offline=True)
    # Bellerive pays for length and does not pay for short game.
    assert profile.emphasis["Driving Distance"] > 0.05
    assert profile.emphasis["Around Green"] < -0.02
    assert max(profile.emphasis, key=profile.emphasis.get) == "Driving Distance"

    slate = build_slate(CONTEST, COURSE, offline=True)
    fits = build_fits(slate.names, COURSE, offline=True)
    assert len(fits) == 50
    raw, _ = dg.load_course_fit(COURSE, offline=True)
    raw_index = NameIndex((f.name, f) for f in raw.values())
    theirs = np.array([raw_index.get(n).fit for n in slate.names])
    mine = np.array([fits[dg.normalize_name(n)].fit for n in slate.names])
    assert np.corrcoef(mine, theirs)[0, 1] > 0.8      # a faithful reconstruction
    assert mine.std() == pytest.approx(theirs.std(), rel=1e-6)   # anchored to their scale
    # Every golfer's fit is exactly the sum of its parts.
    for name, f in fits.items():
        assert sum(f.components.values()) == pytest.approx(f.fit, abs=1e-9)


def test_tee_times_cover_the_field_and_join_by_name():
    from pgadfs.data.ids import NameIndex

    tee = dg.load_tee_times(offline=True)
    assert len(tee) == 50
    slate = build_slate(CONTEST, COURSE, offline=True)
    index = NameIndex((t.name, t) for t in tee)
    assert all(index.get(g.name) is not None for g in slate.golfers)
    # Fifty golfers in twosomes off one tee: twenty-five distinct times.
    assert len({t.minutes for t in tee}) == 25
    assert len({g.tee_slot for g in slate.golfers}) == 25
    assert all(g.tee_time for g in slate.golfers)
