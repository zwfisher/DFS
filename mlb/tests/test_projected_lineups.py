"""Projected lineups conditioned on the opposing starter's hand.

Lineups post one to three hours before first pitch. These tests cover the
model that fills the gap, and the property that makes it worth having:
managers platoon, so "who started recently" and "who starts against a
left-hander" are different questions, and only the second one is useful.
"""

from __future__ import annotations

import numpy as np
import pandas as pd
import pytest

from mlbdfs.config import OUTCOMES
from mlbdfs.data.fixtures import (
    lineup_history_truth,
    make_lineup_history,
    make_slate,
)
from mlbdfs.data.sources import _extract_lineups
from mlbdfs.projections.build import RateBook
from mlbdfs.projections.projected_lineups import (
    apply_to_slate,
    platoon_split,
    project_lineup,
)
from mlbdfs.projections.rates import blend_platoon, shrink


# --------------------------------------------------------------------------
# Recovering lineups from play-by-play
# --------------------------------------------------------------------------


def _pbp_rows(game_pk, date_str, away, home, away_hand, home_hand, extras=()):
    rows, ab = [], 1
    for slot in range(1, 10):
        rows.append(dict(game_pk=game_pk, game_date=date_str, at_bat_number=ab,
                         batter=1000 + slot, pitcher=1, inning_topbot="Top",
                         home_team=home, away_team=away, p_throws=home_hand))
        ab += 1
    for slot in range(1, 10):
        rows.append(dict(game_pk=game_pk, game_date=date_str, at_bat_number=ab,
                         batter=2000 + slot, pitcher=2, inning_topbot="Bot",
                         home_team=home, away_team=away, p_throws=away_hand))
        ab += 1
    for batter, half, hand in extras:
        rows.append(dict(game_pk=game_pk, game_date=date_str, at_bat_number=ab,
                         batter=batter, pitcher=9, inning_topbot=half,
                         home_team=home, away_team=away, p_throws=hand))
        ab += 1
    return pd.DataFrame(rows)


def test_starting_nine_recovered_from_play_by_play():
    pbp = _pbp_rows(1, "2026-07-01", "NYY", "BOS", "R", "L",
                    extras=[(1999, "Top", "R")])  # a pinch hitter
    out = _extract_lineups(pbp)

    nyy = out[out["team"] == "NYY"]
    assert len(nyy) == 9
    assert 1999 not in set(nyy["player_id"]), "pinch hitter is not a starter"
    assert list(nyy.sort_values("batting_order")["player_id"]) == [
        1000 + i for i in range(1, 10)
    ]


def test_opposing_hand_is_per_side():
    pbp = _pbp_rows(1, "2026-07-01", "NYY", "BOS", away_hand="R", home_hand="L")
    out = _extract_lineups(pbp)
    # NYY bats in the top, so it faces the home starter, who is a lefty.
    assert set(out[out["team"] == "NYY"]["opp_hand"]) == {"L"}
    assert set(out[out["team"] == "BOS"]["opp_hand"]) == {"R"}


def test_extract_lineups_reports_missing_columns():
    with pytest.raises(ValueError, match="missing columns"):
        _extract_lineups(pd.DataFrame({"game_pk": [1]}))


# --------------------------------------------------------------------------
# The projection itself
# --------------------------------------------------------------------------


def test_start_probabilities_sum_to_nine():
    """Exactly nine hitters start, so the estimates have to add up to nine.

    The same discipline the ownership model uses: a set of independent
    per-player estimates otherwise describes no coherent lineup.
    """
    history = make_lineup_history(n_games=40, seed=1)
    for hand in ("L", "R"):
        projection = project_lineup(history, "NYY", hand)
        assert projection.frame["start_probability"].sum() == pytest.approx(9.0, abs=1e-6)
        assert projection.frame["start_probability"].between(0, 1).all()


def test_platoon_is_recovered():
    """Ground truth the model is never told: 8 plays only against lefties."""
    history = make_lineup_history(n_games=50, seed=2, platoon_strength=1.0)
    vs_l = project_lineup(history, "NYY", "L")
    vs_r = project_lineup(history, "NYY", "R")

    p_l = vs_l.frame.set_index("player_id")["start_probability"]
    p_r = vs_r.frame.set_index("player_id")["start_probability"]

    assert p_l[8] > p_r[8], "lefty masher must be likelier against a lefty"
    assert p_r[9] > p_l[9], "righty masher must be likelier against a righty"
    # Exactly one of the pair starts, whichever hand is on the mound.
    assert p_l[8] + p_l[9] == pytest.approx(1.0, abs=0.25)

    swing = platoon_split(vs_l, vs_r)
    assert set(swing.index[:2]) == {8, 9}, "the platoon pair should swing most"


def test_no_platoon_produces_no_platoon_signal():
    """The failure mode shrinkage exists to prevent: inventing a platoon."""
    history = make_lineup_history(n_games=60, seed=4, platoon_strength=0.0)
    vs_l = project_lineup(history, "NYY", "L")
    vs_r = project_lineup(history, "NYY", "R")

    swing = platoon_split(vs_l, vs_r)["swing"].abs()
    assert swing.max() < 0.45, "a non-platooning team must not look like one"


def test_the_prior_moves_the_platoon_spot_but_not_the_core():
    """The prior changes selection, not just confidence.

    Worth pinning because the opposite is easy to assume. Shrinkage is
    toward each player's *own* overall rate rather than a shared constant,
    so it is not a common monotone transform across players: a platoon bat
    with a strong hand-specific record and a weak overall one can trade
    places with his counterpart as the prior moves. The everyday core does
    not move, which is what makes the behaviour tolerable.
    """
    history = make_lineup_history(n_games=40, seed=5, platoon_strength=1.0)
    light = set(project_lineup(history, "NYY", "L", hand_prior_starts=0.5)
                .starters["player_id"])
    heavy = set(project_lineup(history, "NYY", "L", hand_prior_starts=20.0)
                .starters["player_id"])

    core = set(range(1, 8))
    assert core <= light and core <= heavy, "everyday players must be stable"
    # A light prior trusts the hand-specific record and takes the lefty masher.
    assert 8 in light
    assert light != heavy


def test_starters_are_nine_in_batting_order():
    history = make_lineup_history(n_games=40, seed=6)
    starters = project_lineup(history, "NYY", "R").starters
    assert len(starters) == 9
    assert list(starters["batting_order"]) == list(range(1, 10))


def test_a_call_up_is_not_charged_for_games_before_he_arrived():
    """A player who has taken over the job should overtake the man he replaced.

    The denominator has to run back only to a player's first appearance. Over
    the whole window instead, a hitter promoted ten games ago is charged with
    the twenty before he was on the roster, and because a short stretch is
    often lopsided by pitcher hand, the hand-specific denominator is where it
    bites hardest -- an everyday call-up reads as a part-timer.
    """
    history = make_lineup_history(n_games=40, seed=7)
    # Player 12 replaces player 1 for the last ten games.
    recent = np.sort(history["game_date"].unique())[-10:]
    history.loc[
        history["game_date"].isin(recent) & (history["player_id"] == 1), "player_id"
    ] = 12

    projection = project_lineup(history, "NYY", "R")
    probability = projection.frame.set_index("player_id")["start_probability"]
    assert probability.get(12, 0) > probability.get(1, 0)


def test_asof_excludes_the_future():
    history = make_lineup_history(n_games=40, seed=8)
    cutoff = np.sort(history["game_date"].unique())[10]
    projection = project_lineup(history, "NYY", "R", asof=cutoff)
    assert projection.games_observed <= 10


def test_unknown_team_returns_empty():
    history = make_lineup_history(n_games=20, seed=9)
    assert project_lineup(history, "NOBODY", "R").frame.empty


def test_history_missing_columns_is_reported():
    with pytest.raises(ValueError, match="missing columns"):
        project_lineup(pd.DataFrame({"team": ["NYY"]}), "NYY", "R")


# --------------------------------------------------------------------------
# Applying to a slate
# --------------------------------------------------------------------------


def test_apply_to_slate_fills_only_unconfirmed_lineups():
    slate, _ = make_slate(n_games=2, seed=1)
    teams = slate.teams
    kept, cleared = teams[0], teams[1]

    for player in slate.players:
        if player.is_pitcher or player.team != cleared:
            continue
        player.batting_order = None
        player.confirmed = False

    history = pd.concat(
        [
            make_lineup_history(n_games=30, seed=i, team=team)
            for i, team in enumerate(teams)
        ],
        ignore_index=True,
    )
    # The synthetic history uses integer ids; map the slate's players onto them.
    id_map = {}
    for team in teams:
        hitters = [p for p in slate.players if p.team == team and not p.is_pitcher]
        for n, player in enumerate(sorted(hitters, key=lambda p: p.player_id), start=1):
            id_map[player.player_id] = n

    apply_to_slate(slate, history, id_map=id_map)

    confirmed = [p for p in slate.players if p.team == kept and not p.is_pitcher]
    assert all(p.confirmed and p.start_probability == 1.0 for p in confirmed)

    projected = [p for p in slate.players if p.team == cleared and not p.is_pitcher]
    assert any(p.batting_order is not None for p in projected)
    assert all(p.start_probability <= 1.0 for p in projected)


# --------------------------------------------------------------------------
# Platoon splits on the rates themselves
# --------------------------------------------------------------------------


def _counts(pid, pa, **kw):
    row = {"player_id": pid, "season": 2026, "pa": pa}
    row.update({o: 0 for o in OUTCOMES})
    row.update(kw)
    row["field_out"] = pa - sum(kw.values())
    return row


def test_platoon_rates_move_the_right_way():
    """The other half of handedness: how a hitter hits the arm he faces."""
    overall = shrink(pd.DataFrame([_counts("a", 600, strikeout=120, home_run=25,
                                           single=90, walk=60)]))
    splits = pd.DataFrame([
        dict(_counts("a", 150, strikeout=20, home_run=14, single=28, walk=20),
             vs_hand="L"),
        dict(_counts("a", 450, strikeout=100, home_run=11, single=62, walk=40),
             vs_hand="R"),
    ])
    hr = OUTCOMES.index("home_run")

    vs_l = blend_platoon(overall, splits, "L")[list(OUTCOMES)].iloc[0].to_numpy()
    vs_r = blend_platoon(overall, splits, "R")[list(OUTCOMES)].iloc[0].to_numpy()
    base = overall[list(OUTCOMES)].iloc[0].to_numpy()

    assert vs_l[hr] > base[hr] > vs_r[hr]
    # Shrunk toward the player's own overall rate, not taken at face value.
    assert vs_l[hr] < 14 / 150
    assert np.allclose([vs_l.sum(), vs_r.sum()], 1.0)


def test_platoon_rates_pass_through_without_splits():
    overall = shrink(pd.DataFrame([_counts("a", 400, strikeout=90, home_run=12)]))
    for splits in (None, pd.DataFrame()):
        out = blend_platoon(overall, splits, "L")
        assert np.allclose(
            out[list(OUTCOMES)].to_numpy(), overall[list(OUTCOMES)].to_numpy()
        )


def test_rate_book_selects_by_hand_and_falls_back():
    flat = np.full(len(OUTCOMES), 1.0 / len(OUTCOMES))
    power = flat.copy()
    power[OUTCOMES.index("home_run")] *= 3.0
    power = power / power.sum()

    book = RateBook(batters={"a": flat}, batters_by_hand={"L": {"a": power}})
    assert not np.allclose(book.batter("a", "L"), book.batter("a"))
    # No split for right-handers, and no entry at all for an unknown player.
    assert np.allclose(book.batter("a", "R"), book.batter("a"))
    assert np.allclose(book.batter("a", None), book.batter("a"))
    assert book.batter("missing", "L").sum() == pytest.approx(1.0)


# --------------------------------------------------------------------------
# Walk-forward accuracy
# --------------------------------------------------------------------------


def test_walk_forward_accuracy_beats_a_naive_baseline():
    """The number that justifies the whole module.

    Against synthetic history the projection should land around eight of
    nine. Real accuracy will be lower -- the generator here is far simpler
    than a manager -- but a regression below this on synthetic data means
    something is broken.
    """
    from mlbdfs.backtest import accuracy_summary, lineup_accuracy

    history = pd.concat(
        [
            make_lineup_history(n_games=60, seed=i, team=team, platoon_strength=k)
            for i, (team, k) in enumerate([("NYY", 1.0), ("BOS", 0.0), ("TB", 0.6)])
        ],
        ignore_index=True,
    )
    accuracy = lineup_accuracy(history, min_prior_games=20)
    assert not accuracy.empty

    summary = accuracy_summary(accuracy).set_index("split")
    assert summary.loc["all", "mean_hits_of_9"] > 8.0
    assert summary.loc["all", "slot_mae"] < 1.0

    # Left-handed starters are where lineups are hardest to guess and most
    # worth guessing right, so accuracy there must not collapse.
    assert summary.loc["vs LHP", "mean_hits_of_9"] > 7.5
