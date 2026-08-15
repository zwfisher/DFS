"""Projected lineups from recent starts, conditioned on the opposing hand.

Lineups post one to three hours before first pitch, which is later than most
people want to build. This module fills the gap by estimating, from a team's
recent games, how likely each hitter is to start today and where he is likely
to bat.

**Handedness is the whole point.** Managers platoon, so "who has started
recently" is the wrong question -- the right one is "who starts against a
left-hander". A modal lineup taken from ten games against righties will be
confidently wrong about exactly the players a platoon decides, which are the
ones worth knowing about. Every estimate here is therefore computed against
the probable starter's throwing hand and shrunk toward the player's overall
rate, so a hitter with four starts against lefties borrows strength from his
forty overall rather than being taken at face value.

Two properties this inherits from elsewhere in the project, deliberately:

* **It produces a probability, not a guess.** ``start_probability`` already
  exists on ``Player`` and the simulator already prices it. This replaces a
  crude salary-rank proxy with a measurement.
* **It has to add up.** Exactly nine hitters start, so the projected start
  probabilities for a team are normalized to sum to nine -- the same
  discipline the ownership model uses, and for the same reason: a set of
  independent per-player estimates describes no coherent lineup.

The honest limitation is that this is backward looking. It cannot know about
a scratch announced this morning or a manager's press conference. What it
can do is notice that a player has not started in a week, and price him
accordingly.
"""

from __future__ import annotations

from dataclasses import dataclass
from datetime import date

import numpy as np
import pandas as pd

from ..config import (
    LINEUP_HALF_LIFE_GAMES,
    LINEUP_HAND_PRIOR_STARTS,
    LINEUP_LOOKBACK_GAMES,
    LINEUP_SLOT_PRIOR_STARTS,
)

HISTORY_COLUMNS = ["game_date", "team", "player_id", "batting_order", "opp_hand"]


@dataclass
class LineupProjection:
    """A team's projected lineup against one pitcher hand."""

    team: str
    vs_hand: str
    frame: pd.DataFrame  # player_id, start_probability, expected_slot, n_starts
    games_observed: int
    games_vs_hand: int

    @property
    def starters(self) -> pd.DataFrame:
        """The projected nine, in batting order."""
        top = self.frame.nlargest(9, "start_probability").copy()
        top = top.sort_values("expected_slot").reset_index(drop=True)
        top["batting_order"] = np.arange(1, len(top) + 1)
        return top


def project_lineup(
    history: pd.DataFrame,
    team: str,
    vs_hand: str,
    asof: date | pd.Timestamp | None = None,
    lookback_games: int = LINEUP_LOOKBACK_GAMES,
    half_life_games: float = LINEUP_HALF_LIFE_GAMES,
    hand_prior_starts: float = LINEUP_HAND_PRIOR_STARTS,
) -> LineupProjection:
    """Estimate who starts for ``team`` against a ``vs_hand`` starter.

    ``history`` is the frame from ``data.sources.lineup_history``. Games are
    weighted by recency with the given half life, so a lineup from three days
    ago counts for more than one from three weeks ago without discarding the
    older evidence outright.
    """
    _validate(history)
    hand = str(vs_hand).upper()[:1]

    games = history[history["team"] == team].copy()
    games["game_date"] = pd.to_datetime(games["game_date"])
    if asof is not None:
        games = games[games["game_date"] < pd.Timestamp(asof)]
    if games.empty:
        return LineupProjection(team, hand, _empty_frame(), 0, 0)

    # Recency weight, by how many games back rather than how many days --
    # an off day should not age a lineup.
    dates = np.sort(games["game_date"].unique())[::-1][:lookback_games]
    rank = {d: i for i, d in enumerate(dates)}
    games = games[games["game_date"].isin(dates)].copy()
    games["games_back"] = games["game_date"].map(rank)
    games["weight"] = 0.5 ** (games["games_back"] / half_life_games)

    per_game = games.drop_duplicates("game_date")[
        ["game_date", "games_back", "weight", "opp_hand"]
    ]

    # Each player's denominator runs back only as far as his first appearance,
    # not over the whole window. A hitter called up or acquired ten games ago
    # should not be charged with the thirty games before he was on the roster
    # -- and because the recent stretch may be heavy on one pitcher hand, the
    # hand-specific denominator is where that bites hardest. Getting this
    # wrong makes every new everyday player look like a part-timer.
    # Approximated by each player's earliest start in the window. For a
    # genuine call-up that is right. For an established bench bat it is
    # slightly generous -- he was available before his first start too --
    # which nudges part-timers' rates up a little. Measured against synthetic
    # history the trade is clearly worth it: it costs a fraction of a point
    # of Brier on strict platoons and rescues a newly promoted everyday
    # player from being read as a part-timer.
    debut = games.groupby("player_id")["games_back"].max()
    cum = per_game.sort_values("games_back")
    cum_total = cum["weight"].cumsum()
    cum_hand = cum["weight"].where(cum["opp_hand"] == hand, 0.0).cumsum()
    by_games_back = pd.Series(cum["games_back"].to_numpy())

    def _available(series: pd.Series) -> pd.Series:
        idx = np.searchsorted(by_games_back.to_numpy(), debut.to_numpy(), side="right") - 1
        return pd.Series(
            series.to_numpy()[np.clip(idx, 0, len(series) - 1)], index=debut.index
        )

    total_weight_p = _available(cum_total)
    hand_weight_p = _available(cum_hand)

    started = games.groupby("player_id").agg(
        w_all=("weight", "sum"),
        n_starts=("weight", "size"),
        slot_all=("batting_order", lambda s: np.average(
            s, weights=games.loc[s.index, "weight"]
        )),
    )
    vs = games[games["opp_hand"] == hand]
    if not vs.empty:
        hand_stats = vs.groupby("player_id").agg(
            w_hand=("weight", "sum"),
            n_starts_hand=("weight", "size"),
            slot_hand=("batting_order", lambda s: np.average(
                s, weights=vs.loc[s.index, "weight"]
            )),
        )
    else:
        hand_stats = pd.DataFrame(
            columns=["w_hand", "n_starts_hand", "slot_hand"], dtype=float
        )

    out = started.join(hand_stats, how="left").fillna(
        {"w_hand": 0.0, "n_starts_hand": 0.0}
    )

    denom_all = total_weight_p.reindex(out.index).clip(lower=1e-9)
    denom_hand = hand_weight_p.reindex(out.index).fillna(0.0)

    p_all = (out["w_all"] / denom_all).clip(0.0, 1.0)
    p_hand_raw = np.where(denom_hand > 0, out["w_hand"] / denom_hand.clip(lower=1e-9), np.nan)

    # Shrink the hand-specific rate toward the player's own overall rate.
    # Platoon usage is a real, individual effect, but four starts against
    # lefties is not enough to establish one on its own.
    prior = hand_prior_starts
    p = (out["w_hand"] + prior * p_all) / (denom_hand + prior)
    p = np.where(denom_hand > 0, p, p_all)
    out["start_probability"] = np.clip(p, 0.0, 1.0)
    out["p_overall"] = p_all
    out["p_vs_hand_raw"] = p_hand_raw

    # Exactly nine hitters start, so the estimates must sum to nine.
    total = out["start_probability"].sum()
    if total > 0:
        out["start_probability"] = np.minimum(
            out["start_probability"] * 9.0 / total, 1.0
        )

    out["expected_slot"] = _blend_slot(out, hand_prior=LINEUP_SLOT_PRIOR_STARTS)

    frame = out.reset_index()[
        [
            "player_id",
            "start_probability",
            "expected_slot",
            "p_overall",
            "p_vs_hand_raw",
            "n_starts",
            "n_starts_hand",
        ]
    ]
    return LineupProjection(
        team=team,
        vs_hand=hand,
        frame=frame.sort_values("start_probability", ascending=False).reset_index(
            drop=True
        ),
        games_observed=len(dates),
        games_vs_hand=int((per_game["opp_hand"] == hand).sum()),
    )


def _blend_slot(out: pd.DataFrame, hand_prior: float) -> np.ndarray:
    """Expected batting slot, hand-specific shrunk toward overall."""
    slot_all = out["slot_all"].to_numpy(dtype=float)
    if "slot_hand" in out.columns:
        slot_hand = out["slot_hand"].to_numpy(dtype=float)
        n_hand = out["n_starts_hand"].to_numpy(dtype=float)
    else:
        slot_hand = np.full(len(out), np.nan)
        n_hand = np.zeros(len(out))

    w = n_hand / (n_hand + hand_prior)
    blended = np.where(np.isnan(slot_hand), slot_all, w * slot_hand + (1 - w) * slot_all)
    return np.clip(blended, 1.0, 9.0)


def platoon_split(projection: LineupProjection, other: LineupProjection) -> pd.DataFrame:
    """Players whose projected role changes most with the opposing hand.

    Useful for eyeballing whether a team platoons at all, and for catching
    the case where a hitter is an everyday player against righties and a
    bench bat against lefties -- a distinction worth several points of
    projection and the entire difference between a zero and a start.
    """
    a = projection.frame.set_index("player_id")["start_probability"]
    b = other.frame.set_index("player_id")["start_probability"]
    merged = pd.DataFrame(
        {f"vs_{projection.vs_hand}": a, f"vs_{other.vs_hand}": b}
    ).fillna(0.0)
    merged["swing"] = merged.iloc[:, 0] - merged.iloc[:, 1]
    return merged.reindex(merged["swing"].abs().sort_values(ascending=False).index)


def apply_to_slate(
    slate,
    history: pd.DataFrame,
    id_map: dict[str, int] | None = None,
    asof: date | None = None,
    hands: dict[str, str] | None = None,
) -> dict[str, LineupProjection]:
    """Attach projected batting orders and start probabilities to a slate.

    ``hands`` maps a team to the throwing hand of the starter it faces; when
    absent it is read from the slate's own probable pitchers. Only hitters
    without a posted batting order are touched, so a partially confirmed
    slate keeps the lineups that are real.
    """
    to_mlbam = id_map or {}
    projections: dict[str, LineupProjection] = {}

    for team in slate.teams:
        hand = (hands or {}).get(team) or _opposing_hand(slate, team)
        if hand is None:
            continue

        projection = project_lineup(history, team, hand, asof=asof)
        if projection.frame.empty:
            continue
        projections[team] = projection

        starters = projection.starters.set_index("player_id")
        probability = projection.frame.set_index("player_id")["start_probability"]

        for player in slate.players:
            if player.is_pitcher or player.team != team or player.confirmed:
                continue
            key = to_mlbam.get(player.player_id, player.player_id)
            if key in starters.index:
                player.batting_order = int(starters.loc[key, "batting_order"])
                player.start_probability = float(probability.get(key, 0.5))
            else:
                player.batting_order = None
                player.start_probability = float(probability.get(key, 0.05))

    return projections


def _opposing_hand(slate, team: str) -> str | None:
    game = slate.game_for(team)
    opponent = game.opponent_of(team)
    starter = slate.starter_for(opponent)
    return starter.throws if starter else None


def _empty_frame() -> pd.DataFrame:
    return pd.DataFrame(
        columns=[
            "player_id", "start_probability", "expected_slot",
            "p_overall", "p_vs_hand_raw", "n_starts", "n_starts_hand",
        ]
    )


def _validate(history: pd.DataFrame) -> None:
    missing = [c for c in HISTORY_COLUMNS if c not in history.columns]
    if missing:
        raise ValueError(f"lineup history is missing columns: {missing}")
