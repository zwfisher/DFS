"""Slate data structures shared by every layer.

A ``Slate`` is the raw picture of a contest: who is playable, what they cost,
who they face, and what the market thinks each team will score. A
``SimSlate`` is the compiled form the simulator consumes -- matchup-adjusted
rate matrices with players resolved to integer indices, so the hot loop never
touches a string or a DataFrame.
"""

from __future__ import annotations

from dataclasses import dataclass, field

import numpy as np
import pandas as pd

HITTER_POSITIONS = ("C", "1B", "2B", "3B", "SS", "OF")


@dataclass
class Player:
    player_id: str
    name: str
    team: str
    opponent: str
    positions: tuple[str, ...]
    salary: int
    is_pitcher: bool = False
    batting_order: int | None = None  # 1-9, None if not in the lineup
    bats: str = "R"
    throws: str = "R"
    confirmed: bool = False
    dk_id: str | None = None

    @property
    def in_lineup(self) -> bool:
        return self.is_pitcher or self.batting_order is not None


@dataclass
class Game:
    game_id: str
    away: str
    home: str
    away_total: float
    home_total: float
    away_starter: str | None = None  # player_id
    home_starter: str | None = None

    @property
    def park(self) -> str:
        return self.home

    def implied_total(self, team: str) -> float:
        return self.away_total if team == self.away else self.home_total

    def opponent_of(self, team: str) -> str:
        return self.home if team == self.away else self.away


@dataclass
class Slate:
    players: list[Player]
    games: list[Game]
    name: str = "slate"

    def __post_init__(self) -> None:
        self._by_id = {p.player_id: p for p in self.players}
        self._game_by_team = {}
        for g in self.games:
            self._game_by_team[g.away] = g
            self._game_by_team[g.home] = g

    def player(self, player_id: str) -> Player:
        return self._by_id[player_id]

    def game_for(self, team: str) -> Game:
        return self._game_by_team[team]

    @property
    def teams(self) -> list[str]:
        return sorted({p.team for p in self.players})

    def lineup_for(self, team: str) -> list[Player]:
        """The nine hitters for a team, ordered by batting slot."""
        hitters = [
            p
            for p in self.players
            if p.team == team and not p.is_pitcher and p.batting_order is not None
        ]
        return sorted(hitters, key=lambda p: p.batting_order)

    def starter_for(self, team: str) -> Player | None:
        game = self.game_for(team)
        pid = game.home_starter if game.home == team else game.away_starter
        return self._by_id.get(pid) if pid else None

    def to_frame(self) -> pd.DataFrame:
        return pd.DataFrame(
            [
                {
                    "player_id": p.player_id,
                    "name": p.name,
                    "team": p.team,
                    "opponent": p.opponent,
                    "positions": "/".join(p.positions),
                    "salary": p.salary,
                    "is_pitcher": p.is_pitcher,
                    "batting_order": p.batting_order,
                    "confirmed": p.confirmed,
                }
                for p in self.players
            ]
        )


# --------------------------------------------------------------------------
# Compiled simulation inputs
# --------------------------------------------------------------------------


@dataclass
class SimTeam:
    """One side of a game, compiled for the simulator."""

    team: str
    batter_idx: np.ndarray  # (9,) int32 -- global player indices in batting order
    vs_starter: np.ndarray  # (9, n_outcomes) float64 -- rates vs opposing starter
    vs_bullpen: np.ndarray  # (9, n_outcomes) float64 -- rates vs opposing bullpen
    sb_rate: np.ndarray  # (9,) float64 -- steal attempt rate per time on first
    starter_idx: int  # global index of this team's own starting pitcher
    implied_runs: float


@dataclass
class SimGame:
    game_id: str
    park: str
    away: SimTeam
    home: SimTeam


@dataclass
class SimSlate:
    """Everything the simulator needs, with names resolved to indices."""

    games: list[SimGame]
    player_ids: list[str]  # index -> player_id
    slate: Slate = field(repr=False, default=None)

    @property
    def n_players(self) -> int:
        return len(self.player_ids)

    def index_of(self, player_id: str) -> int:
        return self.player_ids.index(player_id)

    def id_to_index(self) -> dict[str, int]:
        return {pid: i for i, pid in enumerate(self.player_ids)}
