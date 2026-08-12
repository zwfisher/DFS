"""Compile a slate plus talent rates into simulator inputs.

This is the seam between the projection layer and the simulation layer. Once
``build_sim_slate`` has run, everything downstream works in integer indices
and float arrays -- no names, no DataFrames, no dictionary lookups in a hot
loop.
"""

from __future__ import annotations

import numpy as np

from ..config import LEAGUE_RUNS_PER_GAME, N_OUTCOMES
from ..slate import SimGame, SimSlate, SimTeam, Slate
from .lineups import resolve_lineup
from .matchup import build_matchup_rates, scale_to_team_total
from .rates import league_rate_vector

DEFAULT_SB_RATE = 0.055


class RateBook:
    """Rate lookups with a league-average fallback for anyone missing.

    A slate should never fail to build because one player has no history --
    a September call-up gets league-average talent and a wide interval, which
    is the honest representation of what we know about him.
    """

    def __init__(
        self,
        batters: dict[str, np.ndarray] | None = None,
        pitchers: dict[str, np.ndarray] | None = None,
        bullpens: dict[str, np.ndarray] | None = None,
        steal_rates: dict[str, float] | None = None,
    ) -> None:
        self.batters = batters or {}
        self.pitchers = pitchers or {}
        self.bullpens = bullpens or {}
        self.steal_rates = steal_rates or {}
        self._league = league_rate_vector()

    def batter(self, player_id: str) -> np.ndarray:
        return np.asarray(self.batters.get(player_id, self._league), dtype=np.float64)

    def pitcher(self, player_id: str | None) -> np.ndarray:
        if player_id is None:
            return self._league
        return np.asarray(self.pitchers.get(player_id, self._league), dtype=np.float64)

    def bullpen(self, team: str) -> np.ndarray:
        if team in self.bullpens:
            return np.asarray(self.bullpens[team], dtype=np.float64)
        # A generic bullpen is a touch better than a generic starter: relief
        # arms strike out more and are used in favourable matchups.
        pen = self._league.copy()
        pen[0] *= 1.10  # strikeouts
        pen[7] *= 0.94  # home runs
        return pen / pen.sum()

    def steal(self, player_id: str) -> float:
        return float(self.steal_rates.get(player_id, DEFAULT_SB_RATE))


def build_sim_slate(slate: Slate, rates: RateBook) -> SimSlate:
    """Turn a slate into matchup-adjusted, index-resolved simulator inputs."""
    player_ids = [p.player_id for p in slate.players]
    index = {pid: i for i, pid in enumerate(player_ids)}

    games: list[SimGame] = []
    for game in slate.games:
        sides = []
        for team, opp, starter_id in (
            (game.away, game.home, game.away_starter),
            (game.home, game.away, game.home_starter),
        ):
            lineup = resolve_lineup(slate, team)
            opp_starter_id = game.home_starter if team == game.away else game.away_starter

            batter_matrix = np.stack([rates.batter(p.player_id) for p in lineup])
            opp_sp = rates.pitcher(opp_starter_id)
            opp_pen = rates.bullpen(opp)

            vs_starter = build_matchup_rates(batter_matrix, opp_sp, game.park)
            vs_bullpen = build_matchup_rates(batter_matrix, opp_pen, game.park)

            implied = game.implied_total(team)
            vs_starter = scale_to_team_total(vs_starter, implied, LEAGUE_RUNS_PER_GAME)
            vs_bullpen = scale_to_team_total(vs_bullpen, implied, LEAGUE_RUNS_PER_GAME)

            sides.append(
                SimTeam(
                    team=team,
                    batter_idx=np.array(
                        [index[p.player_id] for p in lineup], dtype=np.int32
                    ),
                    vs_starter=vs_starter,
                    vs_bullpen=vs_bullpen,
                    sb_rate=np.array(
                        [rates.steal(p.player_id) for p in lineup], dtype=np.float64
                    ),
                    starter_idx=index.get(starter_id, -1) if starter_id else -1,
                    implied_runs=implied,
                )
            )

        games.append(
            SimGame(game_id=game.game_id, park=game.park, away=sides[0], home=sides[1])
        )

    return SimSlate(games=games, player_ids=player_ids, slate=slate)


def validate_rates(matrix: np.ndarray, tol: float = 1e-6) -> None:
    """Assert a rate matrix is a valid set of probability vectors."""
    arr = np.atleast_2d(matrix)
    if arr.shape[-1] != N_OUTCOMES:
        raise ValueError(f"expected {N_OUTCOMES} outcome columns, got {arr.shape[-1]}")
    if (arr < 0).any():
        raise ValueError("negative probability in rate matrix")
    sums = arr.sum(axis=1)
    if not np.allclose(sums, 1.0, atol=tol):
        raise ValueError(f"rate rows must sum to 1, got range [{sums.min()}, {sums.max()}]")
