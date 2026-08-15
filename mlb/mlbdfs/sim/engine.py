"""Slate-level simulation and the score matrix everything else consumes.

The ``(n_sims, n_players)`` score matrix is the central interface of the
project. Projections produce it; ownership, the field generator, the
optimizer and the ROI evaluator all read it. Keeping that contract narrow is
what lets each layer be replaced without touching the others.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np
import pandas as pd

from ..config import SIM, SimConfig
from ..slate import SimSlate
from .game import simulate_game


@dataclass
class SimResult:
    """Simulated DraftKings points for every player on the slate."""

    scores: np.ndarray  # (n_sims, n_players) float32
    player_ids: list[str]
    team_runs: dict[str, np.ndarray]  # team -> (n_sims,) runs scored
    starter_outs: dict[str, np.ndarray]  # pitcher player_id -> (n_sims,) outs
    player_pa: dict[str, np.ndarray]  # hitter player_id -> (n_sims,) plate appearances

    @property
    def n_sims(self) -> int:
        return self.scores.shape[0]

    def index_of(self, player_id: str) -> int:
        return self.player_ids.index(player_id)

    def for_player(self, player_id: str) -> np.ndarray:
        return self.scores[:, self.index_of(player_id)]

    def summary(self) -> pd.DataFrame:
        """Per-player mean, spread and the quantiles that matter for GPPs.

        ``p90`` is the ceiling number to look at; ``mean`` is what cash games
        and the salary market price. A player whose p90 sits far above his
        mean is exactly the tournament profile stacking is trying to buy.
        """
        s = self.scores
        q = np.quantile(s, [0.10, 0.25, 0.50, 0.75, 0.90], axis=0)
        return pd.DataFrame(
            {
                "player_id": self.player_ids,
                "mean": s.mean(axis=0),
                "sd": s.std(axis=0),
                "p10": q[0],
                "p25": q[1],
                "median": q[2],
                "p75": q[3],
                "p90": q[4],
            }
        )

    def prob_above(self, threshold: float) -> np.ndarray:
        """Per-player probability of exceeding a point threshold."""
        return (self.scores > threshold).mean(axis=0)

    def correlation(self, player_ids: list[str]) -> np.ndarray:
        """Correlation matrix for a subset of players.

        Useful as a sanity check that the simulator is producing the
        teammate correlation stacking depends on. Same-team hitters land
        around +0.10, which is lower than intuition suggests but is what
        DraftKings scoring actually produces: a home run is ten points, so
        each hitter's own Bernoulli noise swamps a good deal of the shared
        game state. A hitter against the opposing starter runs about -0.31.
        """
        idx = [self.index_of(p) for p in player_ids]
        return np.corrcoef(self.scores[:, idx], rowvar=False)


def simulate_slate(
    sim_slate: SimSlate,
    n_sims: int | None = None,
    seed: int | None = None,
    cfg: SimConfig = SIM,
) -> SimResult:
    """Simulate every game on the slate and assemble the score matrix."""
    n = int(n_sims if n_sims is not None else cfg.n_sims)
    rng = np.random.default_rng(cfg.seed if seed is None else seed)

    n_players = sim_slate.n_players
    scores = np.zeros((n, n_players), dtype=np.float32)
    team_runs: dict[str, np.ndarray] = {}
    starter_outs: dict[str, np.ndarray] = {}
    player_pa: dict[str, np.ndarray] = {}

    for game in sim_slate.games:
        result = simulate_game(game, n, rng, cfg)

        for t, side in enumerate((game.away, game.home)):
            for slot in range(9):
                pid = int(result.batter_idx[t, slot])
                if pid < 0:
                    continue

                points = result.bat_pts[t, slot]
                appearances = result.pa_count[t, slot]

                # A hitter who is scratched scores zero, and that is by far
                # the most damaging thing that can happen to a lineup. When
                # the batting order is only a guess, the chance he does not
                # play is drawn per simulation rather than assumed away.
                #
                # The replacement bat is not re-simulated -- the team's other
                # eight hitters keep the run environment they had. That
                # understates the knock-on effect slightly and is worth far
                # less than pricing the zero at all.
                p_start = float(side.start_probability[slot])
                if p_start < 1.0:
                    starts = rng.random(n) < p_start
                    points = points * starts
                    appearances = appearances * starts

                scores[:, pid] += points
                player_pa[sim_slate.player_ids[pid]] = appearances
            sp = int(result.starter_idx[t])
            if sp >= 0:
                scores[:, sp] += result.sp_pts[t]
                starter_outs[sim_slate.player_ids[sp]] = result.sp_outs[t]
            team_runs[side.team] = result.runs[t]

    return SimResult(
        scores=scores,
        player_ids=list(sim_slate.player_ids),
        team_runs=team_runs,
        starter_outs=starter_outs,
        player_pa=player_pa,
    )


def stack_distribution(
    result: SimResult, player_ids: list[str]
) -> dict[str, float]:
    """Score distribution of a group of players summed together.

    Comparing a real stack against the same players shuffled across
    independent simulations is the cleanest demonstration that correlation is
    being captured: the correlated version has a materially fatter right tail
    at the same mean.
    """
    idx = [result.index_of(p) for p in player_ids]
    total = result.scores[:, idx].sum(axis=1)

    shuffled = np.empty_like(result.scores[:, idx])
    rng = np.random.default_rng(0)
    for j, col in enumerate(idx):
        shuffled[:, j] = rng.permutation(result.scores[:, col])
    independent = shuffled.sum(axis=1)

    return {
        "mean": float(total.mean()),
        "sd": float(total.std()),
        "p90": float(np.quantile(total, 0.90)),
        "p99": float(np.quantile(total, 0.99)),
        "independent_sd": float(independent.std()),
        "independent_p90": float(np.quantile(independent, 0.90)),
        "independent_p99": float(np.quantile(independent, 0.99)),
    }


def calibration_report(
    result: SimResult, actuals: dict[str, float]
) -> pd.DataFrame:
    """Coverage check against realized scores.

    A well-calibrated projection puts realized results inside its 80%
    interval about 80% of the time and produces a flat PIT column. A
    U-shaped PIT histogram means the intervals are too narrow, which is the
    usual failure and the one that makes an optimizer overconfident.
    """
    rows = []
    for pid, actual in actuals.items():
        if pid not in result.player_ids:
            continue
        col = result.for_player(pid)
        rows.append(
            {
                "player_id": pid,
                "actual": actual,
                "mean": float(col.mean()),
                "pit": float((col < actual).mean()),
                "in_80": bool(
                    np.quantile(col, 0.10) <= actual <= np.quantile(col, 0.90)
                ),
                "in_50": bool(
                    np.quantile(col, 0.25) <= actual <= np.quantile(col, 0.75)
                ),
            }
        )
    return pd.DataFrame(rows)
