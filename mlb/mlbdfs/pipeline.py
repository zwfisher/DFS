"""End-to-end slate pipeline.

Ties the four layers together in the right order and, importantly, with the
right separation between them.

**The holdout matters.** The candidate pool is built by optimizing against
sampled draws from a simulation. Scoring those same candidates on those same
draws measures how well the optimizer fit its own noise, not how good the
lineups are -- with a few hundred candidates selected from millions of legal
rosters, that bias is enormous, and it shows up as absurd win
probabilities. So the pipeline runs two independent simulations: one the
optimizer sees, and one used only for evaluation. It costs a second
simulation and it is not optional.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np
import pandas as pd

from .config import OPTIMIZER, OWNERSHIP, SIM
from .optimize.contest import Contest, large_gpp
from .optimize.milp import Lineup, LineupOptimizer
from .optimize.portfolio import evaluate_lineups, select_portfolio
from .ownership.field import Field, generate_field
from .ownership.heuristic import project_ownership
from .ownership.uncertainty import ownership_interval
from .projections.build import RateBook, build_sim_slate
from .sim.engine import SimResult, simulate_slate
from .slate import Slate


@dataclass
class PipelineResult:
    slate: Slate
    sim: SimResult  # used for projections, ownership and pool generation
    holdout: SimResult  # used only for evaluation
    ownership: pd.DataFrame
    field: Field
    pool: list[Lineup]
    evaluation: pd.DataFrame
    selected: pd.DataFrame
    contest: Contest

    def projections(self) -> pd.DataFrame:
        """Player projections with intervals, ownership and leverage."""
        summary = self.sim.summary().set_index("player_id")
        out = self.ownership.set_index("player_id").join(
            summary[["mean", "sd", "p10", "median", "p90"]]
        )
        out["value"] = out["mean"] / (out["salary"] / 1000.0)
        return out.reset_index().sort_values("mean", ascending=False)


def run_pipeline(
    slate: Slate,
    rates: RateBook,
    contest: Contest | None = None,
    n_sims: int = 10_000,
    n_field: int = 10_000,
    n_candidates: int = 200,
    n_lineups: int = 20,
    seed: int = 1,
    verbose: bool = True,
) -> PipelineResult:
    """Run projections, ownership, field and optimization for one slate."""
    contest = contest or large_gpp()
    log = print if verbose else (lambda *a, **k: None)

    sim_slate = build_sim_slate(slate, rates)

    log(f"simulating {len(sim_slate.games)} games x {n_sims} sims ...")
    sim = simulate_slate(sim_slate, n_sims=n_sims, seed=seed)
    # Independent draws for evaluation; see the module docstring.
    holdout = simulate_slate(sim_slate, n_sims=n_sims, seed=seed + 9973)

    log("projecting ownership ...")
    ownership = ownership_interval(project_ownership(slate, sim))

    log(f"generating field of {n_field} opponent lineups ...")
    field = generate_field(slate, ownership, n_lineups=n_field, seed=seed)

    log(f"building candidate pool ({n_candidates} solves) ...")
    optimizer = LineupOptimizer(slate)
    pool = optimizer.generate_pool(
        sim.scores, sim.player_ids, n_candidates=n_candidates, seed=seed
    )

    log(f"evaluating {len(pool)} candidates against the field ...")
    evaluation = evaluate_lineups(
        pool, holdout.scores, holdout.player_ids, field, contest
    )
    selected = select_portfolio(pool, evaluation, n_lineups=n_lineups)

    return PipelineResult(
        slate=slate,
        sim=sim,
        holdout=holdout,
        ownership=ownership,
        field=field,
        pool=pool,
        evaluation=evaluation,
        selected=selected,
        contest=contest,
    )


def in_sample_bias(result: PipelineResult) -> pd.DataFrame:
    """How much the holdout changes the answer.

    Worth looking at once on any new slate. A large gap between in-sample
    and holdout ROI is not a bug -- it is the selection bias the holdout
    exists to remove -- but the size of it tells you how much the candidate
    pool is fitting simulation noise, and therefore how much to trust small
    ROI differences between candidates.
    """
    in_sample = evaluate_lineups(
        result.pool, result.sim.scores, result.sim.player_ids, result.field, result.contest
    )
    merged = in_sample[["lineup", "roi", "p_win"]].merge(
        result.evaluation[["lineup", "roi", "p_win"]],
        on="lineup",
        suffixes=("_in_sample", "_holdout"),
    )
    return pd.DataFrame(
        [
            {
                "metric": "mean ROI",
                "in_sample": merged["roi_in_sample"].mean(),
                "holdout": merged["roi_holdout"].mean(),
            },
            {
                "metric": "mean P(win)",
                "in_sample": merged["p_win_in_sample"].mean(),
                "holdout": merged["p_win_holdout"].mean(),
            },
            {
                "metric": "rank correlation",
                "in_sample": np.nan,
                "holdout": merged["roi_in_sample"].corr(
                    merged["roi_holdout"], method="spearman"
                ),
            },
        ]
    )
