"""End to end: contest id in, lineups out."""

from __future__ import annotations

import json
from dataclasses import dataclass, replace
from pathlib import Path

import numpy as np
import pandas as pd

from .config import Config
from .data.cache import cache_dir
from .optimize.contest import ContestEvaluator, Payouts
from .optimize.milp import candidate_pool
from .optimize.portfolio import Portfolio, select_portfolio
from .ownership.field import Field, build_field
from .ownership.model import enforce_salary_feasibility, projected_ownership, sample_ownership
from .projections.build import projection_table
from .projections.calibrate import Calibration, apply_calibration, calibrate
from .sim.engine import SimResult, simulate
from .slate import Slate, build_slate


@dataclass
class RunResult:
    slate: Slate
    calibration: Calibration
    sim: SimResult
    projections: pd.DataFrame
    ownership: np.ndarray
    field: Field
    portfolio: Portfolio
    evaluator: ContestEvaluator
    candidates: list[tuple[int, ...]]
    ownership_info: dict
    holdout_roi: float = float("nan")

    def lineup_frame(self) -> pd.DataFrame:
        names = self.sim.names
        salaries = self.slate.salaries
        scores = self.sim.points[:, np.asarray(self.portfolio.lineups)].sum(axis=2)
        rows = []
        for k, lineup in enumerate(self.portfolio.lineups):
            rows.append(
                {
                    "lineup": k + 1,
                    "golfers": ", ".join(names[i] for i in lineup),
                    "salary": int(salaries[list(lineup)].sum()),
                    "proj": float(scores[:, k].mean()),
                    "ceiling": float(np.percentile(scores[:, k], 95)),
                    "roi": float(self.portfolio.solo_roi[k]),
                    "marginal_ev": self.portfolio.marginal_ev[k],
                    # Summed ownership is the usual shorthand for how
                    # contrarian a lineup is: 600% is the slate average, so
                    # anything well under it is a lineup the field is not on.
                    "own_sum": float(self.ownership[list(lineup)].sum() * 100),
                    "dupes": float(
                        (self.slate.contest.max_entries if self.slate.contest else 0)
                        * np.prod(self.ownership[list(lineup)])
                    ),
                }
            )
        return pd.DataFrame(rows)

    def to_draftkings_csv(self, path: str | Path) -> Path:
        """A file that can be uploaded straight into the contest."""
        ids = [g.dk_id for g in self.slate.golfers]
        rows = [[ids[i] for i in lineup] for lineup in self.portfolio.lineups]
        frame = pd.DataFrame(rows, columns=["G"] * 6)
        path = Path(path)
        frame.to_csv(path, index=False)
        return path


def _calibration_path(contest_id: int) -> Path:
    return cache_dir() / f"calibration_{contest_id}.json"


def load_or_fit_calibration(
    slate: Slate, cfg: Config, contest_id: int, *, offline: bool, refit: bool
) -> Calibration:
    """Calibration is a few minutes of Nelder-Mead, so it is cached.

    It only depends on the field, the course and the published prices, none
    of which move much once the tee times are out -- but pass `refit` after a
    withdrawal or a big line move.
    """
    path = _calibration_path(contest_id)
    if path.exists() and not refit:
        blob = json.loads(path.read_text())
        return Calibration(
            latent_per_stroke=blob["latent_per_stroke"],
            course_shift=blob["course_shift"],
            talent_multiplier=blob["talent_multiplier"],
            week_sd=blob["week_sd"],
            tie_rule=blob["tie_rule"],
            field_score_to_par=blob["field_score_to_par"],
            diagnostics=blob.get("diagnostics", {}),
        )
    cal = calibrate(slate, cfg.sim, offline=offline)
    path.write_text(
        json.dumps(
            {
                "latent_per_stroke": cal.latent_per_stroke,
                "course_shift": cal.course_shift,
                "talent_multiplier": cal.talent_multiplier,
                "week_sd": cal.week_sd,
                "tie_rule": cal.tie_rule,
                "field_score_to_par": cal.field_score_to_par,
                "diagnostics": {
                    k: cal.diagnostics.get(k, {}) for k in ("variance_fit", "scoring_level", "tie_rule")
                },
            },
            indent=1,
        )
    )
    return cal


def run(
    contest_id: int,
    course: str,
    cfg: Config | None = None,
    *,
    offline: bool = True,
    refit: bool = False,
    salaries_csv: str | None = None,
) -> RunResult:
    cfg = cfg or Config()
    slate = build_slate(contest_id, course, offline=offline, salaries_csv=salaries_csv)

    cal = load_or_fit_calibration(slate, cfg, contest_id, offline=offline, refit=refit)
    scaled = apply_calibration(slate, cal)
    sim_cfg = replace(
        cfg.sim,
        week_sd=cal.week_sd,
        tie_rule=cal.tie_rule,
        field_score_to_par=cal.field_score_to_par,
    )
    sim = simulate(scaled, sim_cfg, cal.model(cfg.sim.course))

    raw_own = projected_ownership(slate, cfg.ownership, points=sim.points.mean(axis=0))
    own, own_info = enforce_salary_feasibility(
        raw_own, slate.salaries, cfg.ownership.target_lineup_spend
    )
    projections = projection_table(scaled, sim, ownership=own, raw_ownership=raw_own)

    if slate.contest is None:
        raise RuntimeError("contest metadata is required to price lineups")
    payouts = Payouts.from_contest(slate.contest)

    rng = np.random.default_rng(cfg.ownership.seed)
    draws = sample_ownership(own, cfg.ownership, rng, n=cfg.contest.ownership_draws)
    field = build_field(
        slate,
        own,
        cfg.contest,
        ownership_draws=draws,
        n_lineups=cfg.contest.n_field_lineups or slate.contest.max_entries,
        projection=sim.points.mean(axis=0),
    )

    # Candidate lineups are *built* on one set of simulations and *priced* on
    # another. Without the split, a lineup that happens to look good on the
    # draws it was optimised against gets to be evaluated on those same
    # draws, and every ROI comes back inflated -- the same overfitting a
    # backtest suffers when it tunes and tests on one sample.
    split = np.random.default_rng(cfg.portfolio.seed)
    order = split.permutation(sim.points.shape[0])
    take = min(cfg.portfolio.roi_sims, len(order) // 2)
    build_points = sim.points[order[take : 2 * take]]
    roi_points = sim.points[order[:take]]

    evaluator = ContestEvaluator(
        field.scores(roi_points), payouts, smoothing=cfg.portfolio.payout_smoothing
    )

    candidates = candidate_pool(
        build_points,
        slate.salaries,
        n_candidates=cfg.portfolio.n_candidates,
        min_salary=cfg.portfolio.min_salary_used,
        rng=split,
    )
    portfolio = select_portfolio(candidates, roi_points, evaluator, cfg.portfolio)

    # The honest number. Selection maximised ROI on `roi_points`, so the ROI
    # it reports there is an upward-biased estimate of its own objective. The
    # same lineups priced on simulations they were never selected against is
    # the estimate to believe -- and the gap between the two is a direct
    # readout of how much of the edge was noise.
    holdout = ContestEvaluator(
        field.scores(build_points), payouts, smoothing=cfg.portfolio.payout_smoothing
    )
    entered = np.asarray(portfolio.lineups)
    holdout_roi = float(holdout.roi(build_points[:, entered].sum(axis=2)).mean())
    del holdout

    return RunResult(
        slate=slate,
        calibration=cal,
        sim=sim,
        projections=projections,
        ownership=own,
        field=field,
        portfolio=portfolio,
        evaluator=evaluator,
        candidates=candidates,
        ownership_info=own_info,
        holdout_roi=holdout_roi,
    )
