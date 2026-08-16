"""Does stacking actually pay, and in which contests?

MLB is not football. There is no QB-to-receiver link where one play scores
two players directly, and same-team hitter correlation comes out around
+0.10 -- far below what the football analogy suggests. So "stack in MLB
GPPs" deserves to be measured rather than inherited as folklore.

The experiment holds everything constant except team concentration: one
slate, one simulation, one field, two candidate pools. The stacked pool
uses the configured stack shapes; the spread pool is capped at two hitters
per team with no stack constraint at all. Both are then priced through the
same payout curves, so the comparison is not confounded by projections,
ownership or field strength.

Run it on synthetic data (no network) or against a real draft group:

    python tools/stacking_value.py
    python tools/stacking_value.py --draft-group 152178 --date 2026-08-16
"""

from __future__ import annotations

import argparse
import dataclasses
import sys
from pathlib import Path

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from mlbdfs.config import ROSTER  # noqa: E402
from mlbdfs.optimize.contest import double_up, large_gpp, single_entry_gpp  # noqa: E402
from mlbdfs.optimize.milp import LineupOptimizer  # noqa: E402
from mlbdfs.optimize.portfolio import evaluate_lineups, field_standing  # noqa: E402
from mlbdfs.ownership.field import generate_field  # noqa: E402
from mlbdfs.ownership.heuristic import project_ownership  # noqa: E402
from mlbdfs.ownership.uncertainty import ownership_interval  # noqa: E402
from mlbdfs.projections.build import build_sim_slate  # noqa: E402
from mlbdfs.sim.engine import simulate_slate  # noqa: E402


def build_pools(slate, sim, n_candidates: int, seed: int) -> dict:
    """A stacked pool and a deliberately un-stacked one."""
    stacked = LineupOptimizer(slate).generate_pool(
        sim.scores, sim.player_ids, n_candidates=n_candidates, seed=seed
    )
    # Two per team is the tightest cap that still admits a legal lineup at
    # every position, and it removes stacking rather than merely discouraging
    # it. Passing no stack shapes as well, so nothing steers the solve.
    spread_rules = dataclasses.replace(ROSTER, max_hitters_per_team=2)
    spread = LineupOptimizer(slate, rules=spread_rules).generate_pool(
        sim.scores, sim.player_ids, n_candidates=n_candidates, seed=seed,
        stack_shapes=(),
    )
    return {"stacked": stacked, "spread": spread}


def compare(slate, sim, holdout, field, contests, n_candidates=120, seed=5,
            top_n=8) -> tuple[pd.DataFrame, pd.DataFrame]:
    pools = build_pools(slate, sim, n_candidates, seed)

    shape_rows, roi_rows = [], []
    for label, pool in pools.items():
        standing = field_standing(pool, holdout.scores, holdout.player_ids, field)
        totals = standing.scores
        shape_rows.append({
            "pool": label,
            "lineups": len(pool),
            "mean": totals.mean(),
            "sd": totals.std(),
            "p90": np.quantile(totals, 0.90),
            "p99": np.quantile(totals, 0.99),
            "p99.9": np.quantile(totals, 0.999),
        })
        for name, contest in contests.items():
            scored = evaluate_lineups(
                pool, holdout.scores, holdout.player_ids, field, contest,
                standing=standing,
            )
            best = scored.nlargest(top_n, "roi")
            roi_rows.append({
                "contest": name,
                "pool": label,
                "roi": best["roi"].mean(),
                "p_win": best["p_win"].mean(),
                "p_cash": best["p_cash"].mean(),
                "mean_score": best["mean_score"].mean(),
            })
    return pd.DataFrame(shape_rows), pd.DataFrame(roi_rows)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--draft-group", type=int, dest="draft_group")
    parser.add_argument("--date")
    parser.add_argument("--games", type=int, default=8)
    parser.add_argument("--sims", type=int, default=8000)
    parser.add_argument("--field", type=int, default=12_000)
    parser.add_argument("--candidates", type=int, default=120)
    parser.add_argument("--seed", type=int, default=5)
    args = parser.parse_args()

    if args.draft_group:
        from mlbdfs.cli import _load_real_slate

        slate, book = _load_real_slate(
            argparse.Namespace(
                draft_group=args.draft_group, slate=None, date=args.date,
                totals=None, no_projected_lineups=False,
            )
        )
    else:
        from mlbdfs.data.fixtures import make_slate

        slate, book = make_slate(n_games=args.games, seed=args.seed)

    sim_slate = build_sim_slate(slate, book)
    sim = simulate_slate(sim_slate, n_sims=args.sims, seed=args.seed)
    holdout = simulate_slate(sim_slate, n_sims=args.sims, seed=args.seed + 9973)
    ownership = ownership_interval(project_ownership(slate, sim))
    field = generate_field(slate, ownership, n_lineups=args.field, seed=args.seed)

    contests = {
        "large GPP (top heavy)": large_gpp(50_000, 1.0),
        "small GPP": single_entry_gpp(2_000, 1.0),
        "double-up (flat)": double_up(10_000, 1.0),
    }

    shape, roi = compare(
        slate, sim, holdout, field, contests,
        n_candidates=args.candidates, seed=args.seed,
    )

    pd.set_option("display.width", 200)
    print("\n-- candidate score distributions --")
    print(shape.round(2).to_string(index=False))
    print("\n-- best 8 from each pool, per contest --")
    print(roi.round(4).to_string(index=False))

    wide = roi.pivot(index="contest", columns="pool", values="roi")
    wide["stacked edge"] = wide["stacked"] / wide["spread"] - 1.0
    print("\n-- stacking's ROI edge by payout shape --")
    print(wide.round(4).to_string())
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
