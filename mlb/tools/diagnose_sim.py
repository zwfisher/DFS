"""Simulator realism diagnostics.

Run this after touching anything in the projection or simulation layers. The
targets are league aggregates the simulator should reproduce without being
told about them -- if runs per game or the strikeout rate drift, the rate
model or the baserunning model has broken, and every projection above it is
wrong in a way that a unit test on a single plate appearance will not catch.
"""

from __future__ import annotations

import sys
import time
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from mlbdfs.data.fixtures import make_neutral_game, make_slate  # noqa: E402
from mlbdfs.projections.build import build_sim_slate  # noqa: E402
from mlbdfs.sim.engine import simulate_slate, stack_distribution  # noqa: E402

# Slate-level targets. The run distribution is checked separately on a
# neutral matchup, because pooling the synthetic slate's wide spread of team
# totals inflates the tail no matter what the engine does.
TARGETS = {
    "runs_per_team_game": (4.2, 4.8),
    "p_shutout": (0.05, 0.11),
    "p_ten_plus": (0.03, 0.09),
    "sp_innings": (5.0, 5.7),
    "sp_points": (11.0, 17.0),
    # A league-average hitter across all nine slots scores about 7.2: roughly
    # 8.6 points of runs, 8.3 of RBI and 47 of hits/walks per team game,
    # spread over nine hitters. Rostered hitters score more because they are
    # better than average, not because the league average is higher.
    "hitter_points": (6.8, 8.0),
    # Real MLB spreads slots 1-9 by about 0.70 PA; this model spreads them by
    # about 0.88 because it never substitutes. Pinch hitters and double
    # switches redistribute plate appearances away from the top of the order
    # in reality, so the bottom of the order here is modestly pessimistic.
    "pa_slot1": (4.55, 4.85),
    "pa_slot9": (3.75, 4.05),
}


def main(n_games: int = 8, n_sims: int = 8000) -> int:
    slate, book = make_slate(n_games=n_games, seed=11)
    sim_slate = build_sim_slate(slate, book)

    t0 = time.time()
    res = simulate_slate(sim_slate, n_sims=n_sims, seed=3)
    elapsed = time.time() - t0

    print(f"{n_sims} sims x {n_games} games in {elapsed:.2f}s "
          f"({n_sims * n_games / elapsed / 1000:.0f}k game-sims/sec)")
    print(f"score matrix: {res.scores.shape} "
          f"{res.scores.nbytes / 1e6:.1f} MB\n")

    runs = np.stack(list(res.team_runs.values()))
    summary = res.summary()
    is_p = np.array([slate.player(p).is_pitcher for p in summary.player_id])

    outs = np.stack(list(res.starter_outs.values()))
    observed = {
        "runs_per_team_game": runs.mean(),
        "p_shutout": (runs == 0).mean(),
        "p_ten_plus": (runs >= 10).mean(),
        "sp_innings": outs.mean() / 3.0,
        "sp_points": summary["mean"][is_p].mean(),
        "hitter_points": summary["mean"][~is_p].mean(),
        "pa_slot1": _mean_pa_for_slot(res, slate, 1),
        "pa_slot9": _mean_pa_for_slot(res, slate, 9),
    }

    ok = True
    print(f"{'metric':24s} {'observed':>10s}  {'target':>16s}")
    for key, (lo, hi) in TARGETS.items():
        val = float(observed[key])
        good = lo <= val <= hi
        ok &= good
        flag = "ok " if good else "OFF"
        print(f"{key:24s} {val:10.3f}  [{lo:6.2f},{hi:6.2f}] {flag}")

    print("\n-- neutral matchup run distribution --")
    nslate, nbook = make_neutral_game()
    nres = simulate_slate(build_sim_slate(nslate, nbook), n_sims=30000, seed=2)
    nruns = np.stack(list(nres.team_runs.values())).ravel()
    for label, val, (lo, hi) in [
        ("mean runs", nruns.mean(), (4.15, 4.55)),
        ("sd runs", nruns.std(), (2.85, 3.25)),
        ("P(shutout)", (nruns == 0).mean(), (0.060, 0.085)),
        ("P(10+ runs)", (nruns >= 10).mean(), (0.040, 0.070)),
    ]:
        good = lo <= val <= hi
        ok &= good
        print(f"{label:24s} {val:10.3f}  [{lo:6.2f},{hi:6.2f}] {'ok ' if good else 'OFF'}")

    print("\n-- stack correlation check --")
    team = slate.teams[0]
    lineup = [p.player_id for p in slate.lineup_for(team)][:5]
    dist = stack_distribution(res, lineup)
    print(f"5-stack ({team}) mean={dist['mean']:.1f} sd={dist['sd']:.1f} "
          f"p90={dist['p90']:.1f} p99={dist['p99']:.1f}")
    print(f"same players, decorrelated  sd={dist['independent_sd']:.1f} "
          f"p90={dist['independent_p90']:.1f} p99={dist['independent_p99']:.1f}")
    lift = dist["p99"] / dist["independent_p99"] - 1.0
    print(f"correlated 99th percentile lift: {lift:+.1%}")
    if lift < 0.05:
        print("OFF: stack correlation is not showing up in the tail")
        ok = False

    corr = res.correlation(lineup)
    off_diag = corr[np.triu_indices(len(lineup), k=1)]
    print(f"mean pairwise teammate correlation: {off_diag.mean():+.3f}")

    opp_sp = slate.starter_for(slate.player(lineup[0]).opponent)
    if opp_sp is not None:
        c = res.correlation([lineup[0], opp_sp.player_id])[0, 1]
        print(f"hitter vs opposing starter correlation: {c:+.3f}")

    print("\nPASS" if ok else "\nFAIL")
    return 0 if ok else 1


def _mean_pa_for_slot(res, slate, slot: int) -> float:
    """Average plate appearances for one batting order slot across the slate."""
    vals = [
        res.player_pa[p.player_id].mean()
        for p in slate.players
        if p.batting_order == slot and p.player_id in res.player_pa
    ]
    return float(np.mean(vals))


if __name__ == "__main__":
    raise SystemExit(main())
