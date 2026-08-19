"""Checks the simulator against numbers it was never told.

The hole model is fitted to a table of category frequencies and then bent by
three calibrated parameters. That leaves plenty of room for it to reproduce
the things it was fitted on while getting the things it was not badly wrong,
and those are the ones that matter: a bogey-free round is worth 3 points, an
all-under-70 week is worth 5, and neither appears anywhere in the fit.
"""

from __future__ import annotations

from dataclasses import replace

import numpy as np
import pandas as pd

from .config import Config
from .data import datagolf as dg
from .data.dkpoints import observed_bonus_rates
from .pipeline import load_or_fit_calibration
from .projections.calibrate import apply_calibration
from .sim.engine import simulate
from .slate import build_slate

# Ranges to orient against, not targets to hit. The bonus rates are anchored
# on a real DraftKings scoring file -- the 2021 PGA Championship at Kiawah,
# where the field played four rounds at roughly +3 to par -- widened upward
# because that event is at the hard end of what a tour course does. Anything
# outside these is worth explaining before trusting the output.
REFERENCE = {
    "birdies or better per round": (2.4, 4.4),
    "bogey-free rounds": (0.008, 0.09),
    "rounds with a 3-birdie streak": (0.03, 0.16),
    "hole scoring points per round": (10.0, 19.0),
    "round score sd (strokes)": (2.7, 3.6),
    "72-hole total sd (strokes)": (6.0, 8.0),
    "eagles per round": (0.05, 0.16),
    "aces per par-3 round": (0.0002, 0.0008),
    "doubles or worse per round": (0.25, 0.85),
}


def diagnose(args) -> int:
    cfg = Config()
    if getattr(args, "sims", None):
        cfg.sim.n_sims = args.sims
    offline = not getattr(args, "live", False)
    slate = build_slate(args.contest, args.course, offline=offline)
    cal = load_or_fit_calibration(slate, cfg, args.contest, offline=offline, refit=False)
    scaled = apply_calibration(slate, cal)
    sim_cfg = replace(
        cfg.sim,
        week_sd=cal.week_sd,
        tie_rule=cal.tie_rule,
        field_score_to_par=cal.field_score_to_par,
    )
    sim = simulate(scaled, sim_cfg, cal.model(cfg.sim.course))

    rounds = sim_cfg.rounds
    n_par3 = sum(1 for p in cfg.sim.course.pars if p == 3)
    par = cfg.sim.course.par

    # Bonus points decompose exactly, which is what makes these observable:
    # the only way to earn 3 points is a streak or a bogey-free round, and
    # the only way to earn 5 is four rounds under 70.
    strokes_sd_total = sim.strokes.std(axis=0).mean()
    per_round = (sim.strokes - par * rounds) / rounds

    measured = {
        "birdies or better per round": sim.birdies.mean() / rounds,
        "eagles per round": sim.eagles.mean() / rounds,
        "aces per par-3 round": sim.aces.mean() / (rounds * n_par3),
        "doubles or worse per round": sim.doubles.mean() / rounds,
        "bogey-free rounds": sim.bogey_free_rounds.mean() / rounds,
        "rounds with a 3-birdie streak": sim.streak_rounds.mean() / rounds,
        "rounds under 70": sim.rounds_under_70.mean() / rounds,
        "hole scoring points per round": float(sim.hole_points.mean()) / rounds,
        "round score sd (strokes)": float(np.sqrt(max(strokes_sd_total**2 / rounds, 0.0))),
        "72-hole total sd (strokes)": float(strokes_sd_total),
        "field score to par per round": float(per_round.mean()),
        "mean DraftKings points": float(sim.points.mean()),
        "mean finish points": float(sim.finish_points.mean()),
        "mean bonus points": float(sim.bonus_points.mean()),
    }

    observed = observed_bonus_rates()
    rows = []
    for name, value in measured.items():
        lo, hi = REFERENCE.get(name, (np.nan, np.nan))
        ok = "" if np.isnan(lo) else ("ok" if lo <= value <= hi else "OUT")
        rows.append({
            "metric": name,
            "simulated": value,
            "2021 PGA": observed.get(name, np.nan),
            "low": lo,
            "high": hi,
            "": ok,
        })
    print(pd.DataFrame(rows).to_string(index=False, float_format=lambda x: f"{x:,.4f}"))

    # Does the simulated leaderboard price like the market's?
    odds = dg.load_finish_odds(offline=offline)
    print("\nfinish probabilities, best to worst in the field:")
    frames = []
    for market, k in (("win", 1), ("top_5", 5), ("top_10", 10), ("top_20", 20)):
        simulated = np.sort(sim.prob_top(k))[::-1]
        model = np.sort([o.model[market] for o in odds if market in o.model])[::-1]
        book = np.sort([o.market[market] for o in odds if market in o.market])[::-1]
        frames.append(
            pd.DataFrame(
                {
                    "market": market,
                    "rank": np.arange(1, 6),
                    "simulated": simulated[:5],
                    "datagolf": model[:5],
                    "sportsbook": book[:5],
                }
            )
        )
    print(pd.concat(frames).to_string(index=False, float_format=lambda x: f"{x:.4f}"))

    # DataGolf publishes a projected DK-score SD for every golfer on the
    # slate, unmasked. That is fifty independent checks on the variance.
    published = np.array([g.dg_score_sd or np.nan for g in slate.golfers])
    got = sim.points.std(axis=0)
    ok = ~np.isnan(published)
    if ok.any():
        print(
            f"\nDraftKings score SD vs DataGolf, {ok.sum()} golfers: "
            f"mean simulated {got[ok].mean():.2f} vs published {published[ok].mean():.2f}, "
            f"correlation {np.corrcoef(got[ok], published[ok])[0, 1]:.3f}, "
            f"rmse {np.sqrt(np.mean((got[ok] - published[ok]) ** 2)):.2f}"
        )
    return 0
