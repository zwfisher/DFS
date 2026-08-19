"""Command line entry point."""

from __future__ import annotations

import argparse
import json
import sys

import numpy as np
import pandas as pd

from . import pipeline
from .config import BELLERIVE, Config
from .data import datagolf as dg
from .data.cache import fetch_json, save_fixture
from .data.dk import CONTEST_URL, DRAFTABLES_URL
from .projections.calibrate import Calibration
from .slate import build_slate

# The contest this package was built for. Both are overridable; nothing else
# in the package hard-codes an event.
DEFAULT_CONTEST = 193_766_688
DEFAULT_COURSE = "Bellerive CC"

PROJECTION_COLUMNS = [
    "golfer", "salary", "proj", "sd", "floor", "ceiling",
    "win", "top5", "top10", "top20", "own_dg", "own", "leverage", "value",
]


def _fmt(df: pd.DataFrame) -> str:
    return df.to_string(index=False, float_format=lambda x: f"{x:,.3f}")


def _common(parser: argparse.ArgumentParser) -> None:
    parser.add_argument("--contest", type=int, default=DEFAULT_CONTEST)
    parser.add_argument("--course", default=DEFAULT_COURSE)
    parser.add_argument(
        "--live", action="store_true",
        help="fetch from DraftKings and DataGolf instead of the shipped snapshot",
    )
    parser.add_argument("--sims", type=int, default=None)
    parser.add_argument("--salaries", default=None, help="DKSalaries.csv fallback")


def _config(args) -> Config:
    cfg = Config()
    if args.sims:
        cfg.sim.n_sims = args.sims
    return cfg


def cmd_fetch(args) -> int:
    """Refresh the shipped snapshot from the live sources."""
    for page, kwargs in (
        ("rankings", {}),
        ("course_fit", {"course": args.course}),
        ("fantasy", {}),
        ("finish_odds", {}),
    ):
        dg.snapshot(page, max_age=0.0, **kwargs)
        print(f"snapshot datagolf/{page}")
    contest = fetch_json(
        CONTEST_URL.format(contest_id=args.contest), f"dk_contest_{args.contest}.json", max_age=0.0
    )
    save_fixture(f"dk_contest_{args.contest}.json", contest)
    group = contest["contestDetail"]["draftGroupId"]
    save_fixture(
        f"dk_draftables_{group}.json",
        fetch_json(DRAFTABLES_URL.format(draft_group=group), f"dk_draftables_{group}.json", max_age=0.0),
    )
    print(f"snapshot draftkings contest {args.contest}, draft group {group}")
    return 0


def cmd_slate(args) -> int:
    slate = build_slate(
        args.contest, args.course, offline=not args.live, salaries_csv=args.salaries
    )
    print(f"{slate.event} at {slate.course}: {len(slate)} golfers")
    if slate.contest:
        c = slate.contest
        print(f"{c.name}: ${c.entry_fee:.0f} entry, {c.entries:,}/{c.max_entries:,} entered, "
              f"${c.total_payouts:,.0f} to {c.paid_places:,} places")
    for source, names in slate.warnings.items():
        if names:
            print(f"  warning: no {source} for {', '.join(names)}")
    df = pd.DataFrame(
        {
            "golfer": slate.names,
            "salary": slate.salaries,
            "skill": [g.skill for g in slate.golfers],
            "course_fit": [g.course_fit for g in slate.golfers],
            "edge": slate.edge,
            "wave": slate.waves,
            "dg_own": slate.ownership,
        }
    ).sort_values("edge", ascending=False)
    print(_fmt(df))
    return 0


def cmd_calibrate(args) -> int:
    cfg = _config(args)
    slate = build_slate(args.contest, args.course, offline=not args.live)
    cal = pipeline.load_or_fit_calibration(
        slate, cfg, args.contest, offline=not args.live, refit=args.refit
    )
    _print_calibration(cal)
    return 0


def _print_calibration(cal: Calibration) -> None:
    print(f"latent per stroke   {cal.latent_per_stroke:.4f}")
    print(f"course shift        {cal.course_shift:+.4f}")
    print(f"talent multiplier   {cal.talent_multiplier:.4f}")
    print(f"week sd (strokes)   {cal.week_sd:.4f}")
    print(f"field score to par  {cal.field_score_to_par:+.4f}")
    print(f"tie rule            {cal.tie_rule}")
    if cal.diagnostics:
        print(json.dumps(cal.diagnostics, indent=1, default=float)[:4000])


def cmd_project(args) -> int:
    result = pipeline.run(
        args.contest, args.course, _config(args),
        offline=not args.live, refit=args.refit, salaries_csv=args.salaries,
    )
    print(_fmt(result.projections[PROJECTION_COLUMNS]))
    if args.out:
        result.projections.to_csv(args.out, index=False)
        print(f"\nwrote {args.out}")
    return 0


def cmd_optimize(args) -> int:
    cfg = _config(args)
    cfg.portfolio.n_lineups = args.lineups
    cfg.portfolio.n_candidates = args.candidates
    cfg.portfolio.max_exposure = args.max_exposure
    result = pipeline.run(
        args.contest, args.course, cfg,
        offline=not args.live, refit=args.refit, salaries_csv=args.salaries,
    )
    print(_fmt(result.projections[PROJECTION_COLUMNS].head(args.top)))
    print()
    print(_fmt(result.lineup_frame()))
    n = len(result.portfolio.lineups)
    print(
        f"\nportfolio: {n} entries, expected prize ${result.portfolio.total_ev:,.2f} on the "
        f"smoothed payout curve (${result.portfolio.raw_ev:,.2f} on the real one)"
    )
    print(
        f"ROI per entry: {result.portfolio.roi:+.1%} where the lineups were chosen, "
        f"{result.holdout_roi:+.1%} on held-out simulations -- believe the second"
    )
    info = result.ownership_info
    if info.get("tilt"):
        print(
            f"ownership was tilted to fit the cap: implied spend "
            f"${info['spend_before']:,.0f} -> ${info['spend_after']:,.0f}"
        )
    print(
        f"simulated field: {len(result.field):,} lineups, "
        f"ownership rmse {100 * float(np.sqrt(((result.field.ownership - result.field.target) ** 2).mean())):.2f} points"
    )
    exposure = result.portfolio.exposures(len(result.slate))
    top = np.argsort(-exposure)[: args.top]
    print("\nexposure:")
    print(_fmt(pd.DataFrame({
        "golfer": [result.slate.names[i] for i in top],
        "entries": (exposure[top] * n).astype(int),
        "exposure": exposure[top],
        "field_own": result.ownership[top],
    })))
    if args.out:
        path = result.to_draftkings_csv(args.out)
        print(f"\nwrote {path} -- upload straight into the contest")
    return 0


def cmd_diagnose(args) -> int:
    from .tools_diagnose import diagnose

    return diagnose(args)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(prog="pgadfs", description=__doc__)
    subs = parser.add_subparsers(dest="command", required=True)

    p = subs.add_parser("fetch", help="refresh the shipped data snapshot")
    _common(p)
    p.set_defaults(func=cmd_fetch)

    p = subs.add_parser("slate", help="show the field, salaries and talent")
    _common(p)
    p.set_defaults(func=cmd_slate)

    p = subs.add_parser("calibrate", help="fit the simulator to market prices")
    _common(p)
    p.add_argument("--refit", action="store_true")
    p.set_defaults(func=cmd_calibrate)

    p = subs.add_parser("project", help="projected points, finish odds and ownership")
    _common(p)
    p.add_argument("--refit", action="store_true")
    p.add_argument("--out", default=None)
    p.set_defaults(func=cmd_project)

    p = subs.add_parser("optimize", help="build a portfolio of lineups")
    _common(p)
    p.add_argument("--refit", action="store_true")
    p.add_argument("--lineups", type=int, default=20)
    p.add_argument("--candidates", type=int, default=400)
    p.add_argument("--max-exposure", type=float, default=0.60)
    p.add_argument("--top", type=int, default=20)
    p.add_argument("--out", default=None, help="write a DraftKings upload CSV")
    p.set_defaults(func=cmd_optimize)

    p = subs.add_parser("diagnose", help="check the simulator against known aggregates")
    _common(p)
    p.set_defaults(func=cmd_diagnose)

    args = parser.parse_args(argv)
    return args.func(args)


if __name__ == "__main__":
    sys.exit(main())
