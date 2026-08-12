"""Command line interface.

``mlbdfs demo`` runs the whole pipeline on synthetic data and needs no
network, which is the fastest way to see what the project does. The other
commands take a DraftKings salary export and pull real data.
"""

from __future__ import annotations

import argparse
import sys
from datetime import date, datetime
from pathlib import Path

import pandas as pd

from .config import CACHE_DIR, OWNERSHIP
from .optimize.contest import CONTESTS
from .optimize.portfolio import compare_objectives, exposure_report
from .pipeline import run_pipeline


def _show(frame: pd.DataFrame, n: int | None = None) -> None:
    with pd.option_context("display.max_columns", None, "display.width", 200):
        print((frame.head(n) if n else frame).to_string(index=False))


def _load_real_slate(args) -> tuple:
    """Assemble a slate and rate book from DraftKings plus live data."""
    from .data import ids, sources
    from .data.dk import build_slate, infer_starters, parse_salaries, unmatched_report
    from .projections.build import rate_book_from_counts

    salaries = parse_salaries(args.slate)
    game_date = (
        datetime.strptime(args.date, "%Y-%m-%d").date() if args.date else date.today()
    )

    print(f"{len(salaries)} players, {salaries['team'].nunique()} teams")

    probables = sources.probable_pitchers(game_date)
    starters = infer_starters(salaries, probables)
    print(f"{len(starters)} announced starters matched")

    lineups = sources.confirmed_lineups(game_date)
    dk_lineups = (
        ids.lineups_to_dk_ids(lineups, salaries) if not lineups.empty else None
    )
    posted = 0 if dk_lineups is None else len(dk_lineups)
    print(f"{posted} hitters with a posted batting order")

    totals = {}
    if args.totals:
        totals = pd.read_csv(args.totals).set_index("team")["total"].to_dict()
    else:
        print("no Vegas totals supplied; using league average for every team")

    slate = build_slate(
        salaries,
        lineups=dk_lineups,
        starters=starters,
        team_totals=totals,
        name=Path(args.slate).stem,
    )

    seasons = [game_date.year - 1, game_date.year]
    batters = pd.concat([sources.batter_counts(s) for s in seasons], ignore_index=True)
    pitchers = pd.concat(
        [sources.pitcher_counts(s) for s in seasons], ignore_index=True
    )
    id_map = ids.map_dk_to_mlbam(salaries)

    unmatched = ids.match_report(salaries, id_map)
    if not unmatched.empty:
        print(f"\n{len(unmatched)} players unmatched to MLBAM ids "
              f"(they get league-average rates):")
        _show(unmatched, 10)

    missing_order = unmatched_report(salaries, dk_lineups)
    if not missing_order.empty:
        print(f"\n{len(missing_order)} hitters without a posted lineup spot")

    book = rate_book_from_counts(batters, pitchers, id_map=id_map)
    return slate, book


def cmd_demo(args) -> int:
    from .data.fixtures import make_slate

    slate, book = make_slate(n_games=args.games, seed=args.seed)
    contest = CONTESTS[args.contest](args.entries, args.fee)
    result = run_pipeline(
        slate,
        book,
        contest=contest,
        n_sims=args.sims,
        n_field=args.field,
        n_candidates=args.candidates,
        n_lineups=args.lineups,
        seed=args.seed,
    )
    _report(result, args)
    return 0


def cmd_project(args) -> int:
    from .sim.engine import simulate_slate
    from .ownership.heuristic import project_ownership, team_stack_ownership
    from .projections.build import build_sim_slate

    slate, book = _load_real_slate(args)
    sim = simulate_slate(build_sim_slate(slate, book), n_sims=args.sims, seed=args.seed)
    own = project_ownership(slate, sim)

    summary = sim.summary().set_index("player_id")
    out = own.set_index("player_id").join(summary[["mean", "sd", "p10", "p90"]])
    out["value"] = out["mean"] / (out["salary"] / 1000.0)
    out = out.reset_index().sort_values("mean", ascending=False)

    print("\n-- projections --")
    _show(
        out[["name", "team", "position", "salary", "mean", "sd", "p10", "p90",
             "value", "ownership"]].round(3),
        args.top,
    )
    print("\n-- team stacks --")
    _show(team_stack_ownership(own).round(3), 10)

    if args.out:
        out.to_csv(args.out, index=False)
        print(f"\nwrote {args.out}")
    return 0


def cmd_optimize(args) -> int:
    slate, book = _load_real_slate(args)
    contest = CONTESTS[args.contest](args.entries, args.fee)
    result = run_pipeline(
        slate,
        book,
        contest=contest,
        n_sims=args.sims,
        n_field=args.field,
        n_candidates=args.candidates,
        n_lineups=args.lineups,
        seed=args.seed,
    )
    _report(result, args)
    return 0


def _report(result, args) -> None:
    from .ownership.heuristic import implied_field_mean

    print("\n-- top projections --")
    proj = result.projections()
    _show(
        proj[["name", "team", "position", "salary", "mean", "p10", "p90",
              "ownership", "own_p10", "own_p90"]].round(3),
        12,
    )

    print(f"\nimplied field mean score: {implied_field_mean(result.ownership):.1f}")

    print("\n-- selected lineups --")
    _show(result.selected.round(4))

    print("\n-- exposure --")
    _show(exposure_report(result.pool, result.selected, result.slate).round(3), 12)

    print("\n-- points versus ROI as the objective --")
    _show(compare_objectives(result.evaluation, top_n=args.lineups).round(4))

    if args.out:
        rows = []
        for row in result.selected.itertuples():
            lineup = result.pool[int(row.lineup) - 1]
            entry = {"lineup": int(row.lineup), "roi": row.roi, "salary": lineup.salary}
            for n, pid in enumerate(lineup.player_ids, start=1):
                entry[f"slot{n}"] = result.slate.player(pid).name
                entry[f"slot{n}_id"] = pid
            rows.append(entry)
        pd.DataFrame(rows).to_csv(args.out, index=False)
        print(f"\nwrote {args.out}")


def cmd_diagnose(args) -> int:
    sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
    from tools.diagnose_sim import main as diagnose

    return diagnose(n_games=args.games, n_sims=args.sims)


def cmd_cache(args) -> int:
    from .data.cache import clear, describe

    if args.clear:
        print(f"deleted {clear(args.name)} cached files")
        return 0
    frame = describe()
    if frame.empty:
        print(f"cache at {CACHE_DIR} is empty")
    else:
        _show(frame)
        print(f"\n{frame['mb'].sum():.1f} MB in {CACHE_DIR}")
    return 0


def cmd_log_ownership(args) -> int:
    from .ownership.logger import accuracy_report, log_slate

    slate_date = datetime.strptime(args.date, "%Y-%m-%d").date()
    projected = pd.read_csv(args.projected) if args.projected else None
    realized = log_slate(args.standings, slate_date, args.contest_name, projected)

    print(f"logged {len(realized)} players from {args.contest_name}")
    _show(realized.head(12).round(4))

    report = accuracy_report()
    if not report.empty:
        print("\n-- projection accuracy across logged slates --")
        _show(report.round(4))
    return 0


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="mlbdfs", description="MLB DFS projections, simulation and optimization"
    )
    sub = parser.add_subparsers(dest="command", required=True)

    def add_common(p, with_slate: bool = True):
        if with_slate:
            p.add_argument("--slate", required=True, help="DraftKings salary CSV")
            p.add_argument("--date", help="slate date, YYYY-MM-DD (default today)")
            p.add_argument("--totals", help="CSV of team,total Vegas implied runs")
        p.add_argument("--sims", type=int, default=10_000)
        p.add_argument("--seed", type=int, default=1)
        p.add_argument("--out", help="write results to this CSV")

    def add_contest(p):
        p.add_argument("--contest", choices=sorted(CONTESTS), default="large_gpp")
        p.add_argument("--entries", type=int, default=50_000)
        p.add_argument("--fee", type=float, default=5.0)
        p.add_argument("--field", type=int, default=20_000)
        p.add_argument("--candidates", type=int, default=200)
        p.add_argument("--lineups", type=int, default=20)

    p = sub.add_parser("demo", help="run the full pipeline on synthetic data, offline")
    add_common(p, with_slate=False)
    add_contest(p)
    p.add_argument("--games", type=int, default=8)
    p.set_defaults(func=cmd_demo)

    p = sub.add_parser("project", help="projections and ownership for a slate")
    add_common(p)
    p.add_argument("--top", type=int, default=30)
    p.set_defaults(func=cmd_project)

    p = sub.add_parser("optimize", help="build and rank lineups for a slate")
    add_common(p)
    add_contest(p)
    p.set_defaults(func=cmd_optimize)

    p = sub.add_parser("diagnose", help="check the simulator against league aggregates")
    p.add_argument("--games", type=int, default=8)
    p.add_argument("--sims", type=int, default=8_000)
    p.set_defaults(func=cmd_diagnose)

    p = sub.add_parser("cache", help="inspect or clear the data cache")
    p.add_argument("--clear", action="store_true")
    p.add_argument("--name", help="limit clearing to one fetcher")
    p.set_defaults(func=cmd_cache)

    p = sub.add_parser(
        "log-ownership", help="record realized ownership from a DK standings export"
    )
    p.add_argument("--standings", required=True)
    p.add_argument("--date", required=True)
    p.add_argument("--contest-name", required=True, dest="contest_name")
    p.add_argument("--projected", help="CSV written by `project`, to pair with")
    p.set_defaults(func=cmd_log_ownership)

    return parser


def main(argv: list[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    return args.func(args)


if __name__ == "__main__":
    raise SystemExit(main())
