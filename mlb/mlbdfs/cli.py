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


def _slate_from_draft_group(draft_group: int) -> tuple:
    """Salaries, pitcher hands and probable starters, straight from the API.

    The draftables feed carries three things the CSV export does not: an
    injury status, throwing hand, and each player's opposing probable
    starter. Filtering on the first is the important one -- a slate export
    lists everyone on the 40-man, and an IL bat is a guaranteed zero that
    the optimizer will happily roster if it looks cheap enough.
    """
    from .data import lobby as dk_lobby

    frame = dk_lobby.fetch_draftables(draft_group)
    if frame.empty:
        raise SystemExit(f"draft group {draft_group} returned no players")

    starters_frame = dk_lobby.probable_starters(frame)
    out = frame[~frame["status"].isin(["IL", "OUT"])].copy()
    dropped = len(frame) - len(out)
    if dropped:
        print(f"dropped {dropped} players listed IL or OUT")

    hands = {
        str(r.dk_id): r.throws
        for r in out.itertuples()
        if r.is_pitcher and r.throws
    }
    starters = {
        r.team: str(r.dk_id)
        for r in starters_frame.itertuples()
        if str(r.dk_id) in set(out["dk_id"].astype(str))
    }
    keep = ["dk_id", "name", "salary", "team", "positions", "is_pitcher",
            "away", "home", "opponent"]
    return out[keep].reset_index(drop=True), hands, starters


def _load_real_slate(args) -> tuple:
    """Assemble a slate and rate book from DraftKings plus live data."""
    from .data import ids, sources
    from .data.dk import build_slate, infer_starters, parse_salaries, unmatched_report
    from .projections.build import rate_book_from_counts
    from .projections.projected_lineups import apply_to_slate

    game_date = (
        datetime.strptime(args.date, "%Y-%m-%d").date() if args.date else date.today()
    )
    draft_group = getattr(args, "draft_group", None)
    if draft_group:
        salaries, hand_by_dk_id, starters = _slate_from_draft_group(draft_group)
        slate_name = f"dg{draft_group}"
    else:
        salaries = parse_salaries(args.slate)
        hand_by_dk_id = {}
        slate_name = Path(args.slate).stem

    print(f"{len(salaries)} players, {salaries['team'].nunique()} teams")

    if not draft_group:
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
        name=slate_name,
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

    # Throwing hand drives both halves of the platoon adjustment, so it has
    # to come from data. Every Player defaults to right-handed, which would
    # quietly switch the whole thing off against left-handers.
    hands = sources.pitcher_handedness(game_date.year)
    hand_by_mlbam = dict(zip(hands["player_id"], hands["throws"]))
    for player in slate.players:
        if not player.is_pitcher:
            continue
        # DraftKings publishes handedness on the draftables feed, and it is
        # the more current of the two -- prefer it when we have it.
        if player.player_id in hand_by_dk_id:
            player.throws = hand_by_dk_id[player.player_id]
            continue
        mlbam = id_map.get(player.player_id)
        if mlbam in hand_by_mlbam:
            player.throws = hand_by_mlbam[mlbam]

    splits = None
    try:
        splits = pd.concat(
            [sources.batter_counts_by_hand(s) for s in seasons], ignore_index=True
        )
    except Exception as exc:  # platoon splits are an enhancement, not a hard need
        print(f"platoon splits unavailable ({exc}); using overall rates")

    book = rate_book_from_counts(
        batters, pitchers, id_map=id_map, batter_splits=splits
    )

    # Fill any lineup that has not posted yet from recent starts against the
    # same pitcher hand.
    unposted = [p for p in slate.players if not p.is_pitcher and not p.confirmed]
    if unposted and not getattr(args, "no_projected_lineups", False):
        history = pd.concat(
            [sources.lineup_history(s) for s in seasons], ignore_index=True
        )
        projections = apply_to_slate(
            slate, history, id_map=id_map, asof=game_date
        )
        covered = sum(
            1 for p in slate.players
            if not p.is_pitcher and p.batting_order and not p.confirmed
        )
        print(f"projected lineups for {len(projections)} teams "
              f"({covered} hitters), conditioned on the opposing starter's hand")

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
        allow_unconfirmed=getattr(args, "allow_unconfirmed", True),
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


def _resolve_contest(args):
    """A real DraftKings contest when one is named, otherwise a shape.

    The synthetic curves in ``optimize.contest`` are the right thing for a
    demo and the wrong thing for a decision -- absolute ROI is only as
    honest as the payout table it was computed against.
    """
    contest_id = getattr(args, "contest_id", None)
    if not contest_id:
        return CONTESTS[args.contest](args.entries, args.fee)

    from .data import lobby as dk_lobby

    payload = dk_lobby.fetch_lobby(getattr(args, "sport", "MLB"))
    rows = dk_lobby.lobby_contests(payload)
    match = rows[rows["contest_id"] == contest_id]
    if match.empty:
        raise SystemExit(f"contest {contest_id} is not in the {args.sport} lobby")
    bands = dk_lobby.fetch_payouts(contest_id)
    if bands.empty:
        raise SystemExit(f"contest {contest_id} publishes no cash payouts")
    contest = dk_lobby.to_contest(match.iloc[0], bands)
    print(
        f"contest: {contest.name} -- ${contest.entry_fee:,.2f} entry, "
        f"{contest.n_entries:,} max entries, rake {contest.rake:.1%}, "
        f"{contest.max_entries_per_user} per user"
    )
    return contest


def _compare_contests(args, result) -> None:
    """Score the finished portfolio through every contest on the slate.

    The shape metrics in `mlbdfs contests` are what you can compute before
    a slate exists. This is the version that knows what you built.
    """
    from .data import lobby as dk_lobby
    from .optimize import screen as sc

    draft_group = getattr(args, "draft_group", None)
    payload = dk_lobby.fetch_lobby(getattr(args, "sport", "MLB"))
    rows = dk_lobby.lobby_contests(payload, draft_group)
    if rows.empty:
        print("\nno lobby contests to compare against")
        return

    rows = rows[
        (rows["prize_pool"] >= args.min_pool)
        & (rows["entry_fee"].between(args.min_fee, args.max_fee))
        & (~rows["name"].str.contains("Satellite|Qualifier", case=False))
    ].head(args.compare_contests)

    contests = []
    for row in rows.to_dict("records"):
        bands = dk_lobby.fetch_payouts(int(row["contest_id"]))
        if not bands.empty:
            contests.append(dk_lobby.to_contest(pd.Series(row), bands))
    if not contests:
        print("\nno contests with published cash payouts to compare against")
        return

    print(f"\n-- the same {len(result.selected)} lineups, priced into each contest --")
    _show(sc.compare_contests(result, contests).round(4))


def cmd_optimize(args) -> int:
    slate, book = _load_real_slate(args)
    contest = _resolve_contest(args)
    result = run_pipeline(
        slate,
        book,
        contest=contest,
        n_sims=args.sims,
        n_field=args.field,
        n_candidates=args.candidates,
        n_lineups=args.lineups,
        seed=args.seed,
        allow_unconfirmed=args.allow_unconfirmed,
        # The single most important number for making ROI mean anything, and
        # it is an observable: `mlbdfs backtest` prints it from a standings
        # export. Without it the simulated field sits near league average and
        # every ROI comes out inflated.
        field_mean_score=getattr(args, "field_mean", None),
        deterministic=getattr(args, "deterministic", False),
    )
    if getattr(args, "compare_contests", None):
        _compare_contests(args, result)
    _report(result, args)
    return 0


def cmd_backtest(args) -> int:
    from . import backtest as bt

    results = bt.load(args.standings)
    print(f"{len(results.entries)} entries, {len(results.players)} players rostered\n")

    print("-- score distribution --")
    _show(bt.score_distribution(results).round(2))

    print("\n-- cost of a zero-scoring player --")
    _show(bt.zero_analysis(results).round(3))

    print("\n-- zero rate by ownership tier (hitters) --")
    print("   a starting hitter posts an empty line about 20% of the time;")
    print("   far above that means those players never took the field\n")
    _show(bt.zeros_by_ownership(results).round(3))

    print("\n-- winners versus the field --")
    _show(bt.winners_versus_field(results).round(3))

    print("\n-- did the chalk pay off? --")
    _show(bt.chalk_performance(results).round(3))

    if args.username:
        summary = bt.user_summary(results, args.username)
        if summary.empty:
            print(f"\nno entries found for '{args.username}'")
        else:
            print(f"\n-- your entries ({args.username}) --")
            _show(summary.round(3))
            _show(bt.entries_for(results, args.username)
                  [["Rank", "Points", "zeros"]].head(25).round(2))

    if args.projected:
        acc = bt.ownership_accuracy(results, pd.read_csv(args.projected))
        if not acc.empty:
            print("\n-- ownership projection accuracy --")
            print(f"   mean absolute error {acc['error'].abs().mean():.3f}, "
                  f"bias {acc['error'].mean():+.3f}")
            _show(acc.head(15).round(3))

    print(f"\nSet target_field_mean_score = {results.field_mean_score:.1f} "
          "in config.py to calibrate the simulated field to this contest.")
    return 0


def cmd_lineup_accuracy(args) -> int:
    from . import backtest as bt
    from .data import sources

    if args.history:
        history = pd.read_parquet(args.history)
    else:
        history = sources.lineup_history(args.season)
    print(f"{len(history)} lineup rows, {history['game_date'].nunique()} dates\n")

    accuracy = bt.lineup_accuracy(history, max_dates=args.max_dates)
    if accuracy.empty:
        print("not enough history to backtest")
        return 1

    print("-- projected lineup accuracy (walk-forward) --")
    _show(bt.accuracy_summary(accuracy).round(3))
    print("\n   mean_hits_of_9 near 8 means the projection is doing real work;")
    print("   near 6 is roughly yesterday's card and not worth the machinery.")
    print("   If 'vs LHP' trails 'vs RHP' badly, the platoon conditioning is off.")

    if args.out:
        accuracy.to_csv(args.out, index=False)
        print(f"\nwrote {args.out}")
    return 0


def cmd_fit_ownership(args) -> int:
    from .ownership.fit import compare_to_heuristic, cross_validate, fit_ownership
    from .ownership.heuristic import implied_field_mean

    features = pd.read_parquet(args.features)
    print(f"{len(features)} players from {args.features}\n")

    model = fit_ownership(features, n_entries=args.entries, l2=args.l2)
    print("-- fitted coefficients --")
    _show(model.as_frame().pivot(
        index="feature", columns="group", values="coefficient"
    ).round(3).reset_index())

    print("\n-- fitted versus the current weights, on realized ownership --")
    _show(compare_to_heuristic(features, model).round(4))

    print("\n-- leave-one-position-out (hitter groups only) --")
    cv = cross_validate(features, n_entries=args.entries, l2=args.l2)
    _show(cv.round(4))

    predicted = model.predict(features)
    truth = float((features["ownership"] * features["proj"]).sum())
    print(f"\nimplied field mean: fitted {implied_field_mean(predicted):.1f} "
          f"against {truth:.1f} from realized ownership.")
    print("A fitted value far below that means the coefficients are flat -- "
          "check it before trusting the error metrics, which look fine when "
          "everything is predicted near zero.")

    if args.out:
        model.as_frame().to_csv(args.out, index=False)
        print(f"\nwrote {args.out}")
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


def cmd_contests(args) -> int:
    from .data import lobby as dk_lobby
    from .optimize import screen as sc

    payload = dk_lobby.fetch_lobby(args.sport)
    groups = dk_lobby.draft_groups(payload)

    if args.date:
        on = datetime.strptime(args.date, "%Y-%m-%d").date()
        groups = groups[groups["start_et"].dt.date == on]
    if args.classic_only:
        groups = groups[groups["game_type_id"] == 2]

    print("-- slates --")
    _show(groups)

    if not args.draft_group:
        print("\npick one with --draft-group to screen its contests")
        return 0

    contests = dk_lobby.lobby_contests(payload, args.draft_group)
    if contests.empty:
        print(f"\nno contests posted for draft group {args.draft_group}")
        return 0

    keep = contests[
        (contests["prize_pool"] >= args.min_pool)
        & (contests["entry_fee"].between(args.min_fee, args.max_fee))
        & (~contests["name"].str.contains("Satellite|Qualifier", case=False))
    ]
    if args.max_entries_per_user:
        keep = keep[keep["max_entries_per_user"] <= args.max_entries_per_user]
    if keep.empty:
        print(f"\nno contests match the filters (of {len(contests)} on this slate)")
        return 0

    payouts = {}
    for contest_id in keep["contest_id"]:
        try:
            payouts[int(contest_id)] = dk_lobby.fetch_payouts(int(contest_id))
        except Exception as exc:  # one bad contest should not sink the screen
            print(f"payouts unavailable for {contest_id}: {exc}")

    scored = sc.screen(keep, payouts)
    # Payout curves are not published for every contest, and are missing
    # wholesale for slates more than a day out -- fall back to ranking on
    # rake, which is always computable from the lobby alone.
    if "breakeven_rank" in scored:
        scored = scored.sort_values("breakeven_rank", ascending=False)
    else:
        print("no published payout curves yet; ranking on rake alone")
        scored = scored.sort_values("rake")
    cols = [
        "contest_id", "name", "entry_fee", "max_entries", "max_entries_per_user",
        "rake", "fill", "overlay_now", "pay_rate", "top_share", "min_cash_multiple",
        "breakeven_rank", "edge_sharpe",
    ]
    cols = [c for c in cols if c in scored.columns]
    print(f"\n-- contests, draft group {args.draft_group} --")
    _show(scored[cols].round(4), args.top)

    if args.out:
        scored.to_csv(args.out, index=False)
        print(f"\nwrote {args.out}")
    return 0


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
            src = p.add_mutually_exclusive_group(required=True)
            src.add_argument("--slate", help="DraftKings salary CSV")
            src.add_argument(
                "--draft-group",
                type=int,
                dest="draft_group",
                help="DraftKings draft group id (see `mlbdfs contests`); "
                     "pulls salaries, handedness and probables from the API",
            )
            p.add_argument("--date", help="slate date, YYYY-MM-DD (default today)")
            p.add_argument("--totals", help="CSV of team,total Vegas implied runs")
            p.add_argument(
                "--no-projected-lineups",
                action="store_true",
                dest="no_projected_lineups",
                help="do not fill unposted lineups from recent starts",
            )
        p.add_argument("--sims", type=int, default=10_000)
        p.add_argument("--seed", type=int, default=1)
        p.add_argument("--out", help="write results to this CSV")

    def add_contest(p):
        p.add_argument("--contest", choices=sorted(CONTESTS), default="large_gpp")
        p.add_argument(
            "--contest-id", type=int, dest="contest_id",
            help="a real DraftKings contest id; its published payout curve "
                 "replaces --contest/--entries/--fee",
        )
        p.add_argument("--sport", default="MLB", help=argparse.SUPPRESS)
        p.add_argument(
            "--compare-contests", type=int, nargs="?", const=8, default=0,
            dest="compare_contests", metavar="N",
            help="after building the portfolio, price the same lineups into "
                 "the N largest contests on the slate (needs --draft-group)",
        )
        p.add_argument("--min-pool", type=float, default=700.0,
                       help=argparse.SUPPRESS)
        p.add_argument("--min-fee", type=float, default=0.0,
                       help=argparse.SUPPRESS)
        p.add_argument("--max-fee", type=float, default=1e9,
                       help=argparse.SUPPRESS)
        p.add_argument(
            "--deterministic", action="store_true",
            help="reproducible candidate pool from the seed, at roughly 3.5x "
                 "the solve time (CP-SAT's parallel search is not)",
        )
        p.add_argument(
            "--field-mean", type=float, dest="field_mean",
            help="average score of a field lineup in contests you enter "
                 "(`mlbdfs backtest` prints it); calibrates field strength, "
                 "without which ROI is inflated",
        )
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
    p.add_argument(
        "--allow-unconfirmed",
        action="store_true",
        help="run before batting orders post; projections are much weaker",
    )
    p.set_defaults(func=cmd_optimize)

    p = sub.add_parser(
        "backtest", help="analyse a finished contest from its standings export"
    )
    p.add_argument("--standings", required=True)
    p.add_argument("--username", help="your DraftKings name, to isolate your entries")
    p.add_argument("--projected", help="CSV from `project`, to score ownership accuracy")
    p.set_defaults(func=cmd_backtest)

    p = sub.add_parser(
        "contests", help="list DraftKings slates and screen their contests"
    )
    p.add_argument("--sport", default="MLB")
    p.add_argument("--date", help="slate date, YYYY-MM-DD")
    p.add_argument("--draft-group", type=int, dest="draft_group")
    p.add_argument("--classic-only", action="store_true", default=True)
    p.add_argument(
        "--all-game-types", action="store_false", dest="classic_only",
        help="include Showdown, Tiers and Snake slates",
    )
    p.add_argument("--min-pool", type=float, default=700.0)
    p.add_argument("--min-fee", type=float, default=0.0)
    p.add_argument("--max-fee", type=float, default=1e9)
    p.add_argument(
        "--max-entries-per-user", type=int, default=0,
        help="keep only contests capped at this many entries per user",
    )
    p.add_argument("--top", type=int, default=30)
    p.add_argument("--out", help="write the screen to this CSV")
    p.set_defaults(func=cmd_contests)

    p = sub.add_parser("diagnose", help="check the simulator against league aggregates")
    p.add_argument("--games", type=int, default=8)
    p.add_argument("--sims", type=int, default=8_000)
    p.set_defaults(func=cmd_diagnose)

    p = sub.add_parser(
        "lineup-accuracy",
        help="walk-forward test of the projected lineup model",
    )
    p.add_argument("--season", type=int, default=date.today().year)
    p.add_argument("--history", help="parquet of lineup history, instead of fetching")
    p.add_argument("--max-dates", type=int, default=None, dest="max_dates")
    p.add_argument("--out", help="write per-game results to this CSV")
    p.set_defaults(func=cmd_lineup_accuracy)

    p = sub.add_parser(
        "fit-ownership",
        help="fit ownership weights to realized contest ownership",
    )
    p.add_argument("--features", required=True,
                   help="parquet of slate features with a realized ownership column")
    p.add_argument("--entries", type=int, default=10_000, help="contest entry count")
    p.add_argument("--l2", type=float, default=0.01, help="ridge penalty; keep light")
    p.add_argument("--out", help="write coefficients to this CSV")
    p.set_defaults(func=cmd_fit_ownership)

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
