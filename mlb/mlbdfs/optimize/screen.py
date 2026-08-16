"""Contest selection: which tournament to put a lineup portfolio into.

Lineup quality and contest quality are separate decisions, and the second
one is decided before the slate even starts. Three things move it, in
descending order of how reliably they can be measured:

**Rake.** Every contest hands back some fraction of the entry fees. A
player who is exactly average captures their fair share ``1/N`` of the
prize pool and loses the rake. To break even you must capture
``1 / (1 - rake)`` times your fair share -- 1.19x at DraftKings' 15.9%
small-stakes rake, 1.11x at the 10% charged on the big buy-ins. That gap is
pure, riskless, and available before you have looked at a single player.

**Overlay.** A guaranteed prize pool is paid whether or not the contest
fills. If it locks under-filled, the shortfall is a negative rake, and it
is the only genuinely free money in daily fantasy. It is also the reason
fill percentage is worth watching in the last hour.

**How the payout curve converts skill into money.** A flat double-up pays
for beating a threshold; a top-heavy tournament pays almost entirely for
the top thousandth of outcomes. The same edge is worth very different
amounts in each, which :func:`edge_return` measures directly rather than
by eye.

What this module deliberately does *not* claim to measure is field
strength. Max-entries-per-user is the observable proxy -- in a 150-max
contest a handful of professionals field thousands of lineups and cover
most of the sensible roster space, while in a single-entry contest they get
one bullet each -- but the size of that effect is not in the lobby data,
and pretending to a number for it would be worse than naming it.
"""

from __future__ import annotations

import numpy as np
import pandas as pd

from .contest import Contest


def payout_shape(contest: Contest) -> dict[str, float]:
    """Descriptive statistics of a payout curve.

    ``top_share`` and ``top1pct_share`` are fractions of the prize pool;
    ``pay_rate`` is the fraction of the field that cashes;
    ``min_cash_multiple`` is the smallest prize as a multiple of the entry
    fee, which is what separates a double-up (about 2x) from a tournament
    (often under 2x, and sometimes 1.5x).
    """
    table = contest.payout_table()[1 : contest.n_entries + 1]
    pool = float(table.sum())
    paid = table[table > 0]
    top1pct = max(1, int(round(contest.n_entries * 0.01)))
    return {
        "pool": pool,
        "pay_rate": len(paid) / contest.n_entries if contest.n_entries else 0.0,
        "top_share": float(table[0]) / pool if pool else 0.0,
        "top1pct_share": float(table[:top1pct].sum()) / pool if pool else 0.0,
        "min_cash_multiple": (
            float(paid.min()) / contest.entry_fee
            if len(paid) and contest.entry_fee
            else 0.0
        ),
        "payout_cv": float(table.std() / table.mean()) if table.mean() else 0.0,
    }


def edge_return(contest: Contest, rank_factor: float = 0.9) -> float:
    """ROI for an entrant who finishes ``rank_factor`` of the way up the field.

    The model is deliberately crude and deliberately shape-only: take an
    average entrant, whose finishing rank is uniform over the field, and
    multiply every rank by ``rank_factor``. At 0.9 that is a player who
    finishes 10% higher than chance would put them -- 900th instead of
    1000th, 9th instead of 10th -- which is a modest, realistic edge.

    The point is not the absolute number. It is that the *same* 10% edge
    pays very differently depending on the curve, and this makes two
    contests comparable on that axis. A flat contest converts the edge into
    a small, reliable return; a top-heavy one into a larger, wildly noisier
    one. Both facts show up here, the second as :func:`edge_sharpe`.
    """
    fee = contest.entry_fee
    if fee <= 0 or contest.n_entries <= 0:
        return 0.0
    return float(_shifted_payouts(contest, rank_factor).mean() / fee - 1.0)


def edge_sharpe(contest: Contest, rank_factor: float = 0.9) -> float:
    """Expected profit divided by its standard deviation, per entry.

    The reciprocal squared is roughly how many entries it takes before the
    edge is one standard error clear of zero. In a milly-maker that number
    runs to the tens of thousands, which is the honest reason top-heavy
    tournaments feel like they never pay: the edge is real and the sample
    needed to see it is larger than a season.
    """
    fee = contest.entry_fee
    if fee <= 0 or contest.n_entries <= 0:
        return 0.0
    payouts = _shifted_payouts(contest, rank_factor)
    profit = payouts - fee
    sd = float(profit.std())
    return float(profit.mean() / sd) if sd > 0 else 0.0


def breakeven_rank_factor(contest: Contest, tol: float = 1e-4) -> float:
    """The rank improvement that exactly pays for the rake.

    Read it as: multiply your finishing rank by this and you break even. A
    value of 0.87 means you have to finish 13% higher up the field than
    chance would put you; 0.55 means you need to be nearly twice as good.
    Lower is harder, and it is the single most useful number here because
    it folds rake and payout shape into one comparable quantity.

    The uniform-rescaling model behind it is a shape assumption, and it is
    the assumption most unkind to top-heavy tournaments: a real edge in
    MLB GPPs is not spread evenly over the field, it is concentrated in the
    right tail, because correlated stacks buy tail outcomes specifically.
    So treat this as the bar to clear with a *generic* edge, and let the
    portfolio's simulated ROI against the actual payout curve settle the
    top-heavy contests.
    """
    lo, hi = 1e-3, 1.0
    if edge_return(contest, hi) >= 0:
        return hi
    while hi - lo > tol:
        mid = (lo + hi) / 2
        if edge_return(contest, mid) >= 0:
            lo = mid
        else:
            hi = mid
    return (lo + hi) / 2


def _shifted_payouts(contest: Contest, rank_factor: float) -> np.ndarray:
    """Payout drawn by each field member after scaling their rank."""
    table = contest.payout_table()
    ranks = np.arange(1, contest.n_entries + 1)
    shifted = np.clip(np.ceil(ranks * rank_factor).astype(int), 1, contest.n_entries)
    return table[shifted]


def overlay(row: pd.Series) -> float:
    """Prize pool minus fees collected at the current fill, or zero.

    Only meaningful for guaranteed contests, and only a forecast until
    lock: an under-filled contest an hour out usually fills. Negative
    values are clipped away because a contest that has over-collected is
    simply paying its stated rake, not a negative overlay.
    """
    if not bool(row.get("guaranteed", False)):
        return 0.0
    collected = float(row["entry_fee"]) * float(row["current_entries"] or 0)
    return max(0.0, float(row["prize_pool"]) - collected)


def compare_contests(result, contests: list[Contest]) -> pd.DataFrame:
    """Run one already-built portfolio through several real payout curves.

    :func:`screen` ranks contests on shape alone, which is all you can do
    before the slate exists. This is the version that knows what you are
    actually entering: the same lineups, the same simulated field, scored
    through each contest's published payout table. Where the two disagree,
    believe this one -- a portfolio built out of correlated stacks has a
    tail-heavy score distribution, and shape-only metrics cannot see that.

    Evaluation runs on the holdout simulation, not the one the pool was
    built against, for the usual reason.
    """
    from .portfolio import evaluate_lineups

    # ``selected.lineup`` is 1-indexed, matching evaluate_lineups' output.
    keep = set(int(n) for n in result.selected["lineup"])
    lineups = [lu for i, lu in enumerate(result.pool, start=1) if i in keep]

    rows = []
    for contest in contests:
        scored = evaluate_lineups(
            lineups, result.holdout.scores, result.holdout.player_ids,
            result.field, contest,
        )
        rows.append(
            {
                "contest": contest.name,
                "entry_fee": contest.entry_fee,
                "entries": contest.n_entries,
                "rake": contest.rake,
                "roi": float(scored["roi"].mean()),
                "best_roi": float(scored["roi"].max()),
                "p_win": float(scored["p_win"].mean()),
                "p_cash": float(scored["p_cash"].mean()),
                "cost": contest.entry_fee * len(lineups),
                "profit": float(
                    (scored["roi"] * contest.entry_fee).sum()
                ),
            }
        )
    return pd.DataFrame(rows).sort_values("profit", ascending=False)


def screen(
    contests: pd.DataFrame,
    payouts: dict[int, pd.DataFrame] | None = None,
    rank_factor: float = 0.9,
) -> pd.DataFrame:
    """Score a lobby frame on every contest-selection axis at once.

    ``contests`` is the output of :func:`mlbdfs.data.lobby.lobby_contests`.
    ``payouts``, keyed by contest id, adds the curve-shape columns; without
    it only the rake and overlay columns are produced, which is still the
    larger part of the decision.
    """
    from ..data.lobby import to_contest

    if contests.empty:
        return contests.copy()

    out = contests.copy()
    gross = out["entry_fee"].astype(float) * out["max_entries"].astype(float)
    out["rake"] = np.where(gross > 0, 1.0 - out["prize_pool"] / gross, np.nan)
    # The multiple of an equal share of the pool needed to break even.
    out["breakeven_share"] = 1.0 / (1.0 - out["rake"])
    out["fill"] = out["current_entries"].astype(float) / out["max_entries"].astype(
        float
    )
    out["overlay_now"] = out.apply(overlay, axis=1)

    if payouts:
        cols: list[dict[str, float]] = []
        for row in out.itertuples():
            bands = payouts.get(int(row.contest_id))
            if bands is None or bands.empty:
                cols.append({})
                continue
            contest = to_contest(out.loc[row.Index], bands)
            shape = payout_shape(contest)
            shape["edge_return"] = edge_return(contest, rank_factor)
            shape["edge_sharpe"] = edge_sharpe(contest, rank_factor)
            shape["breakeven_rank"] = breakeven_rank_factor(contest)
            # Recomputed from the published bands rather than the lobby's
            # advertised pool; they disagree occasionally, and the bands are
            # what actually gets paid.
            shape["rake_published"] = contest.rake
            cols.append(shape)
        out = pd.concat([out, pd.DataFrame(cols, index=out.index)], axis=1)

    return out
