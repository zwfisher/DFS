"""Expected ROI against the simulated field, and portfolio selection.

This is where the three layers meet. Each candidate lineup is scored across
every simulation, ranked against a field of opponent lineups scored in the
*same* simulation, and paid out through the contest's actual payout curve.
The result is expected ROI, which is the number worth maximizing -- unlike
projected points, which ignores both what the field owns and what the
tournament pays.

Ranking a few hundred candidates against tens of thousands of opponents
across tens of thousands of simulations is the one genuinely large
computation in the project. It is done by sorting the field once per
simulation and locating candidates with a searchsorted, which turns an
apparent quadratic into an n log n, and in chunks so the intermediate never
has to fit in memory all at once.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np
import pandas as pd

from ..config import OPTIMIZER, OptimizerConfig
from ..ownership.field import Field
from .contest import Contest
from .milp import Lineup


def _rank_against_field(
    cand_block: np.ndarray, field_block: np.ndarray
) -> np.ndarray:
    """Count field lineups beating each candidate, per simulation.

    ``cand_block`` is ``(chunk, n_cand)`` and ``field_block`` is
    ``(chunk, n_field)``; the result is ``(chunk, n_cand)``.

    The rows are offset into disjoint value bands and flattened so that a
    single global ``searchsorted`` resolves every simulation at once --
    numpy has no row-wise searchsorted, and looping over simulations costs
    more than the sort itself.
    """
    chunk, n_field = field_block.shape
    lo = min(field_block.min(), cand_block.min())
    hi = max(field_block.max(), cand_block.max())
    band = float(hi - lo) + 1.0

    offsets = (np.arange(chunk, dtype=np.float64) * band)[:, None]
    sorted_field = np.sort(field_block, axis=1).astype(np.float64) - lo + offsets
    flat_sorted = sorted_field.ravel()

    shifted_cand = cand_block.astype(np.float64) - lo + offsets
    pos = np.searchsorted(flat_sorted, shifted_cand.ravel(), side="right")
    # Convert the global position back to a position within its own row.
    row_start = (np.arange(chunk) * n_field)[:, None]
    within_row = pos.reshape(cand_block.shape) - row_start
    return n_field - within_row


@dataclass
class FieldStanding:
    """Where each candidate finished in the field, per simulation.

    This is the expensive half of an evaluation and it does not depend on
    the contest at all -- only the payout table and the field-size rescale
    do. Computing it once lets the same lineups be priced into a dozen
    contests for the cost of one, which is what
    :func:`mlbdfs.optimize.screen.compare_contests` needs.
    """

    beaten: np.ndarray     # (n_sims, n_cand) field lineups ahead of each
    scores: np.ndarray     # (n_sims, n_cand) candidate totals
    field_size: int


def field_standing(
    lineups: list[Lineup],
    scores: np.ndarray,
    score_player_ids: list[str],
    field: Field,
    chunk_size: int = 2000,
) -> FieldStanding:
    """Rank every candidate against the field in every simulation."""
    lookup = {pid: i for i, pid in enumerate(score_player_ids)}
    cand_idx = np.array(
        [[lookup[pid] for pid in lu.player_ids] for lu in lineups], dtype=np.int32
    )

    n_sims = scores.shape[0]
    beaten = np.empty((n_sims, len(lineups)), dtype=np.int32)
    totals = np.empty((n_sims, len(lineups)), dtype=np.float64)

    for start in range(0, n_sims, chunk_size):
        sl = slice(start, min(start + chunk_size, n_sims))
        cand_block = scores[sl][:, cand_idx].sum(axis=2)
        field_block = field.score(scores, score_player_ids, sl)
        beaten[sl] = _rank_against_field(cand_block, field_block)
        totals[sl] = cand_block

    return FieldStanding(beaten=beaten, scores=totals, field_size=field.size)


def contest_rank(
    standing: FieldStanding, contest: Contest, seed: int = 0
) -> np.ndarray:
    """Finishing rank in the real contest, from a smaller sampled field.

    The obvious approach -- multiply the count of field lineups beating you
    by ``n_entries / field_size`` and round -- is wrong in a way that
    silently inflates every ROI in the project. With a 12,000-lineup field
    standing in for a 35,671-entry contest the multiplier is 2.97, so zero
    beaten maps to rank 1 and one beaten maps to rank 4: **ranks 2 and 3
    cannot be produced at all.** Every lineup good enough to beat the whole
    sample collects first prize, including the ones that would really have
    come second. Measured on one slate that doubled P(win) and inflated ROI
    by 32%.

    The sampled field is a *sample*, so the honest object is a distribution
    over true rank, not a point. If ``k`` of ``n`` sampled lineups beat a
    candidate, the share of the real field that beats it has a Beta
    posterior, and the count among ``N`` entries is Binomial given that
    share -- a Beta-Binomial draw. This gets the small-``k`` behaviour
    right, which is the only region that matters: at ``k = 0`` the posterior
    still carries real mass, so the candidate lands on rank 1, 2, 3 or worse
    with sensible probabilities instead of always winning.

    Jeffreys' ``Beta(k + 1/2, n - k + 1/2)`` is the prior, chosen because it
    is the one that does not collapse at ``k = 0``. When the field is at
    least as large as the contest there is nothing to extrapolate and the
    exact scaled rank is used.
    """
    beaten = standing.beaten
    n_field = standing.field_size
    n_entries = contest.n_entries

    if n_field >= n_entries:
        scale = n_entries / n_field
        return np.clip(
            np.rint(1.0 + beaten * scale).astype(np.int64), 1, n_entries
        )

    rng = np.random.default_rng(seed)
    share = rng.beta(beaten + 0.5, n_field - beaten + 0.5)
    ahead = rng.binomial(n_entries, np.clip(share, 0.0, 1.0))
    return np.clip(ahead + 1, 1, n_entries).astype(np.int64)


def evaluate_lineups(
    lineups: list[Lineup],
    scores: np.ndarray,
    score_player_ids: list[str],
    field: Field,
    contest: Contest,
    chunk_size: int = 2000,
    standing: FieldStanding | None = None,
    seed: int = 0,
) -> pd.DataFrame:
    """Expected ROI and finishing distribution for each candidate lineup."""
    if standing is None:
        standing = field_standing(
            lineups, scores, score_player_ids, field, chunk_size
        )

    n_sims = standing.scores.shape[0]
    n_cand = len(lineups)
    payout = contest.payout_table()
    rank = contest_rank(standing, contest, seed=seed)
    total_prize = payout[rank].sum(axis=0)
    total_score = standing.scores.sum(axis=0)
    wins = (rank == 1).sum(axis=0)
    cashes = (rank <= _last_paid_rank(contest)).sum(axis=0)

    mean_prize = total_prize / n_sims
    return pd.DataFrame(
        {
            "lineup": np.arange(1, n_cand + 1),
            "mean_score": total_score / n_sims,
            "expected_payout": mean_prize,
            "roi": mean_prize / contest.entry_fee - 1.0,
            "p_win": wins / n_sims,
            "p_cash": cashes / n_sims,
            "salary": [lu.salary for lu in lineups],
            "stack": [
                " / ".join(f"{t}{c}" for t, c in lu.stack.items() if c >= 2)
                for lu in lineups
            ],
        }
    )


def _last_paid_rank(contest: Contest) -> int:
    return max((min(hi, contest.n_entries) for _, hi, _ in contest.payouts), default=0)


def select_portfolio(
    lineups: list[Lineup],
    evaluation: pd.DataFrame,
    n_lineups: int,
    cfg: OptimizerConfig = OPTIMIZER,
) -> pd.DataFrame:
    """Choose which candidates to actually enter.

    Greedy by expected ROI, subject to two portfolio-level limits that a
    per-lineup objective cannot express: no player appears in more than
    ``max_exposure`` of entries, and no two entries share more than
    ``max_overlap`` players. Both exist because a set of entries that all
    win together also all lose together, and a multi-entry bankroll cares
    about the distribution of the portfolio, not of one lineup.
    """
    ranked = evaluation.sort_values("roi", ascending=False)
    max_count = int(np.floor(cfg.max_exposure * n_lineups))

    chosen: list[int] = []
    chosen_sets: list[frozenset[str]] = []
    exposure: dict[str, int] = {}

    for row in ranked.itertuples():
        if len(chosen) >= n_lineups:
            break
        i = int(row.lineup) - 1
        players = lineups[i].player_ids
        pset = frozenset(players)

        if any(exposure.get(p, 0) >= max_count for p in players):
            continue
        if any(len(pset & prev) > cfg.max_overlap for prev in chosen_sets):
            continue

        chosen.append(i)
        chosen_sets.append(pset)
        for p in players:
            exposure[p] = exposure.get(p, 0) + 1

    return evaluation.iloc[chosen].reset_index(drop=True)


def exposure_report(
    lineups: list[Lineup], selected: pd.DataFrame, slate
) -> pd.DataFrame:
    """Player exposure across the chosen portfolio."""
    idx = [int(n) - 1 for n in selected["lineup"]]
    counts: dict[str, int] = {}
    for i in idx:
        for pid in lineups[i].player_ids:
            counts[pid] = counts.get(pid, 0) + 1

    total = max(len(idx), 1)
    rows = [
        {
            "player_id": pid,
            "name": slate.player(pid).name,
            "team": slate.player(pid).team,
            "salary": slate.player(pid).salary,
            "lineups": c,
            "exposure": c / total,
        }
        for pid, c in counts.items()
    ]
    return (
        pd.DataFrame(rows)
        .sort_values("exposure", ascending=False)
        .reset_index(drop=True)
    )


def compare_objectives(
    evaluation: pd.DataFrame, top_n: int = 20
) -> pd.DataFrame:
    """What picking by projected points would have cost you.

    Sorting candidates by mean score and by ROI gives different answers, and
    the gap between them is the whole argument for simulating the field. If
    the two orderings ever agree completely, either the ownership model has
    gone flat or the contest is not top-heavy enough to care.
    """
    by_score = evaluation.nlargest(top_n, "mean_score")
    by_roi = evaluation.nlargest(top_n, "roi")
    overlap = len(set(by_score["lineup"]) & set(by_roi["lineup"]))
    return pd.DataFrame(
        [
            {
                "selection": "top by projected points",
                "mean_roi": by_score["roi"].mean(),
                "mean_score": by_score["mean_score"].mean(),
                "p_win": by_score["p_win"].mean(),
            },
            {
                "selection": "top by expected ROI",
                "mean_roi": by_roi["roi"].mean(),
                "mean_score": by_roi["mean_score"].mean(),
                "p_win": by_roi["p_win"].mean(),
            },
            {
                "selection": f"lineups in common ({overlap}/{top_n})",
                "mean_roi": np.nan,
                "mean_score": np.nan,
                "p_win": np.nan,
            },
        ]
    )
