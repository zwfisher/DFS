"""Choosing which lineups to actually enter.

Twenty entries in the same tournament are twenty separate tickets, so their
expected value is very nearly additive -- but not exactly, and the exception
is the whole reason to care about diversity. When two of your lineups run hot
in the same simulation they take first and second rather than first twice, and
on a curve that pays $200,000 and then $100,000 that costs real money. Greedy
selection here is on *marginal* portfolio value, which prices that in: a
candidate is charged for the places it takes off the lineups already chosen.

Exposure and overlap caps sit on top. They are risk limits, not value
judgements: they bound how much of the portfolio one golfer's week can ruin.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np

from ..config import PortfolioConfig
from .contest import ContestEvaluator

_CHUNK = 64


@dataclass
class Portfolio:
    lineups: list[tuple[int, ...]]
    marginal_ev: list[float]      # EV added by each lineup, in dollars, at the time it was picked
    total_ev: float               # expected prize money, on the smoothed curve
    raw_ev: float                 # expected prize money on the contest's real curve
    roi: float                    # per entry, as a multiple of the fee
    solo_roi: np.ndarray          # each entered lineup's ROI on its own

    def exposures(self, n_golfers: int) -> np.ndarray:
        counts = np.bincount(np.ravel(self.lineups), minlength=n_golfers)
        return counts / max(len(self.lineups), 1)


def select_portfolio(
    candidates: list[tuple[int, ...]],
    points: np.ndarray,
    evaluator: ContestEvaluator,
    cfg: PortfolioConfig,
    *,
    n_golfers: int | None = None,
) -> Portfolio:
    n_golfers = n_golfers or points.shape[1]
    cand = np.asarray(candidates)
    scores = points[:, cand].sum(axis=2).astype(np.float32)     # (S, C)
    base_rank = evaluator.ranks(scores)                          # (S, C)
    prize = evaluator.selection_payouts.prize
    raw_prize = evaluator.payouts.prize
    fee = evaluator.payouts.entry_fee

    ranks_int = np.floor(base_rank + 0.5).astype(np.int64)
    solo_ev = prize(ranks_int).mean(axis=0)
    solo_raw_ev = raw_prize(ranks_int).mean(axis=0)

    chosen: list[int] = []
    marginal: list[float] = []
    max_count = int(np.floor(cfg.max_exposure * cfg.n_lineups))
    used = np.zeros(n_golfers, dtype=int)

    # Ranks of the already-chosen lineups, including displacement by each
    # other. Kept incrementally so each step only pays for the new lineup.
    chosen_rank = np.empty((scores.shape[0], 0), dtype=np.float64)
    chosen_scores = np.empty((scores.shape[0], 0), dtype=np.float32)
    chosen_ev = chosen_raw_ev = 0.0

    for _ in range(cfg.n_lineups):
        legal = np.ones(len(cand), dtype=bool)
        for j, lineup in enumerate(cand):
            if j in chosen:
                legal[j] = False
                continue
            if any(used[i] >= max_count for i in lineup):
                legal[j] = False
                continue
            for k in chosen:
                if len(set(lineup) & set(cand[k])) > cfg.max_overlap:
                    legal[j] = False
                    break
        if not legal.any():
            break

        gain = np.full(len(cand), -np.inf)
        idxs = np.flatnonzero(legal)
        for start in range(0, len(idxs), _CHUNK):
            block = idxs[start : start + _CHUNK]
            # What the new lineup wins, given the chosen ones that beat it.
            if chosen_scores.shape[1]:
                own_better = (chosen_scores[:, None, :] > scores[:, block, None]).sum(axis=2)
            else:
                own_better = np.zeros((scores.shape[0], len(block)), dtype=np.int64)
            new_ev = prize(
                np.floor(base_rank[:, block] + own_better + 0.5).astype(np.int64)
            ).mean(axis=0)

            # What it costs the lineups already chosen, by outscoring them.
            if chosen_scores.shape[1]:
                beats = (scores[:, block, None] > chosen_scores[:, None, :]).astype(np.int64)
                bumped = prize(
                    np.floor(chosen_rank[:, None, :] + beats + 0.5).astype(np.int64)
                )
                current = prize(np.floor(chosen_rank + 0.5).astype(np.int64))
                cost = (bumped.mean(axis=0) - current.mean(axis=0)).sum(axis=1)
            else:
                cost = 0.0
            gain[block] = new_ev + cost

        pick = int(np.argmax(gain))
        marginal.append(float(gain[pick]))
        chosen.append(pick)
        for i in cand[pick]:
            used[i] += 1

        new_scores = scores[:, pick : pick + 1]
        if chosen_scores.shape[1]:
            chosen_rank = chosen_rank + (new_scores > chosen_scores)
        own_better = (chosen_scores > new_scores).sum(axis=1, keepdims=True)
        chosen_rank = np.concatenate([chosen_rank, base_rank[:, pick : pick + 1] + own_better], axis=1)
        chosen_scores = np.concatenate([chosen_scores, new_scores], axis=1)
        chosen_ranks = np.floor(chosen_rank + 0.5).astype(np.int64)
        chosen_ev = float(prize(chosen_ranks).sum(axis=1).mean())
        chosen_raw_ev = float(raw_prize(chosen_ranks).sum(axis=1).mean())

    lineups = [tuple(int(i) for i in cand[j]) for j in chosen]
    n = max(len(lineups), 1)
    return Portfolio(
        lineups=lineups,
        marginal_ev=marginal,
        total_ev=chosen_ev,
        raw_ev=chosen_raw_ev,
        roi=chosen_ev / (n * fee) - 1.0,
        solo_roi=solo_ev[chosen] / fee - 1.0,
    )
