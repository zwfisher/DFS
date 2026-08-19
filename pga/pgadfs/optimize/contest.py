"""Turning a simulated score into money.

A lineup's projection is not what it is worth. What it is worth is where it
lands on the payout curve, and in a 35,000-entry tournament that pays
$200,000 to first and $40 to 7,200th, the curve is almost all of the story:
the difference between a good lineup and a great one is entirely in how
often it reaches the few hundred places that pay real money.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np

from ..data.dk import Contest


@dataclass(frozen=True)
class Payouts:
    """A contest's prize schedule as a dense, 1-indexed vector."""

    prizes: np.ndarray      # prizes[i] is the prize for finishing i+1
    entries: int
    entry_fee: float

    @classmethod
    def from_contest(cls, contest: Contest) -> "Payouts":
        return cls(
            prizes=np.asarray(contest.payout_vector(), dtype=np.float64),
            entries=contest.max_entries,
            entry_fee=contest.entry_fee,
        )

    def smoothed(self, width: float = 2.0, pad: int = 10) -> "Payouts":
        """The same curve, spread over neighbouring ranks on a log scale.

        Selecting lineups on the raw curve does not work, and the reason is
        worth stating plainly: first place here is $200,000, a lineup reaches
        it about three times in a hundred thousand simulations, and pricing
        that from a few thousand draws is a lottery, not an estimate. One
        lucky win adds tens of dollars per simulation to a candidate's mean,
        which is far more than any real difference between candidates -- so
        picking the highest-EV lineups mostly picks the luckiest ones, and
        their measured edge evaporates on fresh draws.

        Smoothing replaces the prize at rank r with a geometric mean of the
        prizes over a multiplicative window around r. The economics survive
        -- the curve is still steep, still top-heavy, still worth chasing --
        but reaching 4th instead of 1st no longer changes a candidate's
        score by two orders of magnitude, and the estimator stops being
        dominated by which candidate happened to win a simulation.

        The window is multiplicative with an additive floor, because a purely
        multiplicative one is narrowest exactly where the curve is steepest:
        at rank 1 it spans two places and smooths almost nothing. `pad` sets
        how many places around the very top get averaged together, and it is
        the parameter that decides how much resolution the selector is
        claiming to have. Ten is roughly honest at a few thousand
        simulations -- enough to tell "reaches the top ten" from "reaches the
        top hundred", not enough to tell first from fourth.

        The total prize pool is preserved exactly, so expected values stay on
        the same scale as the real contest.
        """
        n = len(self.prizes)
        ranks = np.arange(1, n + 1, dtype=np.float64)
        log_prize = np.log(np.maximum(self.prizes, 1e-9))
        cumulative = np.concatenate([[0.0], np.cumsum(log_prize)])
        lo = np.clip(np.floor(ranks / width).astype(int) - pad, 1, n) - 1
        hi = np.clip(np.ceil(ranks * width).astype(int) + pad, 1, n)
        out = np.exp((cumulative[hi] - cumulative[lo]) / (hi - lo))
        out *= self.prizes.sum() / out.sum()
        return Payouts(prizes=out, entries=self.entries, entry_fee=self.entry_fee)

    @property
    def paid_places(self) -> int:
        return len(self.prizes)

    def prize(self, rank: np.ndarray) -> np.ndarray:
        """Prize for a 1-indexed finishing rank, 0 outside the money."""
        rank = np.asarray(rank)
        idx = np.clip(rank - 1, 0, self.paid_places)
        padded = np.concatenate([self.prizes, [0.0]])
        return padded[idx]


class ContestEvaluator:
    """Ranks candidate lineups against a simulated field, sim by sim.

    The field's scores are sorted once per simulation; after that, placing a
    candidate is a binary search. Ties are handled by averaging the best and
    worst rank the score could take, which is what splitting a tie across
    equal scores amounts to on a payout curve this steep.
    """

    def __init__(self, field_scores: np.ndarray, payouts: Payouts, *, smoothing: float = 2.0):
        if field_scores.ndim != 2:
            raise ValueError("field_scores must be (n_sims, n_field_lineups)")
        self.payouts = payouts
        # Selection runs on the smoothed curve; reporting uses the real one.
        self.selection_payouts = payouts.smoothed(smoothing) if smoothing > 1.0 else payouts
        self.n_sims, self.n_field = field_scores.shape
        self._sorted = field_scores
        self._sorted.sort(axis=1)     # in place: the field matrix is large
        # One entry of the contest is the lineup being evaluated, so it is
        # ranked against entries - 1 opponents drawn from the same field.
        self._scale = (payouts.entries - 1) / self.n_field

    def ranks(self, scores: np.ndarray) -> np.ndarray:
        """Estimated finishing rank of each candidate, per simulation.

        `scores` is (n_sims, n_candidates).
        """
        if scores.shape[0] != self.n_sims:
            raise ValueError("candidate scores must have one row per simulation")
        worse = np.empty(scores.shape, dtype=np.float64)
        for s in range(self.n_sims):
            row = self._sorted[s]
            lo = np.searchsorted(row, scores[s], side="left")
            hi = np.searchsorted(row, scores[s], side="right")
            worse[s] = self.n_field - 0.5 * (lo + hi)
        return 1.0 + worse * self._scale

    def payout(
        self,
        scores: np.ndarray,
        own_better: np.ndarray | None = None,
        *,
        smoothed: bool = True,
    ) -> np.ndarray:
        """Prize won by each candidate, per simulation.

        `own_better` is the number of the *entrant's own* other lineups that
        beat this one in that simulation. Entering twenty lineups means
        twenty tickets, and when several of them run hot they push each other
        down the curve -- which is the only reason a portfolio's value is not
        just the sum of its parts.
        """
        rank = self.ranks(scores)
        if own_better is not None:
            rank = rank + own_better
        table = self.selection_payouts if smoothed else self.payouts
        return table.prize(np.floor(rank + 0.5).astype(np.int64))

    def roi(self, scores: np.ndarray, *, smoothed: bool = True) -> np.ndarray:
        """Expected return per candidate, as a multiple of the entry fee."""
        return self.payout(scores, smoothed=smoothed).mean(axis=0) / self.payouts.entry_fee - 1.0
