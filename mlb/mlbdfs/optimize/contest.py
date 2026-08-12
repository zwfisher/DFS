"""Contest payout structures.

A lineup's value depends entirely on what it is entered into. The same
roster that is excellent in a 20-entry double-up is a poor choice in a
milly-maker, because one pays for consistency and the other pays almost
exclusively for the top thousandth of outcomes. Optimizing without the
payout curve in hand is optimizing a proxy.

Curves are expressed as a list of ``(rank_from, rank_to, prize)`` bands,
which is how DraftKings publishes them, and compiled into a lookup array so
that scoring millions of lineup-simulation pairs is a single fancy index.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np


@dataclass
class Contest:
    """A contest's entry fee, size and payout structure."""

    name: str
    entry_fee: float
    n_entries: int
    payouts: list[tuple[int, int, float]]  # (rank_from, rank_to, prize each)
    max_entries_per_user: int = 1

    def payout_table(self) -> np.ndarray:
        """Prize by finishing rank, as a 1-indexed lookup array.

        Index 0 is unused so that ``table[rank]`` reads naturally; ranks
        beyond the paid places return zero.
        """
        table = np.zeros(self.n_entries + 2, dtype=np.float64)
        for lo, hi, prize in self.payouts:
            hi = min(hi, self.n_entries)
            if lo > hi:
                continue
            table[lo : hi + 1] = prize
        return table

    @property
    def total_prizes(self) -> float:
        return sum((min(hi, self.n_entries) - lo + 1) * prize
                   for lo, hi, prize in self.payouts)

    @property
    def rake(self) -> float:
        gross = self.entry_fee * self.n_entries
        return 1.0 - self.total_prizes / gross if gross else 0.0


def large_gpp(
    n_entries: int = 100_000, entry_fee: float = 5.0, name: str = "large_gpp"
) -> Contest:
    """A top-heavy tournament in the shape DraftKings actually pays.

    Roughly 20% of the field cashes, and the winner takes a few percent of
    the prize pool. This shape is why tournament lineups look nothing like
    cash-game lineups: the median outcome is worth nothing, so variance that
    would be reckless in a double-up is the entire point here.
    """
    pool = entry_fee * n_entries * 0.85
    # Shares below are relative weights, not fractions of the pool -- they
    # are normalized to the pool at the end. Writing them as fractions
    # directly is how you end up paying out twice what was collected.
    bands = [
        (1, 1, 0.030),
        (2, 2, 0.018),
        (3, 3, 0.012),
        (4, 5, 0.008),
        (6, 10, 0.005),
        (11, 25, 0.0028),
        (26, 50, 0.0016),
        (51, 100, 0.0010),
        (101, 250, 0.00058),
        (251, 500, 0.00036),
        (501, 1000, 0.00024),
        (1001, 2500, 0.000155),
        (2501, 5000, 0.000108),
        (5001, 10000, 0.000078),
        (10001, 20000, 0.0000545),
    ]
    return Contest(
        name=name,
        entry_fee=entry_fee,
        n_entries=n_entries,
        payouts=_scale_bands(bands, n_entries, pool, reference_field=100_000),
    )


def _scale_bands(
    bands: list[tuple[int, int, float]],
    n_entries: int,
    pool: float,
    reference_field: int,
) -> list[tuple[int, int, float]]:
    """Stretch a payout shape to a contest size and normalize to the pool.

    Bands are written against a reference field size and rescaled
    proportionally, keeping the top few places at their absolute ranks --
    every tournament pays a first place, regardless of size. Prizes are then
    normalized so the payouts sum to exactly the prize pool, which makes the
    rake come out right whatever the shape.
    """
    scale = n_entries / reference_field
    scaled: list[tuple[int, int, float]] = []
    for lo, hi, weight in bands:
        new_lo = lo if lo <= 5 else max(int(round((lo - 1) * scale)) + 1, 1)
        new_hi = hi if hi <= 5 else int(round(hi * scale))
        new_hi = min(new_hi, n_entries)
        if new_lo > new_hi:
            continue
        if scaled and new_lo <= scaled[-1][1]:
            new_lo = scaled[-1][1] + 1
        if new_lo > new_hi:
            continue
        scaled.append((new_lo, new_hi, weight))

    total = sum((hi - lo + 1) * w for lo, hi, w in scaled)
    if total <= 0:
        return scaled
    return [(lo, hi, w * pool / total) for lo, hi, w in scaled]


def double_up(
    n_entries: int = 10_000, entry_fee: float = 5.0, name: str = "double_up"
) -> Contest:
    """A flat contest paying roughly the top 44% at just under double."""
    paid = int(n_entries * 0.44)
    prize = entry_fee * n_entries * 0.86 / paid
    return Contest(
        name=name,
        entry_fee=entry_fee,
        n_entries=n_entries,
        payouts=[(1, paid, prize)],
    )


def single_entry_gpp(
    n_entries: int = 5_000, entry_fee: float = 10.0, name: str = "single_entry"
) -> Contest:
    """A smaller single-entry tournament: top heavy, but far less extreme."""
    pool = entry_fee * n_entries * 0.86
    bands = [
        (1, 1, 0.115),
        (2, 2, 0.070),
        (3, 3, 0.048),
        (4, 5, 0.032),
        (6, 10, 0.019),
        (11, 20, 0.0105),
        (21, 50, 0.0056),
        (51, 100, 0.0032),
        (101, 250, 0.0018),
        (251, 600, 0.0011),
    ]
    return Contest(
        name=name,
        entry_fee=entry_fee,
        n_entries=n_entries,
        payouts=_scale_bands(bands, n_entries, pool, reference_field=5_000),
    )


CONTESTS = {
    "large_gpp": large_gpp,
    "double_up": double_up,
    "single_entry": single_entry_gpp,
}
