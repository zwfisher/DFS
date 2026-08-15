"""Empirical-Bayes per-plate-appearance talent rates.

Raw seasonal rates overfit badly at the sample sizes a single season gives
you -- a hitter with 40 PA and 4 home runs is not a 10% home run hitter. Each
outcome is shrunk toward the league mean with a Beta-Binomial prior whose
strength is that stat's stabilization point, so fast-stabilizing rates
(strikeouts) move off the prior quickly and slow ones (triples) barely move
without a lot of evidence.

The canonical input is a "counts" frame with one row per player-season:

    player_id, season, pa, strikeout, field_out, walk, hit_by_pitch,
    single, double, triple, home_run

Pitcher frames are identical, with the counts being what the pitcher allowed
and ``pa`` meaning batters faced.
"""

from __future__ import annotations

import numpy as np
import pandas as pd

from ..config import (
    LEAGUE_RATES,
    N_OUTCOMES,
    OUTCOMES,
    PITCHER_STABILIZATION_MULTIPLIER,
    SEASON_DECAY,
    STABILIZATION_PA,
)

COUNT_COLUMNS = list(OUTCOMES)
REQUIRED_COLUMNS = ["player_id", "pa", *COUNT_COLUMNS]


def league_rate_vector() -> np.ndarray:
    """League-average rates in canonical outcome order, normalized."""
    v = np.array([LEAGUE_RATES[name] for name in OUTCOMES], dtype=np.float64)
    return v / v.sum()


def prior_strength(is_pitcher: bool = False) -> np.ndarray:
    """Prior weight in plate appearances, per outcome."""
    k = np.array([STABILIZATION_PA[name] for name in OUTCOMES], dtype=np.float64)
    if is_pitcher:
        k = k * PITCHER_STABILIZATION_MULTIPLIER
    return k


def pool_seasons(counts: pd.DataFrame, decay: float = SEASON_DECAY) -> pd.DataFrame:
    """Collapse player-seasons into one weighted row per player.

    Recent seasons count fully and older ones decay geometrically, so a
    player's current talent estimate leans on what he has done lately without
    throwing away the stabilizing weight of his history.
    """
    _validate(counts)
    if "season" not in counts.columns:
        return counts.groupby("player_id", as_index=False)[["pa", *COUNT_COLUMNS]].sum()

    latest = counts["season"].max()
    weights = decay ** (latest - counts["season"].astype(int))
    weighted = counts[["pa", *COUNT_COLUMNS]].mul(weights, axis=0)
    weighted["player_id"] = counts["player_id"].to_numpy()
    return weighted.groupby("player_id", as_index=False).sum()


def shrink(counts: pd.DataFrame, is_pitcher: bool = False) -> pd.DataFrame:
    """Shrink observed counts toward league mean; return per-PA rates.

    Returns one row per player with columns in canonical outcome order, each
    row summing to exactly 1.
    """
    pooled = pool_seasons(counts)

    n = pooled["pa"].to_numpy(dtype=np.float64)[:, None]
    x = pooled[COUNT_COLUMNS].to_numpy(dtype=np.float64)

    k = prior_strength(is_pitcher)[None, :]
    lg = league_rate_vector()[None, :]

    # Beta-Binomial posterior mean, per outcome independently.
    rates = (x + k * lg) / (n + k)

    # Independent shrinkage does not preserve the simplex, so renormalize.
    rates = rates / rates.sum(axis=1, keepdims=True)

    out = pd.DataFrame(rates, columns=COUNT_COLUMNS)
    out.insert(0, "player_id", pooled["player_id"].to_numpy())
    out["pa_sample"] = pooled["pa"].to_numpy()
    return out


def blend_platoon(
    overall: pd.DataFrame,
    splits: pd.DataFrame | None,
    hand: str,
    split_prior_pa: float = 300.0,
) -> pd.DataFrame:
    """Blend a player's overall rates with his split against one hand.

    Platoon samples are small and platoon skill is largely a league-level
    effect rather than an individual one, so the split is shrunk toward the
    player's own overall rates rather than toward league average. With no
    split data the overall rates pass through unchanged.

    ``splits`` is a counts frame carrying an extra ``vs_hand`` column, as
    produced by ``data.sources.batter_counts_by_hand``.
    """
    if splits is None or splits.empty or "vs_hand" not in splits.columns:
        return overall.copy()

    wanted = str(hand).upper()[:1]
    sub = splits[splits["vs_hand"].astype(str).str.upper().str[0] == wanted]
    if sub.empty:
        return overall.copy()

    # Rename explicitly rather than leaning on merge suffixes: `overall`
    # carries `pa_sample` while the pooled splits carry `pa`, so the names
    # do not collide and no suffix is applied to them.
    pooled = pool_seasons(sub.drop(columns=["vs_hand"]))
    pooled = pooled.rename(
        columns={"pa": "_split_pa", **{c: f"_split_{c}" for c in COUNT_COLUMNS}}
    )
    merged = overall.merge(pooled, on="player_id", how="left")

    n = merged["_split_pa"].fillna(0.0).to_numpy(dtype=np.float64)[:, None]
    w = n / (n + split_prior_pa)

    base = merged[COUNT_COLUMNS].to_numpy(dtype=np.float64)
    split_counts = (
        merged[[f"_split_{c}" for c in COUNT_COLUMNS]]
        .fillna(0.0)
        .to_numpy(dtype=np.float64)
    )
    with np.errstate(invalid="ignore", divide="ignore"):
        split_rates = np.where(n > 0, split_counts / np.maximum(n, 1e-9), base)

    blended = (1.0 - w) * base + w * split_rates
    blended = blended / blended.sum(axis=1, keepdims=True)

    out = merged[["player_id"]].copy()
    out[COUNT_COLUMNS] = blended
    out["pa_sample"] = merged["pa_sample"].to_numpy()
    return out


def steal_rates(
    counts: pd.DataFrame, prior_attempts: float = 40.0, prior_rate: float = 0.055
) -> pd.Series:
    """Stolen base attempt rate per time reached first base.

    Expects ``sb`` and ``cs`` columns alongside the outcome counts. Times on
    first are approximated by singles plus walks plus hit by pitches, which
    is what the simulator actually offers a steal opportunity from.
    """
    pooled = pool_seasons(counts)
    if "sb" not in pooled.columns:
        return pd.Series(prior_rate, index=pooled["player_id"], name="sb_attempt_rate")

    on_first = (
        pooled["single"] + pooled["walk"] + pooled["hit_by_pitch"]
    ).to_numpy(dtype=np.float64)
    attempts = (pooled["sb"] + pooled.get("cs", 0)).to_numpy(dtype=np.float64)

    rate = (attempts + prior_attempts * prior_rate) / (on_first + prior_attempts)
    return pd.Series(rate, index=pooled["player_id"], name="sb_attempt_rate")


def as_matrix(rates: pd.DataFrame, player_ids: list) -> np.ndarray:
    """Order a rates frame into an ``(n_players, n_outcomes)`` array.

    Players with no row fall back to league average rather than raising, so a
    slate is never blocked by one missing player.
    """
    indexed = rates.set_index("player_id")
    lg = league_rate_vector()
    out = np.empty((len(player_ids), N_OUTCOMES), dtype=np.float64)
    for i, pid in enumerate(player_ids):
        if pid in indexed.index:
            out[i] = indexed.loc[pid, COUNT_COLUMNS].to_numpy(dtype=np.float64)
        else:
            out[i] = lg
    return out / out.sum(axis=1, keepdims=True)


def _validate(counts: pd.DataFrame) -> None:
    missing = [c for c in REQUIRED_COLUMNS if c not in counts.columns]
    if missing:
        raise ValueError(f"counts frame is missing columns: {missing}")
