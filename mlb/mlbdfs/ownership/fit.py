"""Fit the ownership model to realized contest ownership.

The heuristic in ``heuristic.py`` uses hand-set weights because there was no
data. This module replaces them with maximum-likelihood estimates once a
contest standings export is available, behind the same interface.

The likelihood is the one the model structure implies. Within a position
group holding ``n_slots`` roster spots, every entry fills those spots by
choosing among the eligible players, so the counts across players are
multinomial with probabilities

    p_i = exp(x_i . beta) / sum_j exp(x_j . beta)

and the log-likelihood is ``sum_i count_i * log(p_i)`` where ``count_i`` is
how many entries rostered player i. That is a conditional logit, and fitting
it is what the sum-to-slots structure was chosen for in the first place.

Pitchers and hitters get separate coefficient vectors: the field weighs a
starting pitcher on strikeout upside and a hitter on a stack, and pooling
them would average two different behaviours into one that describes
neither.

**On sample size.** One contest is one slate. The coefficients are precise
in the sense that 35,671 entries pin the ownership shares tightly, but there
is only one draw of slate conditions -- one set of pitchers, one Vegas
board, one weather day. Anything that was true of that slate specifically is
soaked into the coefficients and cannot be separated. Leave-one-position-out
validation, which this module reports, tests whether the coefficients
generalize across position groups; it cannot test whether they generalize
across slates. Log more slates.
"""

from __future__ import annotations

from dataclasses import dataclass, field

import numpy as np
import pandas as pd
from scipy.optimize import minimize

from ..config import ROSTER

# Feature columns the fit consumes, in a fixed order so coefficients stay
# interpretable and comparable to the hand-set weights they replace.
FEATURES = ["value", "proj", "ceiling", "team_total", "salary", "top_of_order"]
HITTER_FEATURES = FEATURES
PITCHER_FEATURES = ["value", "proj", "ceiling", "team_total", "salary"]


@dataclass
class FittedOwnership:
    """Coefficients estimated from realized contest ownership."""

    hitter_coefficients: dict[str, float]
    pitcher_coefficients: dict[str, float]
    n_slates: int = 1
    n_players: int = 0
    log_likelihood: float = 0.0
    diagnostics: pd.DataFrame = field(default_factory=pd.DataFrame)

    def predict(self, features: pd.DataFrame) -> pd.DataFrame:
        """Project ownership for a slate using the fitted coefficients."""
        slots = dict(ROSTER.slots)
        out = []
        for position, group in features.groupby("position", sort=False):
            g = group.copy()
            is_pitcher = bool(g["is_pitcher"].iloc[0])
            names = PITCHER_FEATURES if is_pitcher else HITTER_FEATURES
            beta = self.pitcher_coefficients if is_pitcher else self.hitter_coefficients

            x = _design(g, names)
            utility = x @ np.array([beta[n] for n in names])
            g["utility"] = utility
            g["ownership"] = _shares(utility, float(slots.get(position, 1)))
            out.append(g)
        return pd.concat(out, ignore_index=True).sort_values(
            "ownership", ascending=False
        ).reset_index(drop=True)

    def as_frame(self) -> pd.DataFrame:
        rows = [
            {"group": "hitters", "feature": k, "coefficient": v}
            for k, v in self.hitter_coefficients.items()
        ] + [
            {"group": "pitchers", "feature": k, "coefficient": v}
            for k, v in self.pitcher_coefficients.items()
        ]
        return pd.DataFrame(rows)


def _design(group: pd.DataFrame, names: list[str]) -> np.ndarray:
    """Standardized feature matrix, z-scored within the position group.

    Standardizing within group is what makes a coefficient mean "how much
    the field cares about this, relative to the spread available at this
    position", and keeps the fitted numbers comparable to the hand-set
    weights they replace.
    """
    cols = []
    for name in names:
        v = group[name].to_numpy(dtype=np.float64)
        sd = v.std()
        cols.append(np.zeros_like(v) if sd < 1e-9 else (v - v.mean()) / sd)
    return np.column_stack(cols)


def _shares(utility: np.ndarray, n_slots: float) -> np.ndarray:
    exp_u = np.exp(utility - utility.max())
    return n_slots * exp_u / exp_u.sum()


def _negative_log_likelihood(
    beta: np.ndarray, groups: list[tuple[np.ndarray, np.ndarray]]
) -> float:
    """Weighted multinomial log-likelihood over position groups."""
    total = 0.0
    for x, counts in groups:
        utility = x @ beta
        utility = utility - utility.max()
        log_p = utility - np.log(np.exp(utility).sum())
        total -= float(counts @ log_p)
    return total


def _prepare(
    features: pd.DataFrame, names: list[str], n_entries: int
) -> list[tuple[np.ndarray, np.ndarray]]:
    groups = []
    for _, group in features.groupby("position", sort=False):
        if len(group) < 2:
            continue
        x = _design(group, names)
        # Counts of entries rostering each player. Ownership is per roster
        # slot, so a group with three slots has three times the counts.
        counts = group["ownership"].to_numpy(dtype=np.float64) * n_entries
        if counts.sum() <= 0:
            continue
        groups.append((x, counts))
    return groups


def fit_ownership(
    features: pd.DataFrame,
    n_entries: int = 10_000,
    l2: float = 0.01,
) -> FittedOwnership:
    """Estimate ownership coefficients from realized contest ownership.

    ``features`` needs the columns in :data:`FEATURES` plus ``position``,
    ``is_pitcher`` and a realized ``ownership`` column.

    ``l2`` is a ridge penalty on the coefficients, deliberately light.

    Watch what it does to the *level* of the projections, not just their
    ordering. A heavy penalty shrinks every coefficient toward zero, which
    flattens the softmax toward uniform: correlation with realized ownership
    stays high because the ordering survives, and mean absolute error even
    improves, because most players on a slate are owned near zero and
    predicting near zero for everyone is close for them. The chalk is what
    breaks, and the chalk is what matters. At l2=1.0 the fit put the top
    plays at 5-9% against a realized 20-35%.

    ``heuristic.implied_field_mean`` is the diagnostic that catches this:
    against real ownership from the backtest contest it reads 76.5, and a
    fit that lands far below that is flat regardless of what its error
    metrics say.
    """
    _validate(features)
    results = {}
    total_ll = 0.0

    for label, is_pitcher, names in (
        ("hitters", False, HITTER_FEATURES),
        ("pitchers", True, PITCHER_FEATURES),
    ):
        subset = features[features["is_pitcher"] == is_pitcher]
        groups = _prepare(subset, names, n_entries)
        if not groups:
            results[label] = {n: 0.0 for n in names}
            continue

        scale = sum(c.sum() for _, c in groups)

        def objective(beta, groups=groups, scale=scale):
            return _negative_log_likelihood(beta, groups) / scale + l2 * float(
                beta @ beta
            )

        fit = minimize(objective, np.zeros(len(names)), method="L-BFGS-B")
        results[label] = dict(zip(names, fit.x))
        total_ll -= _negative_log_likelihood(fit.x, groups)

    return FittedOwnership(
        hitter_coefficients=results["hitters"],
        pitcher_coefficients=results["pitchers"],
        n_players=len(features),
        log_likelihood=total_ll,
    )


def cross_validate(
    features: pd.DataFrame, n_entries: int = 10_000, l2: float = 0.01
) -> pd.DataFrame:
    """Leave-one-position-out validation of the fitted coefficients.

    Coefficients are shared across position groups, so holding a group out
    of the fit and predicting it is a genuine out-of-sample test of whether
    the field's preferences are stable across positions.

    It is not a test of whether they are stable across *slates*, which is
    the generalization that actually matters and which one contest cannot
    provide. Read this as a floor on the error, not an estimate of it.
    """
    rows = []
    for position in sorted(features["position"].unique()):
        train = features[features["position"] != position]
        test = features[features["position"] == position].copy()
        if len(test) < 2 or train.empty:
            continue

        # Pitchers carry their own coefficient vector and form a single
        # position group, so holding them out leaves nothing to fit them
        # with and the result measures an untrained model rather than an
        # out-of-sample one. Only the hitter groups are informative here.
        if bool(test["is_pitcher"].iloc[0]):
            continue

        model = fit_ownership(train, n_entries=n_entries, l2=l2)
        predicted = model.predict(test).set_index("player_id")["ownership"]
        test = test.set_index("player_id")
        error = predicted.reindex(test.index) - test["ownership"]

        rows.append(
            {
                "position": position,
                "players": len(test),
                "mae": float(error.abs().mean()),
                "bias": float(error.mean()),
                "max_error": float(error.abs().max()),
            }
        )
    return pd.DataFrame(rows)


def compare_to_heuristic(
    features: pd.DataFrame, fitted: FittedOwnership
) -> pd.DataFrame:
    """Fitted against hand-set weights, both scored on realized ownership."""
    from .heuristic import project_ownership_from_features

    rows = []
    for label, predicted in (
        ("heuristic (hand-set)", project_ownership_from_features(features)),
        ("fitted (this slate)", fitted.predict(features)),
    ):
        merged = predicted.set_index("player_id")[["ownership"]].rename(
            columns={"ownership": "predicted"}
        )
        merged = merged.join(features.set_index("player_id")["ownership"])
        error = merged["predicted"] - merged["ownership"]
        chalk = merged.nlargest(20, "ownership")
        rows.append(
            {
                "model": label,
                "mae": float(error.abs().mean()),
                "bias": float(error.mean()),
                "mae_top20_owned": float(
                    (chalk["predicted"] - chalk["ownership"]).abs().mean()
                ),
                "corr": float(merged["predicted"].corr(merged["ownership"])),
            }
        )
    return pd.DataFrame(rows)


def _validate(features: pd.DataFrame) -> None:
    required = set(FEATURES) | {"position", "is_pitcher", "ownership", "player_id"}
    missing = sorted(required - set(features.columns))
    if missing:
        raise ValueError(f"features frame is missing columns: {missing}")
