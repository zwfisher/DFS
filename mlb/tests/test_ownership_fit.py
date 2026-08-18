"""Fitting ownership weights to realized contest ownership.

The heuristic weights were guesses; these tests cover replacing them with
maximum-likelihood estimates, and the specific way that fit can go wrong.
"""

from __future__ import annotations

import numpy as np
import pandas as pd
import pytest

from mlbdfs.ownership.fit import (
    HITTER_FEATURES,
    cross_validate,
    fit_ownership,
)


def _synthetic(n_per_pos: int = 30, seed: int = 0, beta=(0.0, 0.3, 0.5, 0.0, 0.45, 0.05)):
    """Slate whose ownership is generated from known coefficients."""
    rng = np.random.default_rng(seed)
    rows = []
    for position in ("C", "1B", "2B", "3B", "SS", "OF", "P"):
        n = n_per_pos
        proj = rng.normal(8, 2, n)
        salary = rng.normal(4200, 900, n)
        rows.append(pd.DataFrame({
            "player_id": [f"{position}{i}" for i in range(n)],
            "position": position,
            "is_pitcher": position == "P",
            "proj": proj,
            "ceiling": proj * rng.normal(2.2, 0.2, n),
            "salary": salary,
            "value": proj / (salary / 1000.0),
            "team_total": rng.normal(4.5, 0.6, n),
            "batting_order": rng.integers(1, 10, n),
        }))
    f = pd.concat(rows, ignore_index=True)
    f["top_of_order"] = (f["batting_order"] <= 5).astype(float)

    slots = {"C": 1, "1B": 1, "2B": 1, "3B": 1, "SS": 1, "OF": 3, "P": 2}
    own = []
    for position, g in f.groupby("position", sort=False):
        x = np.column_stack([
            (g[c].to_numpy() - g[c].mean()) / max(g[c].std(), 1e-9)
            for c in HITTER_FEATURES
        ])
        u = x @ np.array(beta)
        e = np.exp(u - u.max())
        own.append(pd.Series(slots[position] * e / e.sum(), index=g.index))
    f["ownership"] = pd.concat(own).sort_index()
    return f


def test_recovers_known_coefficients():
    truth = (0.0, 0.3, 0.5, 0.0, 0.45, 0.05)
    model = fit_ownership(_synthetic(seed=1, beta=truth), n_entries=50_000, l2=1e-4)

    fitted = np.array([model.hitter_coefficients[c] for c in HITTER_FEATURES])
    # team_total is generated with a zero weight, so it is not identified by
    # signal; compare the features that carry one.
    for i, name in enumerate(HITTER_FEATURES):
        if truth[i] != 0:
            assert fitted[i] == pytest.approx(truth[i], abs=0.12), name


def test_ownership_still_sums_to_the_roster_slots():
    f = _synthetic(seed=2)
    predicted = fit_ownership(f, n_entries=20_000).predict(f)
    slots = {"C": 1, "1B": 1, "2B": 1, "3B": 1, "SS": 1, "OF": 3, "P": 2}
    for position, g in predicted.groupby("position"):
        assert g["ownership"].sum() == pytest.approx(slots[position], abs=1e-6)


def test_a_heavy_ridge_flattens_the_chalk():
    """The failure the error metrics do not catch.

    Shrinking every coefficient toward zero flattens the softmax toward
    uniform. Ordering survives, so correlation stays high, and mean absolute
    error even improves because most players are owned near zero. The chalk
    is what breaks, and the chalk is what matters.
    """
    f = _synthetic(seed=3)
    light = fit_ownership(f, n_entries=50_000, l2=1e-4).predict(f)
    heavy = fit_ownership(f, n_entries=50_000, l2=5.0).predict(f)

    assert heavy["ownership"].max() < light["ownership"].max()
    # And the diagnostic that catches it, which the error metrics do not.
    implied = lambda p: float(
        (p.set_index("player_id")["ownership"] * f.set_index("player_id")["proj"]).sum()
    )
    assert implied(heavy) < implied(light)


def test_cross_validation_skips_pitchers():
    """Pitchers are one group with their own coefficients, so holding them
    out leaves nothing to fit them with and measures an untrained model."""
    cv = cross_validate(_synthetic(seed=4), n_entries=20_000)
    assert "P" not in set(cv["position"])
    assert len(cv) == 6


def test_fit_reports_missing_columns():
    with pytest.raises(ValueError, match="missing columns"):
        fit_ownership(pd.DataFrame({"player_id": ["a"], "position": ["C"]}))


def test_top_of_order_excludes_players_with_no_batting_order():
    """`order <= 5` is true for the 0 that means 'not in the lineup'.

    build_features writes 0 for a hitter with no batting order, so the naive
    test handed the top-of-order bonus to every non-starter on the slate --
    456 players rather than 70 on draft group 152195.
    """
    import numpy as np

    from mlbdfs.ownership.heuristic import _top_of_order

    got = _top_of_order(np.array([0, 1, 3, 5, 6, 9]))
    assert list(got) == [0.0, 1.0, 1.0, 1.0, 0.0, 0.0]


def test_build_features_emits_the_column_the_fit_consumes():
    """project --out has to feed fit-ownership without a translation step."""
    from mlbdfs.data.fixtures import make_slate
    from mlbdfs.ownership.fit import FEATURES
    from mlbdfs.ownership.heuristic import build_features
    from mlbdfs.projections.build import build_sim_slate
    from mlbdfs.sim.engine import simulate_slate

    slate, book = make_slate(n_games=2, seed=1)
    sim = simulate_slate(build_sim_slate(slate, book), n_sims=60, seed=1)
    feats = build_features(slate, sim)

    missing = sorted(set(FEATURES) - set(feats.columns))
    assert not missing, f"fit_ownership needs {missing}"

    starters = feats[feats["batting_order"].between(1, 5)]
    bench = feats[feats["batting_order"] == 0]
    assert (starters["top_of_order"] == 1.0).all()
    assert (bench["top_of_order"] == 0.0).all()
