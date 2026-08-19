"""Building candidate lineups.

Six golfers under a salary cap is a small enough problem that the interesting
question is not how to solve it but what to solve *for*. Maximising projected
points gives one lineup, and it is the same lineup everybody else's optimizer
gives them. So the pool is built from randomized objectives instead: each
solve maximises the total under one sampled simulation, which produces
lineups that win in different futures rather than lineups that are all
slightly worse versions of the chalk.
"""

from __future__ import annotations

import numpy as np
from ortools.sat.python import cp_model

from ..config import ROSTER_SIZE, SALARY_CAP


def solve_lineup(
    values: np.ndarray,
    salaries: np.ndarray,
    *,
    min_salary: int = 0,
    banned: set[tuple[int, ...]] | None = None,
    locks: list[int] | None = None,
    excludes: list[int] | None = None,
    max_exposure_counts: np.ndarray | None = None,
    scale: int = 1000,
) -> tuple[int, ...] | None:
    """One optimal six-golfer lineup under the cap, or None if infeasible."""
    n = len(values)
    model = cp_model.CpModel()
    take = [model.NewBoolVar(f"g{i}") for i in range(n)]

    model.Add(sum(take) == ROSTER_SIZE)
    model.Add(sum(int(s) * t for s, t in zip(salaries, take)) <= SALARY_CAP)
    if min_salary:
        model.Add(sum(int(s) * t for s, t in zip(salaries, take)) >= int(min_salary))
    for i in locks or []:
        model.Add(take[i] == 1)
    for i in excludes or []:
        model.Add(take[i] == 0)
    if max_exposure_counts is not None:
        for i, allowed in enumerate(max_exposure_counts):
            if allowed <= 0:
                model.Add(take[i] == 0)

    # Forbid lineups already in the pool: at most five of any previous six.
    for lineup in banned or ():
        model.Add(sum(take[i] for i in lineup) <= ROSTER_SIZE - 1)

    model.Maximize(sum(int(round(v * scale)) * t for v, t in zip(values, take)))

    solver = cp_model.CpSolver()
    solver.parameters.num_search_workers = 1
    solver.parameters.max_time_in_seconds = 5.0
    if solver.Solve(model) not in (cp_model.OPTIMAL, cp_model.FEASIBLE):
        return None
    return tuple(i for i in range(n) if solver.Value(take[i]))


def candidate_pool(
    points: np.ndarray,
    salaries: np.ndarray,
    *,
    n_candidates: int,
    min_salary: int,
    rng: np.random.Generator,
    chalk_solves: int = 1,
) -> list[tuple[int, ...]]:
    """A pool of distinct lineups, each optimal under some sampled future.

    The first few solves use the mean projection -- the lineup an ordinary
    optimizer would build is worth having in the pool, if only so the ROI
    stage can reject it explicitly. The rest each draw one simulation and
    optimise against that.
    """
    n_sims = points.shape[0]
    mean = points.mean(axis=0)
    pool: list[tuple[int, ...]] = []
    seen: set[tuple[int, ...]] = set()

    attempts = 0
    while len(pool) < n_candidates and attempts < n_candidates * 4:
        attempts += 1
        if len(pool) < chalk_solves:
            values = mean
        else:
            values = points[rng.integers(n_sims)]
        lineup = solve_lineup(values, salaries, min_salary=min_salary, banned=seen)
        if lineup is None or lineup in seen:
            continue
        seen.add(lineup)
        pool.append(lineup)
    return pool
