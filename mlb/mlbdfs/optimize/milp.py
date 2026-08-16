"""Lineup construction as a mixed integer program.

Every DraftKings rule is a hard constraint and every stacking preference is
an optional one, so the natural formulation is an integer program: binary
inclusion variables per player, assignment variables to handle multi-position
eligibility, and indicator variables for stacks and games represented.

The solver is OR-Tools CP-SAT rather than CBC through PuLP. That is a
performance decision, and a large one: PuLP writes an LP file to disk and
spawns CBC as a subprocess on every solve, which costs about two seconds
regardless of how easy the model is. Building a pool of several hundred
candidates that way takes twenty minutes. CP-SAT runs in process and solves
the same model in tens of milliseconds.

The candidate pool is generated with **randomized objectives** rather than
overlap constraints. Each solve maximizes points under one plausible draw
from the simulation -- a world where this pitcher was sharp and that lineup
got to the bullpen early -- so the pool naturally contains the lineups that
win in different futures. Forcing diversity with overlap limits instead
produces lineups that differ but have no reason to be good.

Note that maximum projected points is *not* the tournament objective. This
module builds a pool of plausible lineups; ``portfolio.py`` decides which of
them to actually enter by measuring expected ROI against the simulated field.
"""

from __future__ import annotations

from dataclasses import dataclass, field as dc_field

import numpy as np
import pandas as pd
from ortools.sat.python import cp_model

from ..config import OPTIMIZER, ROSTER, OptimizerConfig, RosterRules
from ..slate import Slate

HITTER_POSITIONS = ("C", "1B", "2B", "3B", "SS", "OF")

# CP-SAT works in integers; projections are scaled by this and rounded.
# Two decimal places is far finer than the projections themselves.
OBJECTIVE_SCALE = 100


@dataclass
class Lineup:
    """A constructed lineup."""

    player_ids: list[str]
    salary: int
    projection: float
    stack: dict[str, int] = dc_field(default_factory=dict)

    def key(self) -> frozenset[str]:
        return frozenset(self.player_ids)

    def __len__(self) -> int:
        return len(self.player_ids)


class LineupOptimizer:
    """Repeated MILP solves over one slate.

    Models are cached per stack shape and only the objective is swapped
    between solves, so building the constraint set is paid for once.
    """

    def __init__(
        self,
        slate: Slate,
        rules: RosterRules = ROSTER,
        cfg: OptimizerConfig = OPTIMIZER,
    ) -> None:
        self.slate = slate
        self.rules = rules
        self.cfg = cfg

        self.players = [
            p
            for p in slate.players
            if p.in_lineup
            and (p.is_pitcher or p.start_probability >= cfg.min_start_probability)
        ]
        self.player_ids = [p.player_id for p in self.players]
        self.index = {pid: i for i, pid in enumerate(self.player_ids)}
        self.n = len(self.players)

        self.salary = np.array([p.salary for p in self.players])
        self.is_pitcher = np.array([p.is_pitcher for p in self.players])
        self.team = [p.team for p in self.players]
        self.opponent = [p.opponent for p in self.players]
        self.order = [p.batting_order for p in self.players]

        self.teams = sorted({p.team for p in self.players})
        self.hitters_by_team = {
            t: [i for i in range(self.n) if self.team[i] == t and not self.is_pitcher[i]]
            for t in self.teams
        }
        self.games = sorted({slate.game_for(p.team).game_id for p in self.players})
        self.game_of = [slate.game_for(p.team).game_id for p in self.players]
        self.players_by_game = {
            gid: [i for i in range(self.n) if self.game_of[i] == gid]
            for gid in self.games
        }
        # The constraint structure depends only on the stack shape, not the
        # objective, so it is built once per shape and reused.
        self._model_cache: dict[tuple | None, tuple] = {}

        self.slots = dict(rules.slots)
        self.eligible = {
            pos: [
                i
                for i, p in enumerate(self.players)
                if pos in p.positions or (pos == "P" and p.is_pitcher)
            ]
            for pos in self.slots
        }
        for pos, pool in self.eligible.items():
            if len(pool) < self.slots[pos]:
                raise ValueError(
                    f"only {len(pool)} players eligible at {pos}, need {self.slots[pos]}"
                )

    # ------------------------------------------------------------------
    # Model construction
    # ------------------------------------------------------------------

    def _build(
        self, stack_shape: tuple[int, ...] | None
    ) -> tuple[cp_model.CpModel, list, dict]:
        """Construct the model for a stack shape, with no objective set."""
        m = cp_model.CpModel()

        x = [m.NewBoolVar(f"x_{i}") for i in range(self.n)]
        # Assignment variables let a multi-eligible player fill exactly one
        # slot; without them a 2B/SS would be double counted.
        y = {
            (i, pos): m.NewBoolVar(f"y_{i}_{pos}")
            for pos in self.slots
            for i in self.eligible[pos]
        }

        for pos, count in self.slots.items():
            m.Add(sum(y[(i, pos)] for i in self.eligible[pos]) == count)
        for i in range(self.n):
            slots_for_i = [y[(i, pos)] for pos in self.slots if (i, pos) in y]
            if slots_for_i:
                m.Add(sum(slots_for_i) == x[i])
            else:
                m.Add(x[i] == 0)

        m.Add(
            sum(int(self.salary[i]) * x[i] for i in range(self.n))
            <= self.rules.salary_cap
        )
        m.Add(sum(x) == self.rules.size)

        for t, idxs in self.hitters_by_team.items():
            if idxs:
                m.Add(sum(x[i] for i in idxs) <= self.rules.max_hitters_per_team)

        # At least two games represented. Only the upper bound is needed: the
        # indicator can never be forced up without a player from that game,
        # and the >= 2 requirement is what pushes it.
        g = {gid: m.NewBoolVar(f"g_{k}") for k, gid in enumerate(self.games)}
        for gid in self.games:
            members = self.players_by_game[gid]
            m.Add(g[gid] <= sum(x[i] for i in members))
        m.Add(sum(g.values()) >= self.rules.min_games_represented)

        if not self.rules.allow_hitters_vs_own_pitcher:
            # One aggregated constraint per pitcher rather than one per
            # (pitcher, opposing hitter) pair: rostering the pitcher forces
            # the whole opposing lineup to zero.
            for pi in range(self.n):
                if not self.is_pitcher[pi]:
                    continue
                opposing = self.hitters_by_team.get(self.opponent[pi], [])
                if opposing:
                    m.Add(
                        sum(x[h] for h in opposing)
                        <= len(opposing) * (1 - x[pi])
                    )

        self._add_stack_constraints(m, x, stack_shape)
        return m, x, y

    def _add_stack_constraints(
        self, m: cp_model.CpModel, x: list, stack_shape: tuple[int, ...] | None
    ) -> None:
        """Require the lineup to contain stacks of the requested sizes.

        Sizes are lower bounds on hitters per team. Because DraftKings caps
        a team at five hitters and only eight hitter slots exist, a (5, 3)
        request is exact in practice.
        """
        if not stack_shape:
            return

        stack_vars: dict = {}
        for j, size in enumerate(stack_shape):
            s = {
                t: m.NewBoolVar(f"s{j}_{t}")
                for t in self.teams
                if len(self.hitters_by_team[t]) >= size
            }
            if not s:
                raise ValueError(f"no team has {size} available hitters")
            m.Add(sum(s.values()) == 1)
            for t, var in s.items():
                m.Add(
                    sum(x[i] for i in self.hitters_by_team[t]) >= size
                ).OnlyEnforceIf(var)
            stack_vars[j] = s

        # A team can anchor only one of the requested stacks.
        for t in self.teams:
            members = [sv[t] for sv in stack_vars.values() if t in sv]
            if len(members) > 1:
                m.Add(sum(members) <= 1)

        if self.cfg.require_consecutive_order:
            self._add_consecutive_constraints(m, x, stack_vars, stack_shape)

    def _add_consecutive_constraints(
        self,
        m: cp_model.CpModel,
        x: list,
        stack_vars: dict,
        stack_shape: tuple[int, ...],
    ) -> None:
        """Force each stack into a consecutive run of the batting order.

        Five hitters batting first through fifth score together far more
        often than five scattered through the order, because they come to
        the plate in the same innings. The simulator prices that correctly,
        so it is worth asking the optimizer for it explicitly rather than
        hoping it falls out.

        Windows wrap: the nine, one and two hitters are consecutive.
        """
        for j, size in enumerate(stack_shape):
            if size < 3:
                continue  # a pair is consecutive often enough not to constrain
            for t, team_var in stack_vars[j].items():
                idx_by_order = {
                    self.order[i]: i for i in self.hitters_by_team[t] if self.order[i]
                }
                windows = []
                for start in range(1, 10):
                    slots = [((start - 1 + d) % 9) + 1 for d in range(size)]
                    members = [idx_by_order[s] for s in slots if s in idx_by_order]
                    if len(members) == size:
                        windows.append(members)
                if not windows:
                    m.Add(team_var == 0)
                    continue

                w = [m.NewBoolVar(f"w{j}_{t}_{k}") for k in range(len(windows))]
                m.Add(sum(w) == 1).OnlyEnforceIf(team_var)
                m.Add(sum(w) == 0).OnlyEnforceIf(team_var.Not())
                for k, members in enumerate(windows):
                    m.Add(sum(x[i] for i in members) >= size).OnlyEnforceIf(w[k])

    # ------------------------------------------------------------------
    # Solving
    # ------------------------------------------------------------------

    def _solver(self) -> cp_model.CpSolver:
        """A solver told not to chase proven optimality.

        A candidate pool does not need the true optimum of a noisy sampled
        objective -- a lineup within half a percent is indistinguishable in
        every way that matters, and the last sliver of the gap is where
        branch and bound spends most of its time.

        ``cfg.deterministic`` trades roughly 3.5x runtime for a pool that is
        reproducible from the seed; see the note on the config field.
        """
        solver = cp_model.CpSolver()
        solver.parameters.max_time_in_seconds = float(self.cfg.solve_time_limit)
        solver.parameters.relative_gap_limit = self.cfg.mip_gap
        solver.parameters.num_workers = (
            1 if self.cfg.deterministic else self.cfg.solver_workers
        )
        solver.parameters.log_search_progress = self.cfg.solver_msg
        return solver

    def _cached_model(self, stack_shape: tuple[int, ...] | None) -> tuple:
        key = tuple(stack_shape) if stack_shape else None
        if key not in self._model_cache:
            self._model_cache[key] = self._build(stack_shape)
        return self._model_cache[key]

    def solve(
        self,
        objective: np.ndarray,
        stack_shape: tuple[int, ...] | None = None,
        banned: list[frozenset[str]] | None = None,
        locks: list[str] | None = None,
        excludes: list[str] | None = None,
        max_overlap: int | None = None,
    ) -> Lineup | None:
        """Solve for one lineup. Returns ``None`` if the model is infeasible.

        With no locks, exclusions or banned lineups the cached model for
        this stack shape is reused and only the objective is replaced.
        """
        reusable = not (banned or locks or excludes) and max_overlap is None
        if reusable:
            m, x, _ = self._cached_model(stack_shape)
        else:
            m, x, _ = self._build(stack_shape)
            for pid in locks or []:
                m.Add(x[self.index[pid]] == 1)
            for pid in excludes or []:
                if pid in self.index:
                    m.Add(x[self.index[pid]] == 0)
            limit = self.rules.size - 1 if max_overlap is None else max_overlap
            for prev in banned or []:
                idxs = [self.index[pid] for pid in prev if pid in self.index]
                if idxs:
                    m.Add(sum(x[i] for i in idxs) <= limit)

        coeffs = np.rint(np.asarray(objective) * OBJECTIVE_SCALE).astype(np.int64)
        m.Maximize(sum(int(coeffs[i]) * x[i] for i in range(self.n)))

        solver = self._solver()
        status = solver.Solve(m)
        if status not in (cp_model.OPTIMAL, cp_model.FEASIBLE):
            return None

        chosen = [i for i in range(self.n) if solver.Value(x[i])]
        ids = [self.player_ids[i] for i in chosen]

        stack: dict[str, int] = {}
        for i in chosen:
            if not self.is_pitcher[i]:
                stack[self.team[i]] = stack.get(self.team[i], 0) + 1

        return Lineup(
            player_ids=ids,
            salary=int(self.salary[chosen].sum()),
            projection=float(np.asarray(objective)[chosen].sum()),
            stack=dict(sorted(stack.items(), key=lambda kv: -kv[1])),
        )

    def generate_pool(
        self,
        scores: np.ndarray,
        score_player_ids: list[str],
        n_candidates: int,
        seed: int = 0,
        draws_per_world: int = 25,
        stack_shapes: tuple[tuple[int, ...], ...] | None = None,
    ) -> list[Lineup]:
        """Build a candidate pool by optimizing under many sampled worlds.

        Each iteration averages a handful of simulations into one "world"
        and optimizes against it. Averaging a few draws rather than taking a
        single one keeps the correlation structure -- the whole reason to
        sample from the simulator instead of adding independent noise --
        while cutting the noise enough that the solve is not chasing one
        lucky home run.
        """
        rng = np.random.default_rng(seed)
        cols = self._align(score_player_ids)
        aligned = scores[:, cols]
        mean = aligned.mean(axis=0)

        shapes = stack_shapes if stack_shapes is not None else self.cfg.stack_shapes
        pool: list[Lineup] = []
        seen: set[frozenset[str]] = set()

        for k in range(n_candidates):
            rows = rng.integers(0, aligned.shape[0], size=draws_per_world)
            world = aligned[rows].mean(axis=0)
            objective = mean + self.cfg.randomize_sigma * (world - mean)

            shape = shapes[k % len(shapes)] if shapes else None
            lineup = self.solve(objective, stack_shape=shape)
            if lineup is None:
                continue
            key = lineup.key()
            if key in seen:
                continue
            seen.add(key)
            pool.append(lineup)

        return pool

    def _align(self, score_player_ids: list[str]) -> np.ndarray:
        """Column indices mapping the score matrix onto this optimizer's order."""
        lookup = {pid: i for i, pid in enumerate(score_player_ids)}
        missing = [pid for pid in self.player_ids if pid not in lookup]
        if missing:
            raise ValueError(f"{len(missing)} players missing from score matrix: {missing[:5]}")
        return np.array([lookup[pid] for pid in self.player_ids])


def lineups_to_frame(lineups: list[Lineup], slate: Slate) -> pd.DataFrame:
    """Readable table of generated lineups."""
    rows = []
    for n, lu in enumerate(lineups):
        names = [slate.player(pid).name for pid in lu.player_ids]
        rows.append(
            {
                "lineup": n + 1,
                "salary": lu.salary,
                "projection": round(lu.projection, 2),
                "stack": " / ".join(f"{t}{c}" for t, c in lu.stack.items() if c >= 2),
                "players": ", ".join(names),
            }
        )
    return pd.DataFrame(rows)
