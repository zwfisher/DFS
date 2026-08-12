"""Synthetic opponent field generation.

Ownership projections do not tell you what to do. They tell you what
fraction of entries hold each player, which is a marginal -- and tournaments
are decided by joints. Two slates with identical ownership can demand
completely different lineups depending on whether the field's exposure to a
team is spread across many lineups or concentrated in five-stacks.

So the field is generated as actual lineups: sample opponents under
DraftKings rules with realistic stacking behaviour, then calibrate the
generator until its marginal ownership reproduces the projection. What comes
out is a matrix of opponent rosters that can be scored against the same
simulations as your own lineups, which is what makes expected ROI
computable at all.

Two implementation notes on speed, since the field wants to be tens of
thousands of lineups:

* Selection uses the Gumbel top-k trick -- adding Gumbel noise to log
  weights and taking the top k is exactly sampling without replacement from
  a categorical, and it vectorizes across every lineup at once.
* Slots are filled scarcest-position-first with a running budget penalty, so
  most lineups land under the cap by construction instead of being
  rejected.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np
import pandas as pd

from ..config import OWNERSHIP, ROSTER, OwnershipConfig
from ..slate import Slate
from .heuristic import primary_position

# Scarcest first: a catcher pool is small, outfield is deep, so committing
# the scarce slots early avoids painting the lineup into a corner.
SLOT_ORDER = ["P", "P", "C", "SS", "2B", "3B", "1B", "OF", "OF", "OF"]

# How hard the sampler avoids players it cannot afford given what is left.
# Applied per $1000 of salary above the slot's allowance.
BUDGET_PENALTY = 1.0
# Mild pressure against leaving money on the table. Real entries spend
# almost the whole cap; without this the field drifts cheap and every
# expensive player ends up under-owned no matter how the weights are tuned.
UNDERSPEND_PENALTY = 0.30
# Leftover salary below this is not worth another swap.
SURPLUS_TOLERANCE = 900.0
# Log-weight added to a stack-team player in a slot the stack has claimed.
# This is deliberately large: someone who has decided to stack Colorado
# takes Colorado's hitters more or less regardless of what they are owned,
# and a merely multiplicative boost cannot reproduce that -- ownership
# weights span two orders of magnitude, so a low-owned player on the stack
# team still loses to chalk elsewhere and the field comes out barely
# stacked at all.
STACK_FORCE = 12.0
# Penalty on hitters facing the lineup's own pitcher. The field does this
# sometimes, but not often.
OPPOSING_PENALTY = 0.18


@dataclass
class Field:
    """A generated field of opponent lineups."""

    # Indices into ``player_ids``, which is the row order of the ownership
    # frame the field was built from -- NOT the column order of a score
    # matrix. The two differ, because ownership frames come out sorted by
    # ownership. Always score through ``score()``, which remaps.
    lineups: np.ndarray  # (n_lineups, 10) int32
    player_ids: list[str]
    salaries: np.ndarray  # (n_lineups,) total salary
    realized_ownership: np.ndarray  # (n_players,) marginal ownership

    @property
    def size(self) -> int:
        return self.lineups.shape[0]

    def score(
        self,
        scores: np.ndarray,
        score_player_ids: list[str],
        sim_chunk: slice | None = None,
    ) -> np.ndarray:
        """Score every field lineup across simulations.

        ``scores`` is the ``(n_sims, n_players)`` matrix whose columns are
        ordered by ``score_player_ids``. Returns ``(n_sims, n_lineups)``.
        Chunk the simulations when the field is large -- 20k lineups by 20k
        sims is 1.6 GB in one piece.

        The remap is not optional bookkeeping. Indexing the score matrix
        with raw field indices silently scores every opponent lineup with
        the wrong players, and the result looks entirely plausible.
        """
        cols = self.score_columns(score_player_ids)
        block = scores if sim_chunk is None else scores[sim_chunk]
        return block[:, cols[self.lineups]].sum(axis=2)

    def score_columns(self, score_player_ids: list[str]) -> np.ndarray:
        """Map this field's player indices onto score matrix columns."""
        lookup = {pid: i for i, pid in enumerate(score_player_ids)}
        missing = [pid for pid in self.player_ids if pid not in lookup]
        if missing:
            raise ValueError(
                f"{len(missing)} field players missing from the score matrix: "
                f"{missing[:5]}"
            )
        return np.array([lookup[pid] for pid in self.player_ids], dtype=np.int32)


def _gumbel_topk(
    log_w: np.ndarray, k: int, rng: np.random.Generator
) -> np.ndarray:
    """Sample ``k`` distinct columns per row, proportional to ``exp(log_w)``.

    Adding Gumbel noise to log weights and taking the top k is exact
    sampling without replacement under the Plackett-Luce model, and unlike a
    loop over ``rng.choice`` it runs on every lineup simultaneously.
    """
    gumbel = rng.gumbel(size=log_w.shape)
    perturbed = log_w + gumbel
    if k == 1:
        return perturbed.argmax(axis=1)[:, None]
    idx = np.argpartition(-perturbed, k - 1, axis=1)[:, :k]
    # argpartition does not order within the selection; order so that
    # repeated slots of the same position get a stable assignment.
    rows = np.arange(log_w.shape[0])[:, None]
    order = np.argsort(-perturbed[rows, idx], axis=1)
    return idx[rows, order]


def generate_field(
    slate: Slate,
    ownership: pd.DataFrame,
    n_lineups: int = 10_000,
    seed: int = 0,
    cfg: OwnershipConfig = OWNERSHIP,
    calibration_rounds: int = 4,
) -> Field:
    """Generate a field whose marginal ownership matches the projection.

    The generator is run several times, each time nudging its internal
    weights by the ratio of target to realized ownership. That is iterative
    proportional fitting: the stacking behaviour distorts marginals away
    from the raw weights, and this corrects for it without having to reason
    about the distortion analytically.
    """
    rng = np.random.default_rng(seed)
    ctx = _FieldContext(slate, ownership, cfg)

    weights = ctx.target.copy()
    field = None
    for _ in range(max(1, calibration_rounds)):
        field = _sample_field(ctx, weights, n_lineups, rng)
        ratio = ctx.target / np.maximum(field.realized_ownership, 1e-4)
        # Damped update; a full correction oscillates.
        weights = np.clip(weights * ratio ** 0.65, 1e-5, 50.0)

    return field


class _FieldContext:
    """Pools, salaries and team structure resolved to array indices once."""

    def __init__(self, slate: Slate, ownership: pd.DataFrame, cfg: OwnershipConfig):
        self.projection = ownership["proj"].to_numpy(dtype=np.float64)
        self.cfg = cfg
        self.player_ids = list(ownership["player_id"])
        self.index = {pid: i for i, pid in enumerate(self.player_ids)}
        self.n_players = len(self.player_ids)

        self.salary = ownership["salary"].to_numpy(dtype=np.float64)
        self.target = ownership["ownership"].to_numpy(dtype=np.float64).copy()
        self.is_pitcher = ownership["is_pitcher"].to_numpy()
        position = ownership["position"].to_numpy()

        teams = sorted({p.team for p in slate.players})
        self.team_index = {t: i for i, t in enumerate(teams)}
        self.player_team = np.array(
            [self.team_index[slate.player(pid).team] for pid in self.player_ids]
        )
        self.player_opp = np.array(
            [self.team_index[slate.player(pid).opponent] for pid in self.player_ids]
        )

        self.pools = {
            pos: np.flatnonzero(position == pos) for pos in set(SLOT_ORDER)
        }
        for pos, pool in self.pools.items():
            if pool.size == 0:
                raise ValueError(f"no players available at position {pos}")

        # Ownership-weighted typical salary per position. A flat "budget
        # divided by slots remaining" allowance is wrong here: pitchers cost
        # two to three times what hitters do, so a flat allowance prices
        # every ace out of the field and no amount of weight calibration can
        # put him back in.
        self.baseline_salary = {}
        for pos, pool in self.pools.items():
            w = np.maximum(self.target[pool], 1e-6)
            self.baseline_salary[pos] = float(
                np.average(self.salary[pool], weights=w)
            )
        # Salary the remaining slots are expected to need, by slot number.
        self.reserve_after = np.array(
            [
                sum(self.baseline_salary[p] for p in SLOT_ORDER[k + 1 :])
                for k in range(len(SLOT_ORDER))
            ]
        )

        # Teams ranked by how much hitter ownership the field puts on them;
        # this is the distribution stack teams are drawn from.
        hitter_own = np.where(self.is_pitcher, 0.0, self.target)
        stack_mass = np.zeros(len(teams))
        np.add.at(stack_mass, self.player_team, hitter_own)
        self.stack_weights = stack_mass / stack_mass.sum()
        self.n_teams = len(teams)

        # For each team and each hitter slot, how attractive that slot is to
        # fill from this team -- taken as the best projected ownership among
        # the team's players in that pool. Stackers build around the top of
        # the order, and within a team ownership tracks batting order closely
        # enough to stand in for it here.
        self.hitter_slots = [
            k for k, pos in enumerate(SLOT_ORDER) if pos != "P"
        ]
        self.team_slot_weight = np.full(
            (self.n_teams, len(SLOT_ORDER)), 1e-6, dtype=np.float64
        )
        for k in self.hitter_slots:
            pool = self.pools[SLOT_ORDER[k]]
            for t in range(self.n_teams):
                members = pool[self.player_team[pool] == t]
                if members.size:
                    self.team_slot_weight[t, k] = float(self.target[members].max())


def _sample_field(
    ctx: _FieldContext, weights: np.ndarray, n_lineups: int, rng: np.random.Generator
) -> Field:
    """One pass of lineup construction at the given weights.

    The sampler controls the *shape* of the field -- which players appear
    together, how often a five-stack shows up -- but not its average
    strength. Because ownership sums to the roster slots, the expected
    projection of a field lineup is exactly the ownership-weighted sum of
    player projections, so it is pinned by the ownership model no matter
    how lineups are assembled. Field strength is calibrated in
    ``heuristic.py``; see ``calibrate_to_field_strength``.
    """
    cfg = ctx.cfg
    n = n_lineups

    # Which team each lineup stacks, and how many hitters it commits to it.
    stacks = rng.choice(ctx.n_teams, size=n, p=ctx.stack_weights)
    sizes = rng.choice(
        cfg.field_stack_sizes,
        size=n,
        p=np.asarray(cfg.field_stack_size_weights)
        / np.sum(cfg.field_stack_size_weights),
    )
    unstacked = rng.random(n) >= cfg.field_stack_rate
    sizes = np.where(unstacked, 0, sizes)

    # Decide up front which hitter slots each lineup's stack will occupy,
    # rather than letting the stack fill whichever slots come first. Filling
    # greedily biases every stack into the same positions -- always the
    # scarce infield, never an outfielder -- which distorts exactly the
    # player-level exposures the field is supposed to represent.
    claims = np.zeros((n, len(SLOT_ORDER)), dtype=bool)
    hitter_slots = np.array(ctx.hitter_slots)
    slot_pref = np.log(ctx.team_slot_weight[stacks][:, hitter_slots])
    order = _gumbel_topk(slot_pref, len(hitter_slots), rng)
    rank = np.empty_like(order)
    np.put_along_axis(
        rank, order, np.arange(len(hitter_slots))[None, :].repeat(n, axis=0), axis=1
    )
    claims[:, hitter_slots] = rank < sizes[:, None]

    lineup = np.zeros((n, len(SLOT_ORDER)), dtype=np.int32)
    spent = np.zeros(n)
    stack_used = np.zeros(n, dtype=np.int16)
    own_pitcher_team = np.full((n, 2), -1, dtype=np.int32)

    log_w = np.log(np.maximum(weights, 1e-9))
    filled = 0

    for slot_no, pos in enumerate(SLOT_ORDER):
        pool = ctx.pools[pos]
        budget_left = ROSTER.salary_cap - spent
        # What this slot can spend while still leaving a typical amount for
        # every slot after it.
        allowance = budget_left - ctx.reserve_after[slot_no]

        w = np.broadcast_to(log_w[pool], (n, pool.size)).copy()

        salaries = ctx.salary[pool][None, :]
        over = np.maximum(salaries - allowance[:, None], 0.0)
        under = np.maximum(allowance[:, None] * 0.55 - salaries, 0.0)
        w -= BUDGET_PENALTY * over / 1000.0
        w -= UNDERSPEND_PENALTY * under / 1000.0

        if pos != "P":
            on_stack = ctx.player_team[pool][None, :] == stacks[:, None]
            # Where the stack has claimed this slot, take a stack-team
            # player. Rows whose stack team has nobody in this pool fall back
            # to ordinary sampling rather than becoming infeasible.
            claimed = claims[:, slot_no][:, None]
            feasible = (on_stack & claimed).any(axis=1)[:, None]
            w = w + STACK_FORCE * (on_stack & claimed & feasible)
            # Never exceed the DraftKings cap of five hitters per team.
            at_cap = (stack_used >= ROSTER.max_hitters_per_team)[:, None]
            w = np.where(on_stack & at_cap, -np.inf, w)

            if not ROSTER.allow_hitters_vs_own_pitcher:
                faces_own_p = (
                    ctx.player_opp[pool][None, :] == own_pitcher_team[:, 0:1]
                ) | (ctx.player_opp[pool][None, :] == own_pitcher_team[:, 1:2])
                w = w + np.log(OPPOSING_PENALTY) * faces_own_p

        # Exclude anyone already on the roster (only possible within a pool
        # used by more than one slot: pitchers and outfielders).
        if filled:
            taken = lineup[:, :filled]
            same_pool = np.isin(taken, pool)
            if same_pool.any():
                pos_in_pool = np.searchsorted(pool, taken)
                rows = np.repeat(np.arange(n), filled)
                cols = pos_in_pool.ravel()
                valid = same_pool.ravel()
                w[rows[valid], cols[valid]] = -np.inf

        pick_local = _gumbel_topk(w, 1, rng)[:, 0]
        pick = pool[pick_local]
        lineup[:, slot_no] = pick
        spent += ctx.salary[pick]
        filled += 1

        if pos == "P":
            own_pitcher_team[:, slot_no] = ctx.player_team[pick]
        else:
            stack_used += (ctx.player_team[pick] == stacks).astype(np.int16)

    lineup, spent = _repair_salary(ctx, lineup, spent, weights, rng)
    lineup, spent = _spend_surplus(ctx, lineup, spent, weights, stacks, rng)

    counts = np.bincount(lineup.ravel(), minlength=ctx.n_players)
    realized = counts / lineup.shape[0]

    return Field(
        lineups=lineup,
        player_ids=ctx.player_ids,
        salaries=spent,
        realized_ownership=realized,
    )


def _repair_salary(
    ctx: _FieldContext,
    lineup: np.ndarray,
    spent: np.ndarray,
    weights: np.ndarray,
    rng: np.random.Generator,
    max_passes: int = 6,
) -> tuple[np.ndarray, np.ndarray]:
    """Swap the priciest player out of over-cap lineups until they fit.

    The budget penalty during construction keeps most lineups legal; this
    cleans up the tail rather than doing the heavy lifting.
    """
    for _ in range(max_passes):
        over = spent > ROSTER.salary_cap
        if not over.any():
            break
        rows = np.flatnonzero(over)

        # Replace the most expensive slot with a cheaper player of the same
        # position, chosen by ownership weight among affordable options.
        slot_salaries = ctx.salary[lineup[rows]]
        worst = slot_salaries.argmax(axis=1)

        for j, row in enumerate(rows):
            slot_no = int(worst[j])
            pos = SLOT_ORDER[slot_no]
            pool = ctx.pools[pos]
            current = lineup[row, slot_no]
            headroom = ROSTER.salary_cap - (spent[row] - ctx.salary[current])

            eligible = pool[
                (ctx.salary[pool] <= headroom) & ~np.isin(pool, lineup[row])
            ]
            if eligible.size == 0:
                continue
            p = weights[eligible]
            p = p / p.sum()
            new = rng.choice(eligible, p=p)
            spent[row] += ctx.salary[new] - ctx.salary[current]
            lineup[row, slot_no] = new

    legal = spent <= ROSTER.salary_cap
    return lineup[legal], spent[legal]


def _team_counts(ctx: _FieldContext, lineup: np.ndarray) -> np.ndarray:
    """Hitters per team for each lineup, as ``(n_lineups, n_teams)``."""
    n = lineup.shape[0]
    teams = ctx.player_team[lineup]
    hitters = ~ctx.is_pitcher[lineup]
    flat = (np.arange(n)[:, None] * ctx.n_teams + teams)[hitters]
    return np.bincount(flat, minlength=n * ctx.n_teams).reshape(n, ctx.n_teams)


def _spend_surplus(
    ctx: _FieldContext,
    lineup: np.ndarray,
    spent: np.ndarray,
    weights: np.ndarray,
    stacks: np.ndarray,
    rng: np.random.Generator,
    max_passes: int = 2,
) -> tuple[np.ndarray, np.ndarray]:
    """Upgrade players in lineups that left salary unspent.

    Real entries use nearly the whole cap. Construction alone drifts cheap,
    because at every slot the safe choice is the affordable one, and the
    cumulative effect is a field that under-rosters expensive players --
    which no amount of weight calibration can fix, since the weights are not
    what is binding. Upgrading afterwards attacks the actual cause.

    Team-count and opposing-pitcher constraints are re-checked on every
    swap so the upgrade cannot quietly produce an illegal lineup, and a
    stack-team player may only be replaced by another player from the same
    team -- otherwise the upgrade pass spends its surplus dismantling the
    stacks the construction pass just built.
    """
    log_w = np.log(np.maximum(weights, 1e-9))

    for _ in range(max_passes):
        for slot_no, pos in enumerate(SLOT_ORDER):
            surplus = ROSTER.salary_cap - spent
            rows = np.flatnonzero(surplus > SURPLUS_TOLERANCE)
            if rows.size == 0:
                return lineup, spent

            pool = ctx.pools[pos]
            sub = lineup[rows]
            current = sub[:, slot_no]
            budget = ctx.salary[current] + surplus[rows]

            sal = ctx.salary[pool][None, :]
            # Strict upgrades only, and only ones that fit.
            ok = (sal <= budget[:, None]) & (sal > ctx.salary[current][:, None])

            # Not already on this roster.
            in_pool = np.isin(sub, pool)
            if in_pool.any():
                cols = np.searchsorted(pool, sub)
                r = np.repeat(np.arange(rows.size), sub.shape[1])
                ok[r[in_pool.ravel()], cols.ravel()[in_pool.ravel()]] = False

            if pos != "P":
                # A stack-team player is only ever swapped for a teammate,
                # so the stack shape survives the upgrade.
                is_stack_player = ctx.player_team[current] == stacks[rows]
                cand_team = ctx.player_team[pool][None, :]
                same_team = cand_team == ctx.player_team[current][:, None]
                ok &= ~is_stack_player[:, None] | same_team

                # Respect the five-hitters-per-team cap. Swapping out the
                # current player frees one slot on his own team.
                counts = _team_counts(ctx, sub)
                cur_team = ctx.player_team[current]
                counts[np.arange(rows.size), cur_team] -= 1
                at_cap = counts >= ROSTER.max_hitters_per_team
                ok &= ~at_cap[:, ctx.player_team[pool]]

                if not ROSTER.allow_hitters_vs_own_pitcher:
                    p_teams = ctx.player_team[sub[:, 0:2]]
                    opp = ctx.player_opp[pool][None, :]
                    faces = (opp == p_teams[:, 0:1]) | (opp == p_teams[:, 1:2])
                    ok &= ~faces

            has_option = ok.any(axis=1)
            if not has_option.any():
                continue

            w = np.where(ok, np.broadcast_to(log_w[pool], ok.shape), -np.inf)
            pick = pool[_gumbel_topk(w, 1, rng)[:, 0]]

            apply = rows[has_option]
            new = pick[has_option]
            old = lineup[apply, slot_no]
            spent[apply] += ctx.salary[new] - ctx.salary[old]
            lineup[apply, slot_no] = new

    return lineup, spent


def field_diagnostics(field: Field, ownership: pd.DataFrame) -> pd.DataFrame:
    """Compare the generated field's marginals against the target.

    The point of calibration is that these two columns agree. If they do
    not, the field is a different field from the one the ownership model
    describes, and every ROI number computed against it is measuring the
    wrong tournament.
    """
    out = ownership.copy()
    out["field_ownership"] = field.realized_ownership
    out["ownership_error"] = out["field_ownership"] - out["ownership"]
    return out.sort_values("ownership", ascending=False).reset_index(drop=True)


def stack_shape_distribution(
    field: Field, player_team: np.ndarray, is_pitcher: np.ndarray
) -> pd.Series:
    """How often the generated field plays each largest-stack size.

    This is the check that the field is shaped like a real field and not
    merely correct on the marginals: the same ownership can come from a
    tournament of five-stacks or one of scattered one-offs, and those two
    fields reward completely different lineups.
    """
    n = field.size
    n_teams = int(player_team.max()) + 1
    teams = player_team[field.lineups]
    hitters = ~is_pitcher[field.lineups]
    flat = (np.arange(n)[:, None] * n_teams + teams)[hitters]
    counts = np.bincount(flat, minlength=n * n_teams).reshape(n, n_teams)
    return (
        pd.Series(counts.max(axis=1))
        .value_counts(normalize=True)
        .sort_index()
        .rename("share")
    )
