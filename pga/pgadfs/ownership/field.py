"""The other 35,000 entries.

Ownership marginals do not tell you what to do. Two slates with identical
ownership can reward opposite lineups depending on how the field *combines*
those golfers, and a six-man roster under a salary cap combines them in a
very particular way: entries spend almost the whole cap, so a lineup with
one expensive golfer is forced cheap everywhere else. That joint structure
is what decides whether being 20% on a golfer is contrarian or chalky, and
the only way to see it is to build the field.

Lineups are drawn one golfer at a time, weighted by ownership, with the
golfers who would strand the remaining budget dropped before each pick.
Conditioning on the cap as it goes -- rather than sampling freely and
filtering afterwards -- is what makes the field buildable at all here; see
`_sample_lineups`. The conditioning still distorts the marginals, so the
sampling weights are re-fitted until the field's realised ownership matches
the projection.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np

from ..config import ROSTER_SIZE, SALARY_CAP, FieldConfig
from ..slate import Slate


@dataclass
class Field:
    lineups: np.ndarray        # (n, 6) golfer indices
    ownership: np.ndarray      # realised, as a fraction of entries
    target: np.ndarray         # what it was fitted to
    acceptance: float          # share of sampled lineups that cleared the cap

    def __len__(self) -> int:
        return len(self.lineups)

    def scores(self, points: np.ndarray, chunk: int = 2048) -> np.ndarray:
        """(n_sims, n_lineups) DraftKings totals for the whole field.

        Chunked over lineups: the obvious one-liner materialises an
        (n_sims, n_lineups, 6) array, which for a 20,000-lineup field is an
        order of magnitude more memory than the result itself.
        """
        out = np.empty((points.shape[0], len(self.lineups)), dtype=np.float32)
        for lo in range(0, len(self.lineups), chunk):
            block = self.lineups[lo : lo + chunk]
            out[:, lo : lo + chunk] = points[:, block].sum(axis=2)
        return out


def _sample_lineups(
    weights: np.ndarray,
    salaries: np.ndarray,
    n_wanted: int,
    cfg: FieldConfig,
    rng: np.random.Generator,
    *,
    projection: np.ndarray | None = None,
    keep_best_of: int = 1,
) -> tuple[np.ndarray, float]:
    """Draw `n_wanted` cap-legal lineups by weighted sampling without replacement.

    Golfers are picked one at a time, and at every pick the ones that would
    strand the budget are removed from consideration first: too expensive to
    leave room for the cheapest remaining golfers, or too cheap to still
    reach the floor with the most expensive ones. That conditioning is the
    whole reason this is a sequential draw rather than a one-shot Gumbel
    top-k with a filter on the end.

    Filtering afterwards produces the same distribution in principle and is
    useless in practice at this event: entries spend better than 98% of the
    cap, and fewer than one in two hundred ownership-weighted draws lands in
    that window. Worse, the ones it throws away are not a random sample --
    a lineup containing the $14,400 golfer is far more likely to be rejected
    than one without him, so the survivors have him at a tenth of his real
    ownership, and the iterative fit that is supposed to correct that ends up
    fighting the sampler instead of the projection.

    `keep_best_of` models the part of the field that runs an optimizer: those
    entries are still built out of the golfers everyone is on, but they are
    not a random combination of them -- they are the best combination the
    entrant could find. Drawing k and keeping the highest-projecting one is a
    cheap stand-in that gets the effect right without solving 35,000 knapsack
    problems.
    """
    n_players = len(weights)
    need = n_wanted * keep_best_of
    log_w = np.log(np.clip(weights, 1e-12, None))
    floor = cfg.min_salary_used

    # Cheapest and most expensive r-golfer tails, ignoring who has already
    # been taken. That makes the test a relaxation -- it can admit a lineup
    # that turns out to be a dead end -- so the exact window is still checked
    # at the end. It clears the great majority of draws either way.
    ordered = np.sort(salaries)
    cheapest = np.concatenate([[0], np.cumsum(ordered)])
    priciest = np.concatenate([[0], np.cumsum(ordered[::-1])])

    out: list[np.ndarray] = []
    kept = tried = 0
    batch = max(8192, need)
    while kept < need:
        picks = np.empty((batch, ROSTER_SIZE), dtype=np.int64)
        avail = np.ones((batch, n_players), dtype=bool)
        spent = np.zeros(batch, dtype=np.int64)
        for slot in range(ROSTER_SIZE):
            rest = ROSTER_SIZE - slot - 1
            after = spent[:, None] + salaries[None, :]
            feasible = avail & (after + cheapest[rest] <= SALARY_CAP)
            feasible &= after + priciest[rest] >= floor
            # A relaxation can still corner itself; fall back to anything legal.
            stuck = ~feasible.any(axis=1)
            if stuck.any():
                feasible[stuck] = avail[stuck]
            keys = np.where(feasible, log_w + rng.gumbel(size=(batch, n_players)), -np.inf)
            chosen = keys.argmax(axis=1)
            picks[:, slot] = chosen
            spent += salaries[chosen]
            avail[np.arange(batch), chosen] = False

        ok = (spent <= SALARY_CAP) & (spent >= floor)
        tried += batch
        kept += int(ok.sum())
        out.append(picks[ok])
        if tried > 200 * need:
            raise RuntimeError(
                f"only {kept / tried:.2%} of sampled lineups land in "
                f"[{floor}, {SALARY_CAP}]; lower FieldConfig.min_salary_used"
            )

    lineups = np.concatenate(out)[:need]
    if keep_best_of > 1:
        if projection is None:
            raise ValueError("keep_best_of needs a projection to rank by")
        grouped = lineups.reshape(n_wanted, keep_best_of, ROSTER_SIZE)
        totals = projection[grouped].sum(axis=2)
        lineups = np.take_along_axis(
            grouped, totals.argmax(axis=1)[:, None, None], axis=1
        )[:, 0]
    return np.sort(lineups, axis=1), kept / tried


def _draw_mixture(
    weights: np.ndarray,
    slate: Slate,
    cfg: FieldConfig,
    rng: np.random.Generator,
    n: int,
    projection: np.ndarray | None,
) -> tuple[np.ndarray, float]:
    """One block of the field: part random entrants, part optimizer users."""
    n_sharp = int(round(n * cfg.sharp_share)) if projection is not None else 0
    blocks, accepts = [], []
    for count, best_of in ((n - n_sharp, 1), (n_sharp, cfg.sharp_pool)):
        if count <= 0:
            continue
        lineups, acc = _sample_lineups(
            weights, slate.salaries, count, cfg, rng,
            projection=projection, keep_best_of=best_of,
        )
        blocks.append(lineups)
        accepts.append(acc)
    return np.concatenate(blocks), float(np.mean(accepts))


def fit_weights(
    slate: Slate,
    target_ownership: np.ndarray,
    cfg: FieldConfig,
    rng: np.random.Generator,
    *,
    rounds: int = 14,
    probe: int = 10_000,
    projection: np.ndarray | None = None,
    step: float = 0.35,
    max_tilt: float = 40.0,
) -> np.ndarray:
    """Sampling weights whose *drawn* field reproduces the target ownership.

    Iterative proportional fitting: draw a field, see which golfers came in
    over or under, and push the weights the other way. Two things make the
    correction necessary, and both are properties of the field rather than of
    the sampler. The salary filter is not ownership-neutral -- six golfers
    inside a cap is a binding constraint, and it pushes lineups toward the
    cheap end. The optimizer-using share of the field is not neutral either,
    and pushes much harder: selecting the best of twelve draws by projected
    points systematically drops the golfers whose points per dollar are worst,
    which at this event means the most expensive ones.

    So the fit has to be run against the same mixture the field will actually
    be drawn from. Fitting against plain sampling and then drawing a mixture
    leaves Rory McIlroy at a third of his projected ownership.

    The update is heavily damped and the best iterate is kept rather than the
    last. Undamped, the fit oscillates: the salary cap makes ownership of the
    most expensive golfer a *joint* property of the whole lineup, so pushing
    his weight up overshoots, the correction overshoots back, and the last
    iterate is as likely to be a peak as a trough.
    """
    weights = np.clip(target_ownership, 1e-6, None).astype(float)
    weights /= weights.sum()
    base = weights.copy()
    best, best_error = weights.copy(), np.inf
    for _ in range(rounds):
        try:
            lineups, _ = _draw_mixture(weights, slate, cfg, rng, probe, projection)
        except RuntimeError:
            # The weights have wandered somewhere the salary cap cannot
            # follow. Whatever the best iterate was, it is better than this.
            break
        realised = np.bincount(lineups.ravel(), minlength=len(slate)) / len(lineups)
        error = float(np.abs(realised - target_ownership).max())
        if error < best_error:
            best, best_error = weights.copy(), error
        weights = weights * (target_ownership / np.clip(realised, 1e-4, None)) ** step
        # Without a leash the fit can chase an unreachable target -- a golfer
        # the salary structure will not let the field draft that often -- into
        # weights so extreme that nothing clears the cap at all.
        weights = np.clip(weights, base / max_tilt, base * max_tilt)
        weights /= weights.sum()
    return best


def build_field(
    slate: Slate,
    target_ownership: np.ndarray,
    cfg: FieldConfig,
    *,
    ownership_draws: np.ndarray | None = None,
    n_lineups: int | None = None,
    projection: np.ndarray | None = None,
) -> Field:
    """Sample an opponent field whose ownership matches `target_ownership`.

    If `ownership_draws` is given, the field is built in blocks -- one per
    draw -- so that the uncertainty in the ownership projection shows up as
    uncertainty in the *field*, which is where it belongs. Every block reuses
    the weights fitted to the mean projection, rescaled by that draw, so the
    salary-filter correction is paid for once.
    """
    rng = np.random.default_rng(cfg.seed)
    n = n_lineups or cfg.n_field_lineups or len(slate) * 100
    base = fit_weights(slate, target_ownership, cfg, rng, projection=projection)

    draws = np.atleast_2d(ownership_draws if ownership_draws is not None else target_ownership)
    per_draw = max(1, n // len(draws))
    blocks, accepts = [], []
    for draw in draws:
        weights = base * (np.clip(draw, 1e-6, None) / np.clip(target_ownership, 1e-6, None))
        weights /= weights.sum()
        lineups, acc = _draw_mixture(weights, slate, cfg, rng, per_draw, projection)
        blocks.append(lineups)
        accepts.append(acc)

    lineups = np.concatenate(blocks)
    realised = np.bincount(lineups.ravel(), minlength=len(slate)) / len(lineups)
    return Field(
        lineups=lineups,
        ownership=realised,
        target=target_ownership,
        acceptance=float(np.mean(accepts)),
    )
