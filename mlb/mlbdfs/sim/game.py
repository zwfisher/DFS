"""Base-out Markov game simulator, vectorized across simulations.

The performance idea that makes this usable on a laptop: rather than looping
over simulations and stepping one game at a time, every simulation of a game
advances through the same plate appearance together as numpy arrays. The
Python loop therefore runs about 200 times per game -- once per plate
appearance in the longest simulation -- instead of once per simulation. Sims
that have finished a half-inning early are masked out rather than compacted,
which keeps the inner loop branch-free.

What falls out of simulating actual game states, rather than projecting
players in isolation:

* Runs and RBI are emitted by the state machine. No context regression, no
  fudge factor for "hits in a good lineup".
* Teammates are correlated because they literally bat in the same innings.
  A five-stack goes off in exactly the simulations where the team bats
  around, which is the dependency that makes stacking profitable and the
  thing an independent-player model cannot see.
* The opposing pitcher is scored in the same pass, so pitcher-versus-opposing
  -hitter anticorrelation is automatic.

Approximations worth knowing about are marked ``NOTE`` in the code.
"""

from __future__ import annotations

import numpy as np

from ..config import (
    BASERUNNING,
    BB,
    DOUBLE,
    FIELD_OUT,
    HBP,
    HR,
    K,
    N_OUTCOMES,
    SINGLE,
    SIM,
    SimConfig,
    TRIPLE,
)
from ..projections.pitchers import draw_pitch_limits
from ..scoring import HITTER_PA_POINTS, PITCHER_EVENT_POINTS, POINTS_PER_OUT
from ..config import HITTER_SCORING, PITCHER_SCORING
from ..slate import SimGame

# Pitches thrown per plate appearance by outcome; averages to roughly 3.9.
PITCHES_BY_OUTCOME = np.array(
    [4.9, 3.3, 5.4, 3.2, 3.6, 3.7, 3.7, 3.6], dtype=np.float32
)

# A half-inning cannot loop forever; the cap is far beyond any realistic
# half-inning and only exists so a pathological rate vector cannot hang.
MAX_PA_PER_HALF = 40

RUN_PTS = np.float32(HITTER_SCORING.run)
RBI_PTS = np.float32(HITTER_SCORING.rbi)
SB_PTS = np.float32(HITTER_SCORING.stolen_base)


def _add_at(flat: np.ndarray, slot: np.ndarray, mask: np.ndarray, amount, sims, n_sims) -> None:
    """Accumulate ``amount`` into ``flat[slot, sim]`` for the masked sims.

    ``flat`` is a flattened ``(9, n_sims)`` accumulator. Each simulation
    appears at most once per call, so the indices are unique and plain fancy
    indexing is correct -- and several times faster than ``np.add.at``.
    """
    if not mask.any():
        return
    idx = slot[mask].astype(np.intp) * n_sims + sims[mask]
    if np.isscalar(amount):
        flat[idx] += amount
    else:
        flat[idx] += amount[mask]


OFFENSIVE_OUTCOMES = (SINGLE, DOUBLE, TRIPLE, HR, BB)


def _bin_offsets(n_bins: int) -> np.ndarray:
    """Equal-probability standard-normal quantile midpoints.

    Discretizing the shared shock into a handful of bins keeps the rate
    lookup table tiny -- a few hundred rows instead of one row per simulation
    -- while reproducing the distribution closely enough that the resulting
    correlations are indistinguishable from a continuous draw.
    """
    from scipy.stats import norm

    probs = (np.arange(n_bins) + 0.5) / n_bins
    return norm.ppf(probs)


def _tilt(rates: np.ndarray, mult: float) -> np.ndarray:
    """Scale offensive outcome rates by ``mult`` and renormalize.

    Strikeouts move the opposite way at half the magnitude: a pitcher who
    does not have his stuff both allows more contact damage and misses fewer
    bats, but the two do not move one for one.
    """
    m = np.ones(N_OUTCOMES, dtype=np.float64)
    for o in OFFENSIVE_OUTCOMES:
        m[o] = mult
    m[K] = mult ** -0.5
    out = np.asarray(rates, dtype=np.float64) * m[None, :]
    return out / out.sum(axis=1, keepdims=True)


def _build_cum_table(
    vs_starter: np.ndarray, vs_bullpen: np.ndarray, cfg: SimConfig
) -> np.ndarray:
    """Cumulative outcome probabilities for every shared-shock combination.

    Returns an array indexed by ``(env_bin * n_form + form_bin) * 18 +
    phase * 9 + slot``, where phase 0 is the starter and phase 1 the bullpen.
    For the default five environment bins and seven form bins that is 630
    rows -- small enough to stay in cache through the whole game.
    """
    env_z = _bin_offsets(cfg.n_env_bins)
    form_z = _bin_offsets(cfg.n_form_bins)

    blocks = []
    for ez in env_z:
        for fz in form_z:
            log_mult = ez * cfg.game_env_sd + fz * cfg.pitcher_form_sd
            starter = _tilt(vs_starter, float(np.exp(log_mult)))
            # The bullpen shares the environment shock in full but only a
            # damped share of the starter's form, since it is a different set
            # of arms.
            pen_log = ez * cfg.game_env_sd + fz * cfg.pitcher_form_sd * cfg.bullpen_form_damping
            bullpen = _tilt(vs_bullpen, float(np.exp(pen_log)))
            blocks.append(np.concatenate([starter, bullpen], axis=0))

    stacked = np.concatenate(blocks, axis=0).astype(np.float32)
    cum = np.cumsum(stacked, axis=1)
    cum[:, -1] = 1.0  # guard against floating point leaving a sliver above
    return cum


class GameResult:
    """Per-game simulation output, before it is folded into the slate matrix."""

    __slots__ = (
        "bat_pts", "sp_pts", "batter_idx", "starter_idx", "runs", "sp_outs", "pa_count",
    )

    def __init__(self, bat_pts, sp_pts, batter_idx, starter_idx, runs, sp_outs, pa_count):
        self.bat_pts = bat_pts  # (2, 9, n_sims)
        self.sp_pts = sp_pts  # (2, n_sims)
        self.batter_idx = batter_idx  # (2, 9)
        self.starter_idx = starter_idx  # (2,)
        self.runs = runs  # (2, n_sims)
        self.sp_outs = sp_outs  # (2, n_sims) -- outs recorded by each starter
        self.pa_count = pa_count  # (2, 9, n_sims) -- plate appearances by slot


def simulate_game(
    game: SimGame,
    n_sims: int,
    rng: np.random.Generator,
    cfg: SimConfig = SIM,
) -> GameResult:
    """Simulate one game ``n_sims`` times and return per-player point arrays."""
    sides = (game.away, game.home)
    sims = np.arange(n_sims, dtype=np.intp)

    cum = [_build_cum_table(s.vs_starter, s.vs_bullpen, cfg) for s in sides]
    sb_rate = [np.asarray(s.sb_rate, dtype=np.float32) for s in sides]

    # Shared shocks, drawn once per simulated game. The environment factor is
    # common to both lineups; each starter's form factor applies only to the
    # lineup he faces.
    env_bin = rng.integers(0, cfg.n_env_bins, size=n_sims)
    form_bin = rng.integers(0, cfg.n_form_bins, size=(2, n_sims))
    # Row offset into the rate table for each batting team, precomputed so
    # the inner loop only adds the phase and slot.
    tilt_base = np.stack(
        [
            ((env_bin * cfg.n_form_bins + form_bin[1 - t]) * 18).astype(np.int32)
            for t in (0, 1)
        ]
    )

    # Hitter points, indexed [team][slot, sim]; kept flat for scatter writes.
    bat_pts = np.zeros((2, 9, n_sims), dtype=np.float32)
    bat_flat = [bat_pts[0].reshape(-1), bat_pts[1].reshape(-1)]

    # Plate appearances by slot. Emergent rather than imposed, which makes it
    # a genuine check on the simulation: slot one should land near 4.7 and
    # slot nine near 4.0 without anything in the model saying so.
    pa_count = np.zeros((2, 9, n_sims), dtype=np.int16)
    pa_flat = [pa_count[0].reshape(-1), pa_count[1].reshape(-1)]

    # Starting pitcher accumulators, indexed [defending team][sim].
    sp_outs = np.zeros((2, n_sims), dtype=np.int16)
    sp_k = np.zeros((2, n_sims), dtype=np.int16)
    sp_er = np.zeros((2, n_sims), dtype=np.int16)
    sp_h = np.zeros((2, n_sims), dtype=np.int16)
    sp_bb = np.zeros((2, n_sims), dtype=np.int16)
    sp_hbp = np.zeros((2, n_sims), dtype=np.int16)
    sp_pitches = np.zeros((2, n_sims), dtype=np.float32)
    sp_active = np.ones((2, n_sims), dtype=bool)
    sp_lead_at_exit = np.zeros((2, n_sims), dtype=bool)
    pitch_limit = np.stack([draw_pitch_limits(n_sims, rng) for _ in range(2)])

    score = np.zeros((2, n_sims), dtype=np.int16)
    bat_ptr = np.zeros((2, n_sims), dtype=np.int8)  # next batter slot, 0-8
    game_over = np.zeros(n_sims, dtype=bool)

    max_inning = 9 + cfg.max_extra_innings

    for inning in range(1, max_inning + 1):
        for half in (0, 1):  # 0 = top (away bats), 1 = bottom (home bats)
            t = half  # batting team index
            d = 1 - half  # defending team index

            active = ~game_over
            if inning >= 9 and half == 1:
                # The home team does not bat in the bottom of the ninth or
                # later if it is already ahead.
                active = active & (score[1] <= score[0])
            if not active.any():
                continue

            _simulate_half(
                inning=inning,
                half=half,
                t=t,
                d=d,
                active=active,
                n_sims=n_sims,
                sims=sims,
                rng=rng,
                cfg=cfg,
                cum=cum[t],
                tilt_base=tilt_base[t],
                sb_rate=sb_rate[t],
                bat_flat=bat_flat[t],
                pa_flat=pa_flat[t],
                bat_ptr=bat_ptr,
                score=score,
                sp_outs=sp_outs,
                sp_k=sp_k,
                sp_er=sp_er,
                sp_h=sp_h,
                sp_bb=sp_bb,
                sp_hbp=sp_hbp,
                sp_pitches=sp_pitches,
                sp_active=sp_active,
                sp_lead_at_exit=sp_lead_at_exit,
                pitch_limit=pitch_limit,
            )

            # Between-innings hook: the ordinary way a starter's day ends. The
            # manager is deciding whether to send him back out, so the
            # comparison is against the pitch count he would finish the next
            # inning on, not the one he is at now. Without this lookahead a
            # starter always completes the inning that crosses his limit and
            # the simulation runs starters roughly two thirds of an inning
            # deeper than they actually go.
            projected = sp_pitches[d] + cfg.next_inning_pitches
            pull = sp_active[d] & (projected >= pitch_limit[d])
            if pull.any():
                sp_lead_at_exit[d] = np.where(
                    pull, score[d] > score[1 - d], sp_lead_at_exit[d]
                )
                sp_active[d] = sp_active[d] & ~pull

        if inning >= 9:
            game_over = game_over | (score[0] != score[1])
            if game_over.all():
                break

    # Games still tied after the extra-innings cap are left as ties. This
    # affects a fraction of a percent of simulations and only the win bonus.
    sp_pts = _score_starters(
        sp_outs, sp_k, sp_er, sp_h, sp_bb, sp_hbp, sp_active, sp_lead_at_exit, score, cfg
    )

    return GameResult(
        bat_pts=bat_pts,
        sp_pts=sp_pts,
        batter_idx=np.stack([sides[0].batter_idx, sides[1].batter_idx]),
        starter_idx=np.array([sides[0].starter_idx, sides[1].starter_idx]),
        runs=score.astype(np.int16),
        sp_outs=sp_outs.copy(),
        pa_count=pa_count,
    )


def _simulate_half(
    *,
    inning,
    half,
    t,
    d,
    active,
    n_sims,
    sims,
    rng,
    cfg,
    cum,
    tilt_base,
    sb_rate,
    bat_flat,
    pa_flat,
    bat_ptr,
    score,
    sp_outs,
    sp_k,
    sp_er,
    sp_h,
    sp_bb,
    sp_hbp,
    sp_pitches,
    sp_active,
    sp_lead_at_exit,
    pitch_limit,
) -> None:
    """Run one half-inning to three outs for every active simulation."""
    br = BASERUNNING

    outs = np.zeros(n_sims, dtype=np.int8)
    r1 = np.full(n_sims, -1, dtype=np.int8)
    r2 = np.full(n_sims, -1, dtype=np.int8)
    r3 = np.full(n_sims, -1, dtype=np.int8)

    if inning > 9 and cfg.extras_ghost_runner:
        # The automatic runner is the batter who made the last out, i.e. the
        # slot immediately before the one due up.
        ghost = (bat_ptr[t].astype(np.int16) - 1) % 9
        r2 = np.where(active, ghost.astype(np.int8), r2).astype(np.int8)

    batting = active.copy()
    walkoff_possible = inning >= 9 and half == 1

    for _ in range(MAX_PA_PER_HALF):
        if not batting.any():
            break

        # ---- stolen base attempt before the pitch ------------------------
        steal_chance = batting & (r1 >= 0) & (r2 < 0)
        if steal_chance.any():
            slot1 = np.where(r1 >= 0, r1, 0)
            rate = sb_rate[slot1]
            attempt = steal_chance & (rng.random(n_sims) < rate)
            if attempt.any():
                success = attempt & (rng.random(n_sims) < br.sb_success)
                caught = attempt & ~success

                _add_at(bat_flat, r1, success, SB_PTS, sims, n_sims)
                r2 = np.where(success, r1, r2).astype(np.int8)
                r1 = np.where(attempt, np.int8(-1), r1).astype(np.int8)

                if caught.any():
                    outs = (outs + caught).astype(np.int8)
                    live_sp = caught & sp_active[d]
                    sp_outs[d] += live_sp
                    batting = batting & (outs < 3)
                    if not batting.any():
                        break

        # ---- wild pitch, passed ball, balk -------------------------------
        # Everyone moves up one base and a runner on third scores. The run is
        # credited to the runner but no RBI is credited to the batter, which
        # matches official scoring.
        runners_on = batting & ((r1 >= 0) | (r2 >= 0) | (r3 >= 0))
        wp = runners_on & (rng.random(n_sims) < br.wild_pitch_rate)
        if wp.any():
            wp_scores = wp & (r3 >= 0)
            _add_at(bat_flat, r3, wp_scores, RUN_PTS, sims, n_sims)
            wp_runs = wp_scores.astype(np.int16)
            score[t] += wp_runs
            sp_er[d] += np.where(sp_active[d], wp_runs, 0).astype(np.int16)

            r3 = np.where(wp, np.where(r2 >= 0, r2, np.int8(-1)), r3).astype(np.int8)
            r2 = np.where(wp, np.where(r1 >= 0, r1, np.int8(-1)), r2).astype(np.int8)
            r1 = np.where(wp, np.int8(-1), r1).astype(np.int8)

        # ---- draw the plate appearance outcome ---------------------------
        phase = (~sp_active[d]).astype(np.int32)  # 0 starter, 1 bullpen
        slot = bat_ptr[t].astype(np.int32)
        row = tilt_base + phase * 9 + slot
        cum_row = cum[row]  # (n_sims, n_outcomes)

        u = rng.random(n_sims, dtype=np.float32)
        outcome = (u[:, None] >= cum_row).sum(axis=1).astype(np.int8)

        is_k = batting & (outcome == K)
        is_fo = batting & (outcome == FIELD_OUT)
        is_bb = batting & (outcome == BB)
        is_hbp = batting & (outcome == HBP)
        is_1b = batting & (outcome == SINGLE)
        is_2b = batting & (outcome == DOUBLE)
        is_3b = batting & (outcome == TRIPLE)
        is_hr = batting & (outcome == HR)

        ua = rng.random(n_sims, dtype=np.float32)
        ub = rng.random(n_sims, dtype=np.float32)

        # ---- resolve base advancement ------------------------------------
        gidp = is_fo & (r1 >= 0) & (outs < 2) & (ua < br.gidp_rate)
        outs_added = (is_k | is_fo).astype(np.int8) + gidp.astype(np.int8)
        ends_inning = (outs + outs_added) >= 3

        # Ball in play out, inning continues: runners can move up.
        adv_ok = is_fo & ~gidp & ~ends_inning
        sac = adv_ok & (r3 >= 0) & (ua < br.third_scores_on_out)
        third_free = (r3 < 0) | sac
        r2_adv = adv_ok & (r2 >= 0) & third_free & (ub < br.second_to_third_on_out)
        r1_adv = r2_adv & (r1 >= 0)

        # Single.
        s1_r2_scores = is_1b & (r2 >= 0) & (ua < br.second_scores_on_single)
        s1_r2_to3 = is_1b & (r2 >= 0) & ~s1_r2_scores
        s1_r1_to3 = is_1b & (r1 >= 0) & ~s1_r2_to3 & (ub < br.first_to_third_on_single)
        s1_r1_to2 = is_1b & (r1 >= 0) & ~s1_r1_to3

        # Double.
        d_r1_scores = is_2b & (r1 >= 0) & (ua < br.first_scores_on_double)
        d_r1_to3 = is_2b & (r1 >= 0) & ~d_r1_scores

        # Walk or hit by pitch: only forced runners move.
        forced = is_bb | is_hbp
        f_move1 = forced & (r1 >= 0)
        f_move2 = forced & (r1 >= 0) & (r2 >= 0)
        f_score3 = forced & (r1 >= 0) & (r2 >= 0) & (r3 >= 0)

        # ---- who scored ---------------------------------------------------
        r3_scores = (
            (is_1b & (r3 >= 0))
            | (is_2b & (r3 >= 0))
            | (is_3b & (r3 >= 0))
            | (is_hr & (r3 >= 0))
            | f_score3
            | sac
        )
        r2_scores = (
            s1_r2_scores | (is_2b & (r2 >= 0)) | (is_3b & (r2 >= 0)) | (is_hr & (r2 >= 0))
        )
        r1_scores = d_r1_scores | (is_3b & (r1 >= 0)) | (is_hr & (r1 >= 0))
        batter_scores = is_hr

        _add_at(bat_flat, r3, r3_scores, RUN_PTS, sims, n_sims)
        _add_at(bat_flat, r2, r2_scores, RUN_PTS, sims, n_sims)
        _add_at(bat_flat, r1, r1_scores, RUN_PTS, sims, n_sims)
        _add_at(bat_flat, slot.astype(np.int8), batter_scores, RUN_PTS, sims, n_sims)

        runs = (
            r3_scores.astype(np.int8)
            + r2_scores.astype(np.int8)
            + r1_scores.astype(np.int8)
            + batter_scores.astype(np.int8)
        )

        # ---- hitter points ------------------------------------------------
        pa_pts = HITTER_PA_POINTS[outcome]
        slot8 = slot.astype(np.int8)
        _add_at(bat_flat, slot8, batting, pa_pts, sims, n_sims)
        _add_at(pa_flat, slot8, batting, 1, sims, n_sims)
        has_rbi = batting & (runs > 0)
        _add_at(
            bat_flat,
            slot.astype(np.int8),
            has_rbi,
            runs.astype(np.float32) * RBI_PTS,
            sims,
            n_sims,
        )

        # ---- new base state -------------------------------------------------
        new_r1 = np.where(
            is_hr | is_3b | is_2b,
            np.int8(-1),
            np.where(
                is_1b | forced,
                slot.astype(np.int8),
                np.where(r1_adv | gidp, np.int8(-1), r1),
            ),
        ).astype(np.int8)

        new_r2 = np.where(
            is_hr | is_3b,
            np.int8(-1),
            np.where(
                is_2b,
                slot.astype(np.int8),
                np.where(
                    is_1b,
                    np.where(s1_r1_to2, r1, np.int8(-1)),
                    np.where(
                        forced,
                        np.where(f_move1, r1, r2),
                        np.where(r1_adv, r1, np.where(r2_adv, np.int8(-1), r2)),
                    ),
                ),
            ),
        ).astype(np.int8)

        new_r3 = np.where(
            is_hr,
            np.int8(-1),
            np.where(
                is_3b,
                slot.astype(np.int8),
                np.where(
                    is_2b,
                    np.where(d_r1_to3, r1, np.int8(-1)),
                    np.where(
                        is_1b,
                        np.where(s1_r2_to3, r2, np.where(s1_r1_to3, r1, np.int8(-1))),
                        np.where(
                            forced,
                            np.where(f_move2, r2, r3),
                            np.where(r2_adv, r2, np.where(sac, np.int8(-1), r3)),
                        ),
                    ),
                ),
            ),
        ).astype(np.int8)

        r1 = np.where(batting, new_r1, r1).astype(np.int8)
        r2 = np.where(batting, new_r2, r2).astype(np.int8)
        r3 = np.where(batting, new_r3, r3).astype(np.int8)

        # ---- pitcher accounting ----------------------------------------------
        live = batting & sp_active[d]
        sp_pitches[d] += np.where(live, PITCHES_BY_OUTCOME[outcome], 0.0)
        sp_outs[d] += np.where(live, outs_added, 0).astype(np.int16)
        sp_k[d] += (live & is_k).astype(np.int16)
        sp_bb[d] += (live & is_bb).astype(np.int16)
        sp_hbp[d] += (live & is_hbp).astype(np.int16)
        sp_h[d] += (live & (is_1b | is_2b | is_3b | is_hr)).astype(np.int16)
        # NOTE: runs are charged to whoever is pitching when they cross the
        # plate. Real scoring charges inherited runners to the pitcher who put
        # them on, which slightly flatters a starter pulled with men aboard.
        sp_er[d] += np.where(live, runs, 0).astype(np.int16)

        # ---- advance state ----------------------------------------------------
        outs = (outs + np.where(batting, outs_added, 0)).astype(np.int8)
        score[t] += np.where(batting, runs, 0).astype(np.int16)
        bat_ptr[t] = np.where(batting, (bat_ptr[t] + 1) % 9, bat_ptr[t]).astype(np.int8)

        batting = batting & (outs < 3)

        # Blow-up hook: the manager stops managing the pitch count.
        blowup = sp_active[d] & (sp_er[d] >= cfg.hook_runs_allowed)
        if blowup.any():
            sp_lead_at_exit[d] = np.where(blowup, score[d] > score[1 - d], sp_lead_at_exit[d])
            sp_active[d] = sp_active[d] & ~blowup

        if walkoff_possible:
            batting = batting & ~(score[1] > score[0])


def _score_starters(
    sp_outs, sp_k, sp_er, sp_h, sp_bb, sp_hbp, sp_active, sp_lead_at_exit, score, cfg
) -> np.ndarray:
    """Convert starter accumulators into DraftKings points."""
    ps = PITCHER_SCORING
    pts = np.zeros(sp_outs.shape, dtype=np.float32)

    for d in (0, 1):
        team_won = score[d] > score[1 - d]
        # A starter still in at the final out was leading then if his team led.
        lead_at_exit = np.where(sp_active[d], score[d] > score[1 - d], sp_lead_at_exit[d])
        win = (sp_outs[d] >= int(round(cfg.win_min_innings * 3))) & lead_at_exit & team_won

        complete_game = sp_active[d] & (sp_outs[d] >= 24)
        shutout = complete_game & (sp_er[d] == 0)
        no_hitter = complete_game & (sp_h[d] == 0)

        pts[d] = (
            sp_outs[d] * POINTS_PER_OUT
            + sp_k[d] * ps.strikeout
            + sp_er[d] * ps.earned_run
            + sp_h[d] * ps.hit_against
            + sp_bb[d] * ps.walk_against
            + sp_hbp[d] * ps.hit_batsman
            + win * ps.win
            + complete_game * ps.complete_game
            + shutout * ps.complete_game_shutout
            + no_hitter * ps.no_hitter
        )

    return pts
