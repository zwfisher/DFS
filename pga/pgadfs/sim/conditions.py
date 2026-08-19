"""What the course was like when each golfer played it.

A 50-man no-cut field goes off one tee in twosomes eleven minutes apart, so
the first group starts at 9:03 and the last at 1:55 and finishes near 6:15.
That is not a two-wave split, it is a five-hour ramp, and every golfer sits
somewhere different on it.

What changes along that ramp at Bellerive this week is not wind -- the
forecast is 2 to 6 mph for four straight days, which is inside the range
where wind does nothing measurable to scoring. It is the surface. The
morning rounds go out at 80-97% humidity onto greens that held moisture
overnight; by mid-afternoon it is 84 degrees at 62% and the greens are
firmer, faster and less forgiving of a long approach. That is the whole
mechanism, and it is worth modelling as a continuous function of tee time
rather than a binary wave flag, because on this schedule the binary flag
throws away most of the signal.

On the weekend the ramp reverses its meaning. Rounds three and four are
paired off the leaderboard with the leaders out last, so whoever is leading
plays in the worst of the day's conditions -- a small, real drag on the top
of the board that only exists because the tee sheet is endogenous.
"""

from __future__ import annotations

from dataclasses import dataclass
from datetime import datetime, timedelta

import numpy as np

from ..config import ConditionsConfig
from ..data.weather import Forecast


@dataclass(frozen=True)
class RoundConditions:
    """Per-tee-slot scoring adjustment for one round, in strokes.

    `by_slot[i]` is what the golfer in the i-th tee slot gives up, with slot
    0 the earliest. Centred on the field, because the overall difficulty of
    the day is the course calibration's job, not this layer's.
    """

    day: str
    tee_times: tuple[datetime, ...]
    by_slot: np.ndarray
    wind: np.ndarray
    drying: np.ndarray
    precip: np.ndarray

    @property
    def spread(self) -> float:
        return float(self.by_slot.max() - self.by_slot.min())


def round_conditions(
    forecast: Forecast,
    day: str,
    tee_times: list[datetime],
    cfg: ConditionsConfig,
) -> RoundConditions:
    """Turn a day's tee sheet into a per-slot scoring adjustment."""
    windows = [forecast.window(t, cfg.round_hours) for t in tee_times]
    wind = np.array([w.wind for w in windows])
    drying = np.array([w.drying for w in windows])
    precip = np.array([w.precip_probability / 100.0 for w in windows])

    wind_term = cfg.wind_coef * np.maximum(wind - cfg.wind_threshold, 0.0)
    firmness_term = (cfg.firmness_spread / cfg.firmness_reference) * (drying - drying.mean())
    rain_term = cfg.rain_coef * (precip - precip.mean())

    total = cfg.scale * (wind_term - wind_term.mean() + firmness_term + rain_term)
    return RoundConditions(
        day=day,
        tee_times=tuple(tee_times),
        by_slot=total,
        wind=wind,
        drying=drying,
        precip=precip,
    )


def tee_sheet(
    first_tee: datetime, n_players: int, interval_minutes: float, group_size: int = 2
) -> list[datetime]:
    """Tee times for a field going off one tee, earliest first."""
    return [
        first_tee + timedelta(minutes=interval_minutes * (i // group_size))
        for i in range(n_players)
    ]


@dataclass
class ConditionsSchedule:
    """Every round's conditions, plus who is where on the tee sheet.

    Rounds one and two are known in advance: the second round reverses the
    first, which is what a two-round rotation off a single tee does and what
    DataGolf's own early/late flag encodes. Rounds three and four are not
    known -- they depend on the leaderboard -- so those are resolved inside
    the simulation, per simulation.
    """

    rounds: tuple[RoundConditions, ...]
    slot_of_player: np.ndarray      # (n_players,) tee slot in round 1, 0 = earliest
    leaderboard_rounds: tuple[int, ...] = (2, 3)     # 0-indexed rounds paired off the board

    def fixed_adjustment(self, round_index: int) -> np.ndarray | None:
        """Per-golfer strokes for a round whose tee sheet is already known.

        Returns a flat zero if the schedule is shorter than the tournament,
        rather than failing: a missing forecast day should cost the model its
        conditions, not the run.
        """
        if round_index >= len(self.rounds):
            return np.zeros(len(self.slot_of_player))
        if round_index in self.leaderboard_rounds:
            return None
        slots = self.slot_of_player
        if round_index == 1:
            slots = len(slots) - 1 - slots
        return self.rounds[round_index].by_slot[slots]

    def slot_adjustment(self, round_index: int) -> np.ndarray:
        """Strokes by tee slot, for a round drawn off the leaderboard."""
        if round_index >= len(self.rounds):
            return np.zeros(len(self.slot_of_player))
        return self.rounds[round_index].by_slot


def build_schedule(
    forecast: Forecast,
    days: list[str],
    first_tee: list[datetime],
    slot_of_player: np.ndarray,
    cfg: ConditionsConfig,
) -> ConditionsSchedule:
    n = len(slot_of_player)
    rounds = []
    for day, tee in zip(days, first_tee):
        sheet = tee_sheet(tee, n, cfg.tee_interval_minutes, cfg.group_size)
        rounds.append(round_conditions(forecast, day, sheet, cfg))
    return ConditionsSchedule(rounds=tuple(rounds), slot_of_player=np.asarray(slot_of_player))
