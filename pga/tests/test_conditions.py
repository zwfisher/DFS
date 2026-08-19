from dataclasses import replace
from datetime import datetime

import numpy as np
import pytest

from pgadfs.config import BELLERIVE, ConditionsConfig, SimConfig
from pgadfs.data import weather
from pgadfs.sim.conditions import build_schedule, round_conditions, tee_sheet
from pgadfs.sim.engine import simulate
from pgadfs.sim.holes import HoleModel
from pgadfs.slate import Golfer, Slate

DAYS = ["2026-08-20", "2026-08-21", "2026-08-22", "2026-08-23"]


def flat_field(n=50):
    golfers = tuple(
        Golfer(dk_id=i, name=f"g{i}", salary=8000, skill=0.0, course_fit=0.0, wave=0,
               ownership=600.0 / n, tee_slot=i // 2)
        for i in range(n)
    )
    return Slate(event="t", course="t", golfers=golfers)


def schedule_for(n=50, cfg=None):
    cfg = cfg or ConditionsConfig()
    forecast = weather.load_forecast(offline=True)
    first = [datetime.fromisoformat(f"{DAYS[0]}T09:03"), datetime.fromisoformat(f"{DAYS[1]}T09:03"),
             datetime.fromisoformat(f"{DAYS[2]}T10:30"), datetime.fromisoformat(f"{DAYS[3]}T10:30")]
    return build_schedule(forecast, DAYS, first, np.arange(n), cfg)


# -- the forecast -------------------------------------------------------------

@pytest.mark.parametrize("text,expected", [("3 mph", 3.0), ("5 to 10 mph", 7.5), ("", 0.0), ("Calm", 0.0)])
def test_nws_wind_strings(text, expected):
    assert weather._mph(text) == expected


def test_forecast_covers_all_four_rounds():
    f = weather.load_forecast(offline=True)
    assert set(DAYS) <= set(f.days())
    for day in DAYS:
        first, last = f.daylight(day)
        assert first.hour <= 7 and last.hour >= 19


def test_window_averages_over_the_round():
    f = weather.load_forecast(offline=True)
    start = datetime.fromisoformat("2026-08-20T09:00")
    c = f.window(start, length=4.0)
    assert c.hours == 4
    hours = [h for h in f.hours if start <= h.time < start.replace(hour=13)]
    assert c.temperature == pytest.approx(np.mean([h.temperature for h in hours]))
    assert c.wind == pytest.approx(np.mean([h.wind for h in hours]))


def test_drying_rises_through_the_day():
    f = weather.load_forecast(offline=True)
    morning = f.window(datetime.fromisoformat("2026-08-20T09:00"))
    afternoon = f.window(datetime.fromisoformat("2026-08-20T13:30"))
    assert afternoon.drying > morning.drying
    assert afternoon.humidity < morning.humidity


# -- conditions ---------------------------------------------------------------

def test_tee_sheet_puts_a_group_off_together():
    sheet = tee_sheet(datetime.fromisoformat("2026-08-20T09:03"), 6, 11.0, group_size=2)
    assert sheet[0] == sheet[1]
    assert (sheet[2] - sheet[0]).total_seconds() == 11 * 60
    assert (sheet[4] - sheet[0]).total_seconds() == 22 * 60


def test_conditions_are_centred_on_the_field():
    f = weather.load_forecast(offline=True)
    sheet = tee_sheet(datetime.fromisoformat("2026-08-20T09:03"), 50, 11.0)
    rc = round_conditions(f, DAYS[0], sheet, ConditionsConfig())
    assert rc.by_slot.mean() == pytest.approx(0.0, abs=1e-12)
    # Later is firmer is harder.
    assert rc.by_slot[0] < rc.by_slot[-1]
    assert 0.05 < rc.spread < 1.0


def test_calm_week_means_wind_contributes_nothing():
    f = weather.load_forecast(offline=True)
    sheet = tee_sheet(datetime.fromisoformat("2026-08-20T09:03"), 50, 11.0)
    cfg = ConditionsConfig()
    windless = round_conditions(f, DAYS[0], sheet, replace(cfg, wind_coef=0.0))
    normal = round_conditions(f, DAYS[0], sheet, cfg)
    assert normal.wind.max() < cfg.wind_threshold        # nothing is above the threshold
    assert np.allclose(windless.by_slot, normal.by_slot)


def test_scale_zero_turns_the_layer_off():
    f = weather.load_forecast(offline=True)
    sheet = tee_sheet(datetime.fromisoformat("2026-08-20T09:03"), 50, 11.0)
    rc = round_conditions(f, DAYS[0], sheet, ConditionsConfig(scale=0.0))
    assert np.allclose(rc.by_slot, 0.0)


def test_round_two_reverses_round_one():
    sched = schedule_for()
    r1 = sched.fixed_adjustment(0)
    r2 = sched.fixed_adjustment(1)
    assert r1 is not None and r2 is not None
    assert np.allclose(r2, sched.rounds[1].by_slot[::-1])
    # so the two rounds mostly cancel: what is left is only the difference
    # between Thursday's drying curve and Friday's, not the curve itself
    net = np.abs(r1 + r2).max()
    assert net < 0.10
    assert net < 0.4 * sched.rounds[0].spread


def test_weekend_rounds_are_left_to_the_leaderboard():
    sched = schedule_for()
    assert sched.fixed_adjustment(2) is None
    assert sched.fixed_adjustment(3) is None
    assert sched.slot_adjustment(2).shape == (50,)


# -- what it does to the simulation -------------------------------------------

def test_the_draw_is_worth_a_fifth_of_a_stroke_over_one_round():
    slate = flat_field()
    sched = schedule_for()
    cfg = SimConfig(n_sims=6000, seed=3, rounds=1)
    strokes = simulate(slate, cfg, HoleModel(BELLERIVE, 3.19), sched).strokes.mean(axis=0)
    early, late = strokes[:10].mean(), strokes[-10:].mean()
    assert 0.10 < late - early < 0.45


def test_over_four_rounds_the_draw_cancels():
    slate = flat_field()
    sched = schedule_for()
    cfg = SimConfig(n_sims=6000, seed=3)
    strokes = simulate(slate, cfg, HoleModel(BELLERIVE, 3.19), sched).strokes.mean(axis=0)
    assert abs(strokes[-10:].mean() - strokes[:10].mean()) < 0.15


def test_leading_after_two_rounds_means_the_later_tee_time():
    # Leaders go last, so on a day that firms up they give a little back.
    slate = flat_field(n=20)
    sched = schedule_for(n=20)
    cfg = SimConfig(n_sims=4000, seed=9)
    sim = simulate(slate, cfg, HoleModel(BELLERIVE, 3.19), sched)
    assert sched.slot_adjustment(2)[0] < sched.slot_adjustment(2)[-1]
    assert sim.strokes.shape == (4000, 20)


def test_a_windy_week_would_matter():
    # Same machinery, a forecast with real wind in it: the layer has to move.
    f = weather.load_forecast(offline=True)
    windy = weather.Forecast(hours=tuple(
        weather.Hour(h.time, h.temperature, h.wind + 18.0 * (h.time.hour >= 12),
                     h.wind_direction, h.humidity, h.precip_probability, h.forecast)
        for h in f.hours
    ))
    sheet = tee_sheet(datetime.fromisoformat("2026-08-20T09:03"), 50, 11.0)
    calm = round_conditions(f, DAYS[0], sheet, ConditionsConfig())
    gale = round_conditions(windy, DAYS[0], sheet, ConditionsConfig())
    assert gale.spread > calm.spread * 2
