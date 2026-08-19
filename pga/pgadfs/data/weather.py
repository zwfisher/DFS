"""Hourly weather for the course, from the National Weather Service.

DataGolf publishes an hourly forecast on its fantasy page, but only for the
first round -- and a four-round no-cut event needs all four. The NWS
gridpoint API covers a week, is free, needs no key, and for a course in
Missouri it is the authoritative source rather than a reseller of one.

What the simulator wants out of it is not the forecast but the *conditions a
particular golfer plays in*: the average wind, temperature and humidity over
the four and a quarter hours after his tee time. Everything here exists to
answer that question.
"""

from __future__ import annotations

import json
from dataclasses import dataclass
from datetime import datetime, timedelta

from .cache import fetch, load_fixture, save_fixture

POINTS_URL = "https://api.weather.gov/points/{lat},{lon}"

# Bellerive Country Club, Town and Country, Missouri.
BELLERIVE_LATLON = (38.627, -90.452)

# How long a twosome takes to play eighteen holes. A 50-player field in
# twosomes off one tee moves quickly; a full-field threesome week is nearer
# five hours and the number matters, because it sets how much of the day's
# drying each group is exposed to.
ROUND_HOURS = 4.25


@dataclass(frozen=True)
class Hour:
    time: datetime
    temperature: float        # Fahrenheit
    wind: float               # mph, sustained
    wind_direction: str
    humidity: float           # percent
    precip_probability: float # percent
    forecast: str


@dataclass(frozen=True)
class Conditions:
    """What a golfer actually played in, averaged over his round."""

    wind: float
    temperature: float
    humidity: float
    precip_probability: float
    hours: int

    @property
    def drying(self) -> float:
        """A crude index of how hard the course is drying out, in [0, 1]-ish.

        Firm, fast greens are the main thing separating a dawn tee time from
        a mid-afternoon one on a calm week, and firmness tracks how much heat
        and how much dry air the surface has seen. Warm and dry scores high;
        cool and saturated scores low. It is normalised against the day, not
        in absolute terms, in `Forecast.drying_index`.
        """
        return self.temperature * (1.0 - self.humidity / 100.0)


@dataclass
class Forecast:
    """An hourly series, indexed by local time."""

    hours: tuple[Hour, ...]
    location: str = ""

    def __len__(self) -> int:
        return len(self.hours)

    def days(self) -> list[str]:
        return sorted({h.time.date().isoformat() for h in self.hours})

    def window(self, start: datetime, length: float = ROUND_HOURS) -> Conditions:
        """Average conditions over `length` hours from `start`."""
        end = start + timedelta(hours=length)
        chosen = [h for h in self.hours if start <= h.time < end]
        if not chosen:
            nearest = min(self.hours, key=lambda h: abs((h.time - start).total_seconds()))
            chosen = [nearest]
        n = len(chosen)
        return Conditions(
            wind=sum(h.wind for h in chosen) / n,
            temperature=sum(h.temperature for h in chosen) / n,
            humidity=sum(h.humidity for h in chosen) / n,
            precip_probability=sum(h.precip_probability for h in chosen) / n,
            hours=n,
        )

    def daylight(self, day: str) -> tuple[datetime, datetime]:
        """First and last forecast hour of a day's golfing window."""
        on_day = [h for h in self.hours if h.time.date().isoformat() == day and 6 <= h.time.hour <= 20]
        if not on_day:
            raise KeyError(f"no forecast for {day}")
        return on_day[0].time, on_day[-1].time


def _mph(text: str) -> float:
    """NWS reports wind as '3 mph' or '5 to 10 mph'."""
    parts = [p for p in text.replace("mph", "").split() if p.isdigit()]
    if not parts:
        return 0.0
    return sum(float(p) for p in parts) / len(parts)


def parse_nws(payload: dict) -> Forecast:
    hours = []
    for period in payload["properties"]["periods"]:
        pop = (period.get("probabilityOfPrecipitation") or {}).get("value")
        humidity = (period.get("relativeHumidity") or {}).get("value")
        hours.append(
            Hour(
                time=datetime.fromisoformat(period["startTime"]).replace(tzinfo=None),
                temperature=float(period["temperature"]),
                wind=_mph(str(period.get("windSpeed", ""))),
                wind_direction=str(period.get("windDirection") or ""),
                humidity=float(humidity if humidity is not None else 60.0),
                precip_probability=float(pop if pop is not None else 0.0),
                forecast=str(period.get("shortForecast") or ""),
            )
        )
    return Forecast(hours=tuple(sorted(hours, key=lambda h: h.time)))


FIXTURE = "nws_forecast.json"


def load_forecast(
    lat: float = BELLERIVE_LATLON[0],
    lon: float = BELLERIVE_LATLON[1],
    *,
    offline: bool = False,
    max_age: float = 3600.0,
) -> Forecast:
    if offline:
        return parse_nws(load_fixture(FIXTURE))  # type: ignore[arg-type]
    return parse_nws(_fetch_nws(lat, lon, max_age=max_age))


def _fetch_nws(lat: float, lon: float, *, max_age: float) -> dict:
    points = json.loads(fetch(POINTS_URL.format(lat=lat, lon=lon), "nws_points.json", max_age=86400))
    url = points["properties"]["forecastHourly"]
    return json.loads(fetch(url, "nws_hourly.json", max_age=max_age))


def snapshot(lat: float = BELLERIVE_LATLON[0], lon: float = BELLERIVE_LATLON[1]) -> None:
    save_fixture(FIXTURE, _fetch_nws(lat, lon, max_age=0.0))
