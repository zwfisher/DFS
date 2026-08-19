"""DataGolf, read off the free pages.

DataGolf's API needs a paid key, but several of the site's public tools ship
their whole payload inside the HTML as `something = JSON.parse('...')`, and
that payload is not the paywalled view -- it is the full table with the
subscriber-only *columns* blanked. In practice that means:

  datagolf-rankings      true-skill estimates (SG/round) for 500 players
  course-fit-tool        per-player fit for the specific course, in strokes
  fantasy-projections    projected DK ownership, score SD, tee waves, weather
  betting-tool-finish    DataGolf model *and* sportsbook win/top-N prices

On fantasy-projections the projected-points and name columns are replaced by
"Peeking Stop" placeholders below the top five, but `dk_id` survives on every
row and joins straight to DraftKings' `draftableId`. That is the whole trick:
the projections are the part this package builds itself, and the parts that
are genuinely hard to source independently -- ownership and market prices --
come through intact.

None of this is scraping around a paywall: it is the data the pages serve to
an anonymous browser. It is also fragile by nature, so every loader caches
its raw payload and can be pinned to a fixture.
"""

from __future__ import annotations

import json
import re
import unicodedata
from dataclasses import dataclass

from .cache import fetch, load_fixture, save_fixture

BASE = "https://datagolf.com"
PAGES = {
    "rankings": "/datagolf-rankings",
    "course_fit": "/course-fit-tool",
    "fantasy": "/fantasy-projections",
    "finish_odds": "/betting-tool-finish",
}

# A JS single-quoted string literal: any run of non-quote, non-backslash
# characters, or a backslash escape.
_JSON_PARSE = re.compile(r"(\w+)\s*=\s*JSON\.parse\('((?:[^'\\]|\\.)*)'\)")


def _unescape(js_literal: str) -> str:
    """Turn a JS single-quoted literal back into the JSON text it holds."""
    return js_literal.replace("\\'", "'")


def extract_payloads(html: str, *, min_len: int = 200) -> dict[str, object]:
    """Every `name = JSON.parse('...')` in the page, decoded."""
    out: dict[str, object] = {}
    for match in _JSON_PARSE.finditer(html):
        name, body = match.group(1), match.group(2)
        if len(body) < min_len:
            continue
        try:
            out[name] = json.loads(_unescape(body))
        except json.JSONDecodeError:
            continue
    return out


def _page(name: str, *, offline: bool, max_age: float) -> dict[str, object]:
    key = f"datagolf_{name}.html"
    fixture = f"datagolf_{name}.json"
    if offline:
        return load_fixture(fixture)  # type: ignore[return-value]
    payloads = extract_payloads(fetch(BASE + PAGES[name], key, max_age=max_age))
    if not payloads:
        raise RuntimeError(f"no JSON payload found on datagolf{PAGES[name]}")
    return payloads


def _prune(name: str, payloads: dict, course: str | None) -> dict:
    """Drop what the loaders never read.

    The pages carry a lot of chart furniture -- per-bin skill breakdowns,
    every course on tour, sportsbook-by-sportsbook prices -- and a fixture
    that ships in the repo should not.
    """
    if name == "rankings":
        keep = ("dg_id", "first", "last", "dg_skill", "dg_rank", "sample")
        rows = payloads["reload_data"]["data"]["table_data"]["data"]
        rows = [{k: r[k] for k in keep if k in r} for r in rows]
        return {"reload_data": {"data": {"table_data": {"data": rows}}}}
    if name == "course_fit":
        data = payloads["reload_data"]
        courses = [course] if course else [k for k in data if k != "players"]
        out: dict = {"players": {}}
        for c in courses:
            out[c] = {"coefs": data[c].get("coefs")}
            block = data["players"][c]
            out["players"][c] = {
                "event_name": block.get("event_name"),
                "data": block["data"],
            }
        return {"reload_data": out}
    if name == "finish_odds":
        keep = {"dg_id", "player_name"}
        for m in _MARKETS:
            keep |= {f"{m}_dg", f"{m}_best"}
        rows = payloads["flask_data"][0]["data"]
        rows = [{k: r[k] for k in keep if k in r} for r in rows]
        return {"flask_data": [{"data": rows}]}
    if name == "fantasy":
        block_keys = ("constants", "data")
        out = {
            "reload_data": {
                k: {bk: v[bk] for bk in block_keys}
                for k, v in payloads["reload_data"].items()
            }
        }
        if "hourly" in payloads:
            out["hourly"] = payloads["hourly"]
        return out
    return payloads


def snapshot(name: str, *, max_age: float = 0.0, course: str | None = None) -> None:
    """Freeze a page's payloads into the shipped fixtures."""
    payloads = _page(name, offline=False, max_age=max_age)
    save_fixture(f"datagolf_{name}.json", _prune(name, payloads, course))


# --------------------------------------------------------------------------
# Name handling
# --------------------------------------------------------------------------

_SUFFIXES = {"jr", "jr.", "sr", "sr.", "ii", "iii", "iv"}


def normalize_name(name: str) -> str:
    """A join key that survives 'Højgaard' vs 'Hojgaard' and 'Kim, Si Woo'.

    DraftKings, DataGolf's rankings and DataGolf's tools all spell and order
    names differently, so every source is pushed through this before a join.
    """
    name = name.strip()
    if "," in name:
        last, _, first = name.partition(",")
        name = f"{first.strip()} {last.strip()}"
    name = unicodedata.normalize("NFKD", name)
    name = "".join(ch for ch in name if not unicodedata.combining(ch))
    name = name.replace("ø", "o").replace("Ø", "O").replace("æ", "ae").replace("ß", "ss")
    name = name.lower().replace(".", " ").replace("-", " ").replace("'", "")
    parts = [p for p in name.split() if p not in _SUFFIXES]
    return " ".join(parts)


# --------------------------------------------------------------------------
# Loaders
# --------------------------------------------------------------------------


@dataclass(frozen=True)
class Ranking:
    dg_id: int
    name: str
    dg_skill: float      # true talent, strokes gained per round vs a baseline field
    dg_rank: float
    sample: float


def load_rankings(*, offline: bool = False, max_age: float = 21600.0) -> dict[str, Ranking]:
    """dg_skill for every ranked player, keyed by normalized name."""
    payloads = _page("rankings", offline=offline, max_age=max_age)
    rows = payloads["reload_data"]["data"]["table_data"]["data"]  # type: ignore[index]
    out: dict[str, Ranking] = {}
    for r in rows:
        name = f"{r['first']} {r['last']}"
        out[normalize_name(name)] = Ranking(
            dg_id=int(r["dg_id"]),
            name=name,
            dg_skill=float(r["dg_skill"]),
            dg_rank=float(r["dg_rank"]),
            sample=float(r.get("sample") or 0.0),
        )
    return out


@dataclass(frozen=True)
class CourseFit:
    dg_id: int
    name: str
    fit: float           # strokes per round, positive = course suits the player
    distance: float      # standardized skill components
    accuracy: float
    approach: float
    around_green: float
    putting: float


def load_course_fit(
    course: str, *, offline: bool = False, max_age: float = 21600.0
) -> tuple[dict[str, CourseFit], dict]:
    """Per-player course fit for `course`, plus the course's own coefficients."""
    payloads = _page("course_fit", offline=offline, max_age=max_age)
    data = payloads["reload_data"]
    if course not in data:  # type: ignore[operator]
        raise KeyError(f"{course!r} not on the course-fit tool; have {sorted(data)[:5]}...")
    block = data["players"][course]  # type: ignore[index]
    fits: dict[str, CourseFit] = {}
    for r in block["data"]:
        name = r["player_name"]
        fits[normalize_name(name)] = CourseFit(
            dg_id=int(r["dg_id"]),
            name=name,
            fit=float(r["total_comp"]),
            distance=float(r["dist"]),
            accuracy=float(r["acc"]),
            approach=float(r["app"]),
            around_green=float(r["arg"]),
            putting=float(r["putt"]),
        )
    meta = {"event_name": block.get("event_name"), "coefs": data[course].get("coefs")}  # type: ignore[index]
    return fits, meta


@dataclass(frozen=True)
class SlateRow:
    dk_id: int           # == DraftKings draftableId
    name: str | None     # None where DataGolf masks it
    salary: float
    ownership: float     # projected, percent
    score_sd: float      # DataGolf's projected DK-score SD
    wave: int            # 0 = one tee wave, 1 = the other
    tee_order: int
    dg_points: float | None       # DataGolf's projected DK points, where visible
    dg_finish_points: float | None


def load_fantasy_projections(
    slate: str = "DK - Main Slate", *, offline: bool = False, max_age: float = 1800.0
) -> tuple[list[SlateRow], dict]:
    """Projected ownership, score SD and tee waves for a DraftKings slate."""
    payloads = _page("fantasy", offline=offline, max_age=max_age)
    block = payloads["reload_data"][slate]  # type: ignore[index]
    consts = dict(block["constants"])
    masked = {"Peeking Stop", "Stop, Peeking"}
    rows = []
    for r in block["data"]:
        name = r.get("fantasy_name")
        visible = name not in masked and int(r.get("player_num", -1)) > 0
        rows.append(
            SlateRow(
                dk_id=int(r["dk_id"]),
                name=name if visible else None,
                salary=float(r["salary"]),
                ownership=float(r["ownership"]),
                score_sd=float(r["sd"]),
                wave=int(r["early_late"]),
                tee_order=int(r.get("tee_order", 0)),
                dg_points=float(r["adj_points_pred"]) if visible else None,
                dg_finish_points=float(r["adj_points_pred_finish"]) if visible else None,
            )
        )
    return rows, consts


@dataclass(frozen=True)
class FinishOdds:
    """One row of the finish-odds tool.

    `name` is None for everyone outside DataGolf's visible top five, and the
    row order is DataGolf's own and does not match any other page's, so these
    rows cannot be joined to a player. They are still worth having: the
    *distribution* of win / top-N probabilities across the field is exactly
    what the simulator's variance has to reproduce, and that comparison does
    not need identities. See projections.calibrate.
    """

    name: str | None
    model: dict[str, float]    # DataGolf model probabilities
    market: dict[str, float]   # best available price across books


_MARKETS = ("win", "top_5", "top_10", "top_20")
_PLACES = {"win": 1, "top_5": 5, "top_10": 10, "top_20": 20}


def load_finish_odds(*, offline: bool = False, max_age: float = 1800.0) -> list[FinishOdds]:
    """Win / top-5 / top-10 / top-20 probabilities, DataGolf model and market.

    Market prices are best-available decimal odds across books, so taken
    together they overround by less than a single book would -- sometimes not
    at all. Each market is renormalised so the field's probabilities sum to
    the number of places it pays, which removes whatever vig is left and
    makes the two sources comparable.
    """
    payloads = _page("finish_odds", offline=offline, max_age=max_age)
    rows = payloads["flask_data"][0]["data"]  # type: ignore[index]

    masked = ("Stop, Peeking", "Peeking Stop")
    names: list[str | None] = []
    model_raw: list[dict[str, float]] = []
    market_raw: list[dict[str, float]] = []
    for r in rows:
        raw_name = str(r.get("player_name", ""))
        names.append(None if raw_name.startswith(masked) else raw_name)
        model, market = {}, {}
        for m in _MARKETS:
            dec = r.get(f"{m}_dg")
            if dec and dec > 1.0:
                model[m] = 1.0 / float(dec)
            best = r.get(f"{m}_best")
            if best and best > 1.0:
                market[m] = 1.0 / float(best)
        model_raw.append(model)
        market_raw.append(market)

    for raw in (model_raw, market_raw):
        for m, n_places in _PLACES.items():
            total = sum(p.get(m, 0.0) for p in raw)
            if total <= 0:
                continue
            for p in raw:
                if m in p:
                    p[m] *= n_places / total

    return [FinishOdds(n, mo, ma) for n, mo, ma in zip(names, model_raw, market_raw)]


def finish_probability_vectors(
    odds: list[FinishOdds], source: str = "model"
) -> dict[str, list[float]]:
    """Each market's probabilities, sorted high to low.

    Identity-free, which is the point: it is the shape of the field's win
    distribution, and the simulator can be scored against it directly.
    """
    out: dict[str, list[float]] = {}
    for m in _MARKETS:
        vals = [getattr(o, source).get(m) for o in odds]
        vals = sorted((v for v in vals if v is not None), reverse=True)
        if vals:
            out[m] = vals
    return out


def load_weather(*, offline: bool = False, max_age: float = 1800.0) -> dict:
    payloads = _page("fantasy", offline=offline, max_age=max_age)
    return payloads.get("hourly", {})  # type: ignore[return-value]
