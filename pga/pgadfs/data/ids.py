"""Joining golfers across sources that disagree about their names.

DraftKings writes "Nicolas Echavarria" and "Matthew McCarty"; DataGolf's
course-fit tool writes "Echavarria, Nico" and "McCarty, Matt". Neither is
wrong and there is no shared id between those two pages, so the join needs a
ladder of increasingly loose keys rather than a single one.
"""

from __future__ import annotations

from typing import Iterable, TypeVar

from .datagolf import normalize_name

V = TypeVar("V")


def _last_first_initial(name: str) -> str:
    parts = normalize_name(name).split()
    return f"{parts[-1]}|{parts[0][0]}" if len(parts) >= 2 else ""


def _last(name: str) -> str:
    parts = normalize_name(name).split()
    return parts[-1] if parts else ""


class NameIndex:
    """Look a golfer up by name, tolerating nicknames and diacritics.

    Three passes, strictest first: the fully normalized name, then last name
    plus first initial, then last name alone. The looser passes only answer
    when they are unambiguous within the source, so "Kim" never resolves.
    """

    def __init__(self, items: Iterable[tuple[str, V]]):
        self._exact: dict[str, V] = {}
        li: dict[str, list[V]] = {}
        last: dict[str, list[V]] = {}
        for name, value in items:
            self._exact.setdefault(normalize_name(name), value)
            li.setdefault(_last_first_initial(name), []).append(value)
            last.setdefault(_last(name), []).append(value)
        self._li = {k: v[0] for k, v in li.items() if len(v) == 1 and k}
        self._last = {k: v[0] for k, v in last.items() if len(v) == 1 and k}

    def get(self, name: str, default: V | None = None) -> V | None:
        key = normalize_name(name)
        if key in self._exact:
            return self._exact[key]
        hit = self._li.get(_last_first_initial(name))
        if hit is not None:
            return hit
        return self._last.get(_last(name), default)

    def __contains__(self, name: str) -> bool:
        return self.get(name) is not None

    def __len__(self) -> int:
        return len(self._exact)
