"""Fetch-once caching for everything that comes off the network.

Baseball data is slow to pull and almost entirely immutable once a game is
final, so every fetch goes to a parquet file on disk keyed by its arguments.
Beyond politeness to the upstream services, this is what makes iterating on
the projection model bearable -- you pull a season once and then never wait
again.

``MLBDFS_OFFLINE=1`` makes a cache miss an error instead of a fetch, which
is how the test suite guarantees it never touches the network.
"""

from __future__ import annotations

import hashlib
import json
import logging
from collections.abc import Callable
from datetime import datetime, timedelta
from pathlib import Path

import pandas as pd

from ..config import CACHE_DIR, OFFLINE

log = logging.getLogger(__name__)


class OfflineError(RuntimeError):
    """Raised when a fetch is needed but the network is disabled."""


def cache_key(name: str, **params) -> str:
    """Stable filename for a call and its arguments."""
    blob = json.dumps(params, sort_keys=True, default=str)
    digest = hashlib.sha1(blob.encode()).hexdigest()[:12]
    return f"{name}__{digest}"


def cache_path(name: str, **params) -> Path:
    return CACHE_DIR / f"{cache_key(name, **params)}.parquet"


def cached_frame(
    name: str,
    fetch: Callable[[], pd.DataFrame],
    ttl: timedelta | None = None,
    force: bool = False,
    **params,
) -> pd.DataFrame:
    """Return a cached frame, fetching and storing it on a miss.

    ``ttl`` is for the handful of things that do change -- today's lineups,
    a slate's salaries. Historical data is written once and left alone.
    """
    path = cache_path(name, **params)

    if path.exists() and not force:
        if ttl is None:
            return pd.read_parquet(path)
        age = datetime.now() - datetime.fromtimestamp(path.stat().st_mtime)
        if age < ttl:
            return pd.read_parquet(path)

    if OFFLINE:
        raise OfflineError(
            f"{name} is not cached at {path} and MLBDFS_OFFLINE is set. "
            "Run once with network access to populate the cache."
        )

    log.info("fetching %s %s", name, params)
    frame = fetch()

    path.parent.mkdir(parents=True, exist_ok=True)
    frame.to_parquet(path, index=False)
    return frame


def clear(name: str | None = None) -> int:
    """Delete cached files, optionally only those for one fetcher."""
    if not CACHE_DIR.exists():
        return 0
    pattern = f"{name}__*.parquet" if name else "*.parquet"
    files = list(CACHE_DIR.glob(pattern))
    for f in files:
        f.unlink()
    return len(files)


def describe() -> pd.DataFrame:
    """What is in the cache and how big it is."""
    if not CACHE_DIR.exists():
        return pd.DataFrame(columns=["file", "mb", "modified"])
    rows = [
        {
            "file": f.name,
            "mb": round(f.stat().st_size / 1e6, 2),
            "modified": datetime.fromtimestamp(f.stat().st_mtime),
        }
        for f in sorted(CACHE_DIR.glob("*.parquet"))
    ]
    return pd.DataFrame(rows)
