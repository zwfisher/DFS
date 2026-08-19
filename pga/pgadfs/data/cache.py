"""On-disk cache for anything fetched over the network.

Every remote source in this package is a public endpoint that can go away,
rate-limit, or silently change shape mid-week. Caching the raw payload means
a slate can be re-run, and a bad run diagnosed, without re-fetching.
"""

from __future__ import annotations

import json
import os
import time
import urllib.request
from pathlib import Path

USER_AGENT = (
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 "
    "(KHTML, like Gecko) Chrome/126.0 Safari/537.36"
)

DEFAULT_CACHE = Path(os.environ.get("PGADFS_CACHE", Path.home() / ".cache" / "pgadfs"))
FIXTURES = Path(__file__).parent / "fixtures"


def cache_dir() -> Path:
    DEFAULT_CACHE.mkdir(parents=True, exist_ok=True)
    return DEFAULT_CACHE


def fetch(url: str, key: str, *, max_age: float = 3600.0, timeout: float = 45.0) -> str:
    """GET `url`, cached under `key` for `max_age` seconds."""
    path = cache_dir() / key
    if path.exists() and (time.time() - path.stat().st_mtime) < max_age:
        return path.read_text(encoding="utf-8")

    req = urllib.request.Request(url, headers={"User-Agent": USER_AGENT})
    with urllib.request.urlopen(req, timeout=timeout) as resp:
        body = resp.read().decode("utf-8", errors="replace")
    path.write_text(body, encoding="utf-8")
    return body


def fetch_json(url: str, key: str, **kwargs) -> object:
    return json.loads(fetch(url, key, **kwargs))


def load_fixture(name: str) -> object:
    """Read a snapshotted payload shipped with the package."""
    return json.loads((FIXTURES / name).read_text(encoding="utf-8"))


def save_fixture(name: str, obj: object) -> Path:
    FIXTURES.mkdir(parents=True, exist_ok=True)
    path = FIXTURES / name
    path.write_text(json.dumps(obj, separators=(",", ":")), encoding="utf-8")
    return path


def fixture_exists(name: str) -> bool:
    return (FIXTURES / name).exists()
