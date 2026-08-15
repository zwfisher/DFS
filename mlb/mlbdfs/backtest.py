"""Post-contest analysis from a DraftKings standings export.

Run this after every contest. The standings file is the only free source of
ground truth this project has: exact ownership, exact scores, and the full
distribution of what the field actually built. It answers the questions that
matter more than any pre-slate diagnostic -- was the field as strong as the
model assumed, was ownership projected well, and did entries lose for the
reason they appear to have lost for.

The zero-score analysis is here because it turned out to be the single most
predictive feature of finishing position, more than salary usage, stack
shape, or raw projection.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from pathlib import Path

import numpy as np
import pandas as pd

# Standings lineups look like "P Zack Wheeler C Will Smith 1B Freddie Freeman"
LINEUP_RE = re.compile(
    r"\b(?:P|SP|RP|C|1B|2B|3B|SS|OF)\b\s+(.+?)"
    r"(?=\s+\b(?:P|SP|RP|C|1B|2B|3B|SS|OF)\b|$)"
)


@dataclass
class ContestResults:
    entries: pd.DataFrame  # one row per entry, with parsed players and zero count
    players: pd.DataFrame  # one row per rostered player, with ownership and points

    @property
    def field_mean_score(self) -> float:
        """The number to feed back into ``target_field_mean_score``."""
        return float(self.entries["Points"].mean())

    @property
    def winning_score(self) -> float:
        return float(self.entries["Points"].max())


def load(path: str | Path) -> ContestResults:
    """Parse a DraftKings contest standings CSV.

    The export packs two tables into one file: entries on the left, and a
    per-player summary with ownership and points on the right, padded with
    blank rows.
    """
    raw = pd.read_csv(path)

    entries = raw[raw["EntryId"].notna()].copy()
    players = raw[raw["Player"].notna()][
        ["Player", "Roster Position", "%Drafted", "FPTS"]
    ].copy()
    players["ownership"] = (
        players["%Drafted"].astype(str).str.rstrip("%").astype(float) / 100
    )
    players["is_pitcher"] = players["Roster Position"] == "P"

    points = dict(zip(players["Player"], players["FPTS"]))
    entries["players"] = entries["Lineup"].map(lambda x: LINEUP_RE.findall(str(x)))
    entries["n_parsed"] = entries["players"].map(len)
    entries["zeros"] = entries["players"].map(
        lambda ps: sum(1 for p in ps if points.get(p, 0.0) == 0.0)
    )
    entries["unmatched"] = entries["players"].map(
        lambda ps: sum(1 for p in ps if p not in points)
    )
    entries["username"] = (
        entries["EntryName"].astype(str).str.replace(r"\s*\(\d+/\d+\)$", "", regex=True)
    )

    return ContestResults(entries=entries, players=players)


def score_distribution(results: ContestResults) -> pd.DataFrame:
    """Percentiles of the entry score distribution."""
    points = results.entries["Points"]
    rows = [
        {"statistic": f"p{q}", "score": float(np.percentile(points, q))}
        for q in (1, 10, 25, 50, 75, 90, 99, 99.9)
    ]
    rows.append({"statistic": "mean", "score": float(points.mean())})
    rows.append({"statistic": "max", "score": float(points.max())})
    return pd.DataFrame(rows)


def zero_analysis(results: ContestResults) -> pd.DataFrame:
    """What each zero-scoring player cost, by number of zeros in a lineup.

    A hitter who does not start scores zero, and so does one who goes hitless
    with nothing else. Both are the same disaster on a roster, and the
    gradient here is steep enough that avoiding them is worth more than most
    projection accuracy.
    """
    valid = results.entries[results.entries["n_parsed"] == 10]
    total = len(valid)
    top_1pct = max(int(total * 0.01), 1)

    grouped = valid.groupby("zeros").agg(
        entries=("Points", "size"),
        share=("Points", lambda s: len(s) / total),
        mean_score=("Points", "mean"),
        median_rank=("Rank", "median"),
        pct_top_1pct=("Rank", lambda r: (r <= top_1pct).mean()),
    )
    return grouped.reset_index()


def zeros_by_ownership(results: ContestResults) -> pd.DataFrame:
    """Zero-score rate by ownership tier, hitters only.

    The diagnostic that separates "went hitless" from "never played". A
    starting hitter posts an empty line about a fifth of the time; a rate far
    above that in the low-ownership tiers means those players were not in the
    lineup at all.
    """
    hitters = results.players[~results.players["is_pitcher"]].copy()
    hitters["tier"] = pd.cut(
        hitters["ownership"],
        [0, 0.02, 0.05, 0.10, 0.20, 1.0],
        labels=["<2%", "2-5%", "5-10%", "10-20%", "20%+"],
    )
    return (
        hitters.groupby("tier", observed=True)
        .agg(
            players=("FPTS", "size"),
            zero_rate=("FPTS", lambda s: (s == 0).mean()),
            mean_points=("FPTS", "mean"),
        )
        .reset_index()
    )


def winners_versus_field(results: ContestResults, top_n: int = 200) -> pd.DataFrame:
    """How the best entries differed from the field."""
    valid = results.entries[results.entries["n_parsed"] == 10]
    top = valid.nsmallest(top_n, "Rank")
    return pd.DataFrame(
        [
            {
                "metric": "mean zero-scoring players",
                f"top_{top_n}": top["zeros"].mean(),
                "field": valid["zeros"].mean(),
            },
            {
                "metric": "share with no zeros",
                f"top_{top_n}": (top["zeros"] == 0).mean(),
                "field": (valid["zeros"] == 0).mean(),
            },
            {
                "metric": "share with 3+ zeros",
                f"top_{top_n}": (top["zeros"] >= 3).mean(),
                "field": (valid["zeros"] >= 3).mean(),
            },
            {
                "metric": "mean score",
                f"top_{top_n}": top["Points"].mean(),
                "field": valid["Points"].mean(),
            },
        ]
    )


def chalk_performance(results: ContestResults) -> pd.DataFrame:
    """Whether the field's popular plays paid off.

    A positive correlation between ownership and points means the field read
    the slate correctly and leverage was expensive; a negative one means the
    chalk busted and contrarian entries were rewarded.
    """
    rows = []
    for label, subset in (
        ("hitters", results.players[~results.players["is_pitcher"]]),
        ("pitchers", results.players[results.players["is_pitcher"]]),
    ):
        rows.append(
            {
                "group": label,
                "corr_ownership_points": subset["ownership"].corr(subset["FPTS"]),
                "mean_points": subset["FPTS"].mean(),
                "mean_points_top20_owned": subset.nlargest(20, "ownership")[
                    "FPTS"
                ].mean(),
            }
        )
    return pd.DataFrame(rows)


def entries_for(results: ContestResults, username: str) -> pd.DataFrame:
    """One user's entries, with the diagnostics attached.

    DraftKings writes entry names as "username (3/150)", so the match is on
    the username portion and is case insensitive.
    """
    mask = results.entries["username"].str.lower() == username.lower()
    cols = ["Rank", "EntryName", "Points", "zeros", "Lineup"]
    return results.entries.loc[mask, cols].sort_values("Rank").reset_index(drop=True)


def user_summary(results: ContestResults, username: str) -> pd.DataFrame:
    """How one user's entries compared to the field."""
    mine = entries_for(results, username)
    if mine.empty:
        return pd.DataFrame()

    valid = results.entries[results.entries["n_parsed"] == 10]
    total = len(results.entries)
    return pd.DataFrame(
        [
            {"metric": "entries", "you": len(mine), "field": total},
            {
                "metric": "mean score",
                "you": mine["Points"].mean(),
                "field": valid["Points"].mean(),
            },
            {
                "metric": "best score",
                "you": mine["Points"].max(),
                "field": valid["Points"].max(),
            },
            {
                "metric": "mean zero-scoring players",
                "you": mine["zeros"].mean(),
                "field": valid["zeros"].mean(),
            },
            {
                "metric": "best finish (percentile)",
                "you": 1 - mine["Rank"].min() / total,
                "field": np.nan,
            },
            {
                "metric": "median finish (percentile)",
                "you": 1 - mine["Rank"].median() / total,
                "field": 0.5,
            },
        ]
    )


def ownership_accuracy(
    results: ContestResults, projected: pd.DataFrame
) -> pd.DataFrame:
    """Compare a projected ownership frame against what the field did.

    ``projected`` needs ``name`` and ``ownership`` columns -- the output of
    ``mlbdfs project``. Bias matters more than absolute error for tournament
    play: a model that is systematically low on chalk makes every leverage
    play look better than it is.
    """
    from .data.dk import _match_key

    actual = results.players.copy()
    actual["_key"] = actual["Player"].map(_match_key)
    proj = projected.copy()
    proj["_key"] = proj["name"].map(_match_key)

    merged = actual.merge(
        proj[["_key", "ownership"]].rename(columns={"ownership": "projected"}),
        on="_key",
        how="inner",
    )
    if merged.empty:
        return merged

    merged["error"] = merged["projected"] - merged["ownership"]
    return merged[
        ["Player", "Roster Position", "ownership", "projected", "error", "FPTS"]
    ].sort_values("ownership", ascending=False)


# --------------------------------------------------------------------------
# Projected lineup accuracy
# --------------------------------------------------------------------------


def lineup_accuracy(
    history: pd.DataFrame,
    min_prior_games: int = 20,
    max_dates: int | None = None,
) -> pd.DataFrame:
    """Walk-forward test of the projected-lineup model.

    For each team-game, project the lineup using only games that had already
    happened, then compare against who actually started. This is the number
    that decides whether projecting lineups is worth doing at all, and it
    should be run before trusting the projections on a live slate.

    Reported per team-game:

    * ``hits`` -- how many of the projected nine actually started
    * ``slot_mae`` -- mean absolute batting-order error over those hits
    * ``brier`` -- calibration of ``start_probability`` against who started

    A projection that gets seven or eight of nine is doing real work; one
    that gets six is close to what you would get by taking yesterday's card
    and is not worth the machinery.
    """
    from .projections.projected_lineups import project_lineup

    history = history.copy()
    history["game_date"] = pd.to_datetime(history["game_date"])

    rows = []
    for team, team_games in history.groupby("team"):
        dates = np.sort(team_games["game_date"].unique())
        targets = dates[min_prior_games:]
        if max_dates:
            targets = targets[-max_dates:]

        for target in targets:
            actual = team_games[team_games["game_date"] == target]
            if len(actual) < 9:
                continue
            hand = str(actual["opp_hand"].iloc[0])

            projection = project_lineup(history, team, hand, asof=target)
            if projection.frame.empty:
                continue

            projected = projection.starters
            actual_ids = set(actual["player_id"])
            hits = projected["player_id"].isin(actual_ids)

            actual_slots = dict(zip(actual["player_id"], actual["batting_order"]))
            slot_errors = [
                abs(int(row.batting_order) - actual_slots[row.player_id])
                for row in projected[hits].itertuples()
            ]

            probs = projection.frame.set_index("player_id")["start_probability"]
            started = probs.index.isin(actual_ids).astype(float)
            brier = float(np.mean((probs.to_numpy() - started) ** 2))

            rows.append(
                {
                    "team": team,
                    "game_date": target,
                    "opp_hand": hand,
                    "hits": int(hits.sum()),
                    "slot_mae": float(np.mean(slot_errors)) if slot_errors else np.nan,
                    "brier": brier,
                }
            )

    return pd.DataFrame(rows)


def accuracy_summary(accuracy: pd.DataFrame) -> pd.DataFrame:
    """Headline numbers from ``lineup_accuracy``, split by opposing hand.

    The split matters: if accuracy against left-handers is materially worse
    than against right-handers, the platoon conditioning is not working, and
    left-handed starters are where lineups are hardest to guess and most
    worth guessing right.
    """
    if accuracy.empty:
        return pd.DataFrame()

    def block(frame: pd.DataFrame, label: str) -> dict:
        return {
            "split": label,
            "team_games": len(frame),
            "mean_hits_of_9": frame["hits"].mean(),
            "pct_8_or_9": (frame["hits"] >= 8).mean(),
            "pct_perfect": (frame["hits"] == 9).mean(),
            "slot_mae": frame["slot_mae"].mean(),
            "brier": frame["brier"].mean(),
        }

    rows = [block(accuracy, "all")]
    for hand, frame in accuracy.groupby("opp_hand"):
        rows.append(block(frame, f"vs {hand}HP"))
    return pd.DataFrame(rows)
