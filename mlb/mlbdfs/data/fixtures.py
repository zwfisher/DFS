"""Synthetic slate generation for tests and offline development.

Real baseball data requires network access to Baseball Savant, the MLB Stats
API or FanGraphs. These fixtures produce a structurally identical slate with
plausible talent spreads, so every layer above the data fetch can be
developed and tested without a connection.
"""

from __future__ import annotations

import numpy as np

from ..config import LEAGUE_RATES, LEAGUE_RUNS_PER_GAME, N_OUTCOMES, OUTCOMES
from ..projections.build import RateBook
from ..slate import Game, Player, Slate

TEAMS = [
    "NYY", "BOS", "TB", "TOR", "BAL",
    "CLE", "MIN", "DET", "KC", "CWS",
    "HOU", "SEA", "TEX", "LAA", "OAK",
    "ATL", "PHI", "NYM", "MIA", "WSH",
    "MIL", "CHC", "STL", "CIN", "PIT",
    "LAD", "SD", "SF", "ARI", "COL",
]

HITTER_SLOT_POSITIONS = ["C", "1B", "2B", "3B", "SS", "OF", "OF", "OF", "1B"]


def _perturb_rates(base: np.ndarray, rng: np.random.Generator, scale: float) -> np.ndarray:
    """Draw a player's rate vector around the league mean on the logit scale."""
    noise = rng.normal(0.0, scale, size=N_OUTCOMES)
    perturbed = base * np.exp(noise)
    return perturbed / perturbed.sum()


def make_slate(
    n_games: int = 6,
    seed: int = 7,
    salary_noise: float = 0.18,
) -> tuple[Slate, RateBook]:
    """Build a synthetic slate with ``n_games`` games and matching rate book.

    Salaries are generated from the players' own simulated talent so the
    optimizer faces a realistic value landscape rather than random pricing.
    """
    rng = np.random.default_rng(seed)
    league = np.array([LEAGUE_RATES[o] for o in OUTCOMES], dtype=np.float64)
    league = league / league.sum()

    teams = TEAMS[: n_games * 2]
    players: list[Player] = []
    games: list[Game] = []
    batter_rates: dict[str, np.ndarray] = {}
    pitcher_rates: dict[str, np.ndarray] = {}
    steal_rates: dict[str, float] = {}

    for g in range(n_games):
        away, home = teams[2 * g], teams[2 * g + 1]

        total = rng.normal(8.9, 1.1)
        home_edge = rng.normal(0.25, 0.35)
        away_total = max(2.6, (total - home_edge) / 2.0)
        home_total = max(2.6, (total + home_edge) / 2.0)

        starters = {}
        for team, opp in ((away, home), (home, away)):
            # Starting pitcher: better pitchers suppress hits and walks and
            # inflate strikeouts.
            skill = rng.normal(0.0, 1.0)
            sp_rates = league.copy()
            sp_rates[OUTCOMES.index("strikeout")] *= np.exp(0.22 * skill)
            for o in ("single", "double", "home_run", "walk"):
                sp_rates[OUTCOMES.index(o)] *= np.exp(-0.16 * skill)
            sp_rates = sp_rates / sp_rates.sum()

            pid = f"SP_{team}"
            salary = int(np.clip(rng.normal(8600 + 1500 * skill, 700), 5000, 12600))
            players.append(
                Player(
                    player_id=pid,
                    name=f"{team} Starter",
                    team=team,
                    opponent=opp,
                    positions=("P",),
                    salary=salary,
                    is_pitcher=True,
                    throws="R" if rng.random() < 0.72 else "L",
                    confirmed=True,
                    dk_id=pid,
                )
            )
            pitcher_rates[pid] = sp_rates
            starters[team] = pid

            # Nine hitters in batting order, best hitters near the top.
            for slot in range(9):
                talent = rng.normal(0.35 - 0.075 * slot, 0.42)
                rates = league.copy()
                rates[OUTCOMES.index("strikeout")] *= np.exp(-0.18 * talent)
                for o in ("single", "double", "home_run", "walk"):
                    rates[OUTCOMES.index(o)] *= np.exp(0.30 * talent)
                rates = _perturb_rates(rates / rates.sum(), rng, 0.10)

                hid = f"{team}_H{slot + 1}"
                base_salary = 3400 + 1500 * talent + 220 * (8 - slot)
                salary = int(
                    np.clip(
                        rng.normal(base_salary, base_salary * salary_noise), 2000, 6800
                    )
                )
                pos = HITTER_SLOT_POSITIONS[slot]
                positions = (pos,) if rng.random() < 0.8 else (pos, "OF")
                players.append(
                    Player(
                        player_id=hid,
                        name=f"{team} Hitter {slot + 1}",
                        team=team,
                        opponent=opp,
                        positions=tuple(dict.fromkeys(positions)),
                        salary=salary,
                        is_pitcher=False,
                        batting_order=slot + 1,
                        bats="R" if rng.random() < 0.65 else "L",
                        confirmed=True,
                        dk_id=hid,
                    )
                )
                batter_rates[hid] = rates
                steal_rates[hid] = float(
                    np.clip(rng.gamma(1.6, 0.045), 0.002, 0.34)
                )

        games.append(
            Game(
                game_id=f"{away}@{home}",
                away=away,
                home=home,
                away_total=away_total,
                home_total=home_total,
                away_starter=starters[away],
                home_starter=starters[home],
            )
        )

    slate = Slate(players=players, games=games, name=f"synthetic_{n_games}g")
    book = RateBook(
        batters=batter_rates, pitchers=pitcher_rates, steal_rates=steal_rates
    )
    return slate, book


def make_neutral_game(seed: int = 0) -> tuple[Slate, RateBook]:
    """One game of league-average hitters against a league-average pitcher.

    The synthetic slate deliberately spreads team talent widely, which is
    right for exercising the optimizer but wrong for judging the simulator's
    run distribution: pooling teams with implied totals from 2.6 to 6.0
    inflates the tail regardless of what the engine does. This fixture strips
    that out so run-distribution checks measure the simulator alone.
    """
    league = np.array([LEAGUE_RATES[o] for o in OUTCOMES], dtype=np.float64)
    league = league / league.sum()

    players: list[Player] = []
    batter_rates: dict[str, np.ndarray] = {}
    pitcher_rates: dict[str, np.ndarray] = {}
    steal_rates: dict[str, float] = {}

    for team, opp in (("AAA", "BBB"), ("BBB", "AAA")):
        pid = f"SP_{team}"
        players.append(
            Player(
                player_id=pid, name=f"{team} SP", team=team, opponent=opp,
                positions=("P",), salary=8000, is_pitcher=True, confirmed=True,
            )
        )
        pitcher_rates[pid] = league.copy()
        for slot in range(9):
            hid = f"{team}_H{slot + 1}"
            players.append(
                Player(
                    player_id=hid, name=f"{team} H{slot + 1}", team=team,
                    opponent=opp, positions=(HITTER_SLOT_POSITIONS[slot],),
                    salary=4000, batting_order=slot + 1, confirmed=True,
                )
            )
            batter_rates[hid] = league.copy()
            steal_rates[hid] = 0.055

    # A neutral park and a league-average implied total on both sides.
    games = [
        Game(
            game_id="AAA@BBB", away="AAA", home="BBB",
            away_total=LEAGUE_RUNS_PER_GAME, home_total=LEAGUE_RUNS_PER_GAME,
            away_starter="SP_AAA", home_starter="SP_BBB",
        )
    ]
    slate = Slate(players=players, games=games, name="neutral")
    return slate, RateBook(
        batters=batter_rates, pitchers=pitcher_rates, steal_rates=steal_rates
    )
