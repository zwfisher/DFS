# MLB DFS for DraftKings

Projections with confidence intervals, simulated ownership, and a lineup
optimizer that builds stacks and ranks them by expected ROI rather than by
projected points.

Python, under `mlb/`. The R code in the repository root is the older NFL
work and is untouched.

## Quick start

```bash
cd mlb
python -m venv .venv && source .venv/bin/activate
pip install -e ".[data,dev]"

mlbdfs demo                 # whole pipeline on synthetic data, no network
mlbdfs diagnose             # check the simulator against league aggregates
pytest                      # 84 tests, all offline
```

A real slate needs a DraftKings salary export:

```bash
mlbdfs project  --slate DKSalaries.csv --date 2026-08-12
mlbdfs optimize --slate DKSalaries.csv --date 2026-08-12 \
                --contest large_gpp --entries 50000 --fee 5 \
                --lineups 20 --out lineups.csv
```

**Run it after batting orders post**, usually one to three hours before
first pitch. `optimize` refuses a slate whose lineups are mostly unposted
unless you pass `--allow-unconfirmed`, for reasons in the next section.

After the contest settles:

```bash
mlbdfs backtest --standings contest-standings-193621106.csv --username yourname
```

## How it works

Four layers. The interface between them is a single `(n_sims, n_players)`
matrix of simulated DraftKings scores.

### 1. Projections

Per-plate-appearance talent rates over `{K, out, BB, HBP, 1B, 2B, 3B, HR}`,
shrunk toward league average with a Beta-Binomial prior whose strength is
each stat's stabilization point — so strikeout rate moves off the prior
quickly and triples barely move at all. Then log5 against the opposing
starter, park factors, and a damped nudge toward the Vegas implied total.

### 2. Simulation

A base-out Markov chain walks each team through its lineup, batter by
batter, and the opposing pitcher is scored in the same pass. Simulating
actual game states rather than players in isolation buys three things:

- **Runs and RBI are emitted**, not modelled. No context regression.
- **Teammate correlation is structural.** A five-stack scores in exactly the
  simulations where the team bats around. An independent-player model
  cannot see that and will systematically underprice stacks.
- **Pitcher-versus-opposing-hitter anticorrelation** falls out for free.

Confidence intervals are simulated quantiles, not regression standard
errors — the DraftKings score distribution is lumpy and right-skewed (a
home run is ten points), and tournaments pay for the right tail specifically.

The engine is vectorized *across* simulations rather than over them: every
sim of a game steps through the same plate appearance together as numpy
arrays, so the Python loop runs about 200 times per game instead of once per
simulation.

### 3. Ownership

Ownership is modelled, not simulated — it is a near-deterministic function
of salary, projected value, Vegas totals and batting order. The model is a
conditional logit per position group, which matters for one reason:
**ownership has to add up.** Total ownership at a position equals the number
of roster spots there. Per-player regressions violate that and produce
slates where every good player is 40% owned.

Monte Carlo enters one level up, where it earns its place:

- **Dirichlet draws** over the ownership vector, which preserve the sum
  constraint and the negative correlation between players — if the chalk
  comes in low, that ownership went somewhere else.
- **A generated field of opponent lineups**, sampled under DraftKings rules
  with realistic stacking, then calibrated so its marginals reproduce the
  projection. This is the real payoff: ownership marginals alone cannot tell
  you what to do, because tournaments are decided by joints.

### 4. Optimizer

OR-Tools CP-SAT. Salary cap, roster slots with multi-position eligibility,
five hitters per team, two games minimum, no hitters against your own
pitcher. Stack shapes (5-3, 5-2, 4-4, ...) with an option to require
consecutive batting order, which correlates far better than five scattered
hitters.

The candidate pool comes from **randomized objectives** — each solve
maximizes points under one sampled draw from the simulation, so the pool
contains lineups that win in different futures. Then every candidate is
scored against the field through the contest's payout curve, and the
portfolio is chosen by expected ROI under exposure and overlap limits.

That last step is the point of the whole project. On a typical slate,
lineups selected by ROI project **fewer** points than lineups selected by
projection but win two to four times as often.

## Validation

`mlbdfs diagnose` checks the simulator against league aggregates it is never
told:

| metric | simulated | real MLB |
|---|---|---|
| runs per team game | 4.27 | ~4.4 |
| P(shutout) | 0.071 | ~0.07 |
| P(10+ runs) | 0.058 | ~0.05 |
| starter innings | 5.0 | ~5.2 |
| PA, leadoff slot | 4.73 | 4.66 |
| PA, nine hole | 3.85 | 3.96 |
| P(hitter scores exactly 0) | 0.216 | ~0.20 |

## Performance

Measured on a 10-game slate, single core:

| stage | time |
|---|---|
| simulation, 10k sims | 6s |
| field, 20k opponent lineups | 3s |
| candidate pool, 150 lineups | 38s |
| ROI evaluation | 20s |
| **end to end** | **~90s** |

A 15-game slate at 20k sims simulates in 14s. The optimizer dominates
runtime at roughly 150ms per lineup; lower `--candidates` to trade
thoroughness for speed.

## Wait for lineups

This is the most important operational rule in the project, and it is here
because ignoring it cost real money.

A hitter who is not in the starting lineup scores zero, and zeros dominate
finishing position. From a 35,671-entry contest:

| | top 200 | field |
|---|---|---|
| mean zero-scoring players | 0.47 | 2.13 |
| share with no zeros | 60.5% | 10.1% |
| share with 3+ zeros | 0.5% | 36.7% |

Each additional zero was worth about 13 points. No entry with three or more
zeros finished in the top 1%.

Before lineups post there is no way to know who starts, so `resolve_lineup`
guesses the nine from salary. The model now prices that guess instead of
assuming it right: guessed starters carry a `start_probability` below one,
which the simulator applies per simulation, roughly doubling their chance of
scoring zero and lowering their projection accordingly. Salary rank also
maps onto batting slots through a realistic pattern rather than descending
salary — leadoff hitters are frequently cheap, and the expensive bats hit
second through fourth.

That makes running early *less wrong*. It does not make it right.

### Projected lineups

To run before lineups post, the model estimates who starts from the team's
recent games -- **conditioned on the handedness of the probable opposing
starter**, which is the whole point. Managers platoon, so "who started
recently" is the wrong question; a modal lineup drawn from ten games against
righties will be confidently wrong about exactly the players a platoon
decides.

It needs no new data source: the starting nine are the first nine distinct
batters in a team-game, and Statcast carries the pitcher's throwing hand on
every pitch, so both come out of the Statcast pull the projections already
make.

```bash
mlbdfs lineup-accuracy --season 2026     # walk-forward validation
mlbdfs optimize --slate DKSalaries.csv --allow-unconfirmed
```

Handedness is applied twice, since it changes two different things:

- **Who is in the lineup** -- start probability and batting slot, estimated
  against the hand on the mound and shrunk toward the player's overall rate.
- **How they hit** -- platoon splits on the per-plate-appearance rates,
  shrunk toward each hitter's own overall line rather than league average.

Run `mlbdfs lineup-accuracy` before trusting it. On synthetic history the
projection lands about 8.5 of 9 with a batting-slot error under 0.1; real
accuracy will be lower, and that command is how you find out by how much.

Still wait for the lineups when you can. This narrows the gap; it does not
close it, and it cannot know about a scratch announced an hour before lock.

## Things to know before trusting the output

**Verify the scoring constants.** `config.py` encodes DraftKings MLB Classic
scoring, but DraftKings was unreachable from the environment this was built
in, so the values are from secondary sources. Check them against the live
rules page. A wrong constant here silently corrupts every layer above it.

**Absolute ROI is soft; relative ROI is the signal.** ROI depends on how
strong the simulated field is, which is set by the ownership model — see
below. Rankings between candidate lineups are far more trustworthy than the
ROI numbers themselves.

**Ownership is the weakest layer.** It is hand-calibrated, not fitted,
because there is no clean free source of historical MLB ownership. Two
things follow. First, set `target_field_mean_score` from the average score
in contests you actually enter; without it the implied field sits near
league average and ROI comes out inflated. Second, start logging:

```bash
mlbdfs log-ownership --standings contest-standings.csv \
                     --date 2026-08-12 --contest-name "MLB $5 Milly"
```

DraftKings lets you download standings for any contest you entered, and
those give *exact* ownership. Once enough slates accumulate, the conditional
logit can be fit for real and swapped in behind the same interface.

**Known modelling gaps**, documented at their call sites: reached-on-error
is not simulated (~0.1 runs/game, omitted because misattributing earned runs
would cost a pitcher more than the play is worth); no pinch hitting, which
makes the bottom of the order slightly pessimistic on plate appearances;
weather is not modelled beyond static park factors.

## Layout

```
mlbdfs/
  config.py          scoring constants, roster rules, every tunable
  scoring.py         DraftKings points, box score and per-event
  slate.py           Slate / SimSlate data structures
  pipeline.py        end-to-end run, including the evaluation holdout
  backtest.py        post-contest analysis, and lineup projection accuracy
  data/              caching, pybaseball + Stats API, DK CSV, id crosswalk
  projections/       talent rates, log5 matchup, projected lineups, workload
  sim/               base-out Markov engine, slate simulation
  ownership/         conditional logit, Dirichlet draws, field, logger
  optimize/          CP-SAT lineups, contest payouts, ROI and portfolio
tools/diagnose_sim.py   simulator realism battery
tests/                  84 offline tests
```

## Docs

`docs/DESIGN.md` covers the modelling decisions in more depth — why the
Markov simulator, why Monte Carlo belongs at the field level rather than the
ownership level, and the bugs found along the way that are worth not
reintroducing.
