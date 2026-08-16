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
pytest                      # 92 tests, all offline
```

A real slate comes straight off DraftKings' API — no CSV export needed:

```bash
mlbdfs contests --date 2026-08-16                        # what slates exist
mlbdfs contests --date 2026-08-16 --draft-group 152178   # screen its contests

mlbdfs project  --draft-group 152178
mlbdfs optimize --draft-group 152178 --contest-id 193832694 \
                --lineups 20 --out lineups.csv
```

`--draft-group` pulls salaries, positions, handedness, injury status and each
team's probable starter in one request. `--contest-id` replaces the synthetic
payout shape with the contest's *published* payout table, which is what makes
the ROI numbers mean anything. A salary CSV still works via `--slate`, but it
carries none of that, and in particular it does not carry injury status —
the export lists the whole 40-man and an IL bat is a guaranteed zero.

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

Pricing one portfolio into many contests costs roughly one evaluation, not
one per contest: ranking against the field is the expensive part and does
not depend on the payout curve, so `compare_contests` computes it once.

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

**Measured on 498 real team-games** of walk-forward backtesting:

| | value |
|---|---|
| projected nine that actually start | **7.55 of 9** |
| naive baseline (yesterday's card) | 6.89 |
| stronger baseline (modal nine of last ten) | 7.11 |
| batting-slot error, among correct picks | 0.76 |
| vs RHP / vs LHP | 7.66 / 7.32 |

So the handedness conditioning and recency weighting are worth about
+0.44 players over a decent hand-agnostic baseline. Lineups against
left-handers are genuinely harder, which is where managers make changes.

**The ceiling is the important part.** `start_probability` is reasonably
calibrated, but it tops out near 0.94 -- even the model's most confident
players fail to start 6% of the time. There is no such thing as a lock
before the card is posted:

| projected start probability | actually started |
|---|---|
| ≥ 0.80 | 90.5% |
| ≥ 0.90 | 93.5% |
| ≥ 0.95 | 93.7% |

That discount is not cosmetic. On a real 8-game slate it moves the mean
projected starter from 7.18 points to 6.17 — and until recently
`resolve_lineup` overwrote it with 1.0, so the whole model was inert on
exactly the slates it exists for. Anything you compare a discounted
projection against has to be discounted too; see the field-calibration note
in `docs/DESIGN.md`.

At roughly 13 points per zero, rostering eight hitters at 0.90 costs about
half a zero per lineup against waiting for confirmation. That is real but
survivable; it is not nothing. Set `OPTIMIZER.min_start_probability` (0.80
leaves about five playable hitters per team-game) to decline the uncertain
ones.

Still wait for the lineups when you can. This narrows the gap; it does not
close it, and it cannot know about a scratch announced an hour before lock.

## Picking the contest

Which tournament you enter is a separate decision from which lineups you
build, it is made before the slate starts, and on DraftKings' current MLB
lobby it is worth more than most lineup tweaks. `mlbdfs contests` scores
every contest on a slate.

**Rake is the one certain lever.** An exactly-average player captures their
fair share `1/N` of the prize pool and loses the rake, so breaking even
means capturing `1 / (1 - rake)` times fair share. DraftKings prices that
by buy-in, and the gradient is steep and monotone — measured across the
guaranteed Classic MLB contests in one lobby pull:

| buy-in | contests | median rake | must capture |
|---|---|---|---|
| ≤ $0.50 | 6 | 15.9% | 1.189x fair share |
| $1 | 8 | 15.9% | 1.189x |
| $2–5 | 17 | 15.7% | 1.187x |
| $6–20 | 12 | 15.0% | 1.176x |
| $21–100 | 32 | 11.4% | 1.129x |
| $101–1,000 | 12 | 9.8% | 1.109x |
| > $1,000 | 3 | 5.7% | 1.060x |

Playing at $20 instead of $1 hands back about 5 points of edge before a
single lineup is built. That is not an argument for playing above your
bankroll — it is an argument for knowing what the cheap seats cost.

**Overlay is the only free money.** A guaranteed pool is paid whether the
contest fills or not, so a contest that locks under-filled has a *negative*
rake. `overlay_now` reports the shortfall at the current fill; it only
means something in the last hour, because almost everything fills.

**Payout shape decides what kind of edge gets paid.** `breakeven_rank`
folds rake and shape into one number: multiply your finishing rank by it
and you break even. On the same slate, a double-up asks for 0.87 (finish
13% higher up the field than chance) and the top-heavy Bat Flip asks for
0.76. The shape assumption behind it — that skill lifts you a constant
*fraction* of the way up the field — is the one least kind to tournaments,
because a real MLB GPP edge is concentrated in the right tail where
correlated stacks live. So read `breakeven_rank` as the bar for a generic
edge, and let simulated ROI against the actual payout curve
(`optimize --contest-id`) settle the top-heavy ones.

**`edge_sharpe` is the bankroll warning.** It is expected profit over its
standard deviation per entry; roughly `1/sharpe²` entries are needed before
the edge clears one standard error. For the $150K Bat Flip that is about
85,000 entries. The edge can be real and still invisible for a season.

**Max entries per user is the field-strength proxy.** In a 150-max contest
a handful of professionals field thousands of lineups and blanket the
sensible roster space; in a single-entry contest they get one bullet each.
The screen reports the rule and deliberately does not put a number on the
effect — that number is not in the lobby data.

Once the portfolio exists, the shape metrics stop being the best available
answer. `optimize --compare-contests N` prices the same lineups through each
contest's real payout table:

```bash
mlbdfs optimize --draft-group 152178 --contest-id 193832694 \
                --compare-contests 10 --max-fee 30
```

The pattern that shows up is the one the shape metrics predict. Small
single-entry fields give a per-entry win probability around 1.2–1.4% against
0.14% in the big top-heavy ones, but only a third of the ROI — the big
fields pay far more for the same tail. Ranking is by ROI rather than total
expected profit, since profit is largest wherever the buy-in is largest,
which says nothing about which contest was the better place to stake it.

One caveat that matters more on small slates than large: `evaluate_lineups`
assumes no ties, so pot-splitting is unpriced. On a two-game slate the
winning lineup is frequently duplicated and first place is split several
ways, which the ROI figure does not know about.

## Things to know before trusting the output

**Verify the scoring point values.** The roster rules are now confirmed
against DraftKings' own game-type endpoint -- ten slots, $50,000 cap, two
games and two teams minimum, five hitters per team. The *scoring* values are
not: DraftKings renders that table client side and exposes no JSON for it,
so those numbers still come from secondary sources. A wrong constant there
silently corrupts every layer above it and no test can catch it.

**Absolute ROI is soft; relative ROI is the signal.** ROI depends on how
strong the simulated field is, which is set by the ownership model — see
below. Rankings between candidate lineups are far more trustworthy than the
ROI numbers themselves.

Measured against a real 35,671-entry contest, the generated field comes in
16 to 23 points low at every quantile from the median out to the 99.9th,
with roughly the right spread. Some of that is a scale mismatch — the real
contest locked with posted lineups while an early run discounts every hitter
by his chance of not starting — and some of it is the projection shortfall
in the next section. Either way, the ROI *level* is not a number to act on.

**Ownership is now fitted, on one slate.** The weights come from maximum
likelihood against a real 35,671-entry contest rather than from guesses, and
against the old hand-set values on that contest: mean absolute error
0.0222 → 0.0115, correlation with realized ownership 0.22 → 0.81, error on
the twenty chalkiest plays 0.155 → 0.081. Holding out whole position groups
reproduces the coefficients, so they transfer across positions — but one
slate cannot show whether they transfer across *slates*. Re-fit as contests
accumulate:

```bash
mlbdfs fit-ownership --features slate_features.parquet --entries 35671
```

Two of the original guesses were qualitatively wrong. `salary` was set
negative on the theory that the field is price averse; the fitted sign is
strongly positive — the field pays up. `value` was the dominant term at
1.85; fitted, it is almost exactly zero. What the field buys is ceiling and
price, not bargains.

Value-seeking still shows up in the output, but as a *consequence of the
salary cap* rather than a preference: expected lineup salary has to fit
under $50,000, and the tilt enforcing that is what makes cheap productive
players popular.

Two related invariants worth knowing, both checkable:

- `implied_field_mean` — ownership-weighted projection, the expected score
  of a field lineup.
- `expected_lineup_salary` — ownership-weighted salary. Above the cap, no
  distribution over legal lineups can produce those marginals, and the field
  generator will discard nearly everything it builds.

Set `target_field_mean_score` from the average score
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
tests/                  92 offline tests
```

## Docs

`docs/DESIGN.md` covers the modelling decisions in more depth — why the
Markov simulator, why Monte Carlo belongs at the field level rather than the
ownership level, and the bugs found along the way that are worth not
reintroducing.
