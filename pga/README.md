# PGA DFS for DraftKings

Hole-by-hole tournament simulation, projected ownership, a simulated
opponent field, and a lineup portfolio chosen by expected ROI rather than by
projected points.

Python, under `pga/`. Built for the 2026 BMW Championship at Bellerive, but
nothing outside `cli.py` hard-codes an event.

## Quick start

```bash
cd pga
uv sync --extra dev

uv run pgadfs slate                 # the field, salaries and talent
uv run pgadfs project               # projections, finish odds, ownership
uv run pgadfs optimize --out dk.csv # 20 lineups, ready to upload
uv run pgadfs diagnose              # check the simulator against known numbers
uv run pytest                       # 60 tests, all offline
```

Everything runs from a snapshot shipped in the package, so it works with no
network. Add `--live` to pull current data, or `uv run pgadfs fetch` to
refresh the snapshot. Nothing needs a login or an API key.

## Where the data comes from

DraftKings' web app is authenticated; the JSON endpoints behind it are not.
`api.draftkings.com` gives the contest, its payout schedule, and every
golfer's salary from a contest id alone -- no CSV export. (`--salaries
DKSalaries.csv` still works as a fallback.)

DataGolf's API needs a paid key, but several of its public tools ship their
whole payload inside the page as `JSON.parse('...')`, with the
subscriber-only *columns* blanked rather than the rows. That leaves:

| page | what survives |
|---|---|
| `datagolf-rankings` | true skill in strokes gained per round, 500 players |
| `course-fit-tool` | per-golfer fit for Bellerive, in strokes |
| `fantasy-projections` | projected DraftKings ownership, score SD, tee waves, weather |
| `betting-tool-finish` | win / top-5 / top-10 / top-20 prices, DataGolf's model and the books |
| `historical-dfs-data/sample` | one real event of DraftKings scoring, itemised |

Two of those are load-bearing. On the fantasy page the projected points and
names are masked outside DataGolf's top five, but `dk_id` survives on every
row and joins straight to DraftKings' `draftableId` -- so **ownership**, the
input that is genuinely hard to source independently, comes through intact
for all fifty golfers while the projections, which this package builds
itself, do not. And the historical sample is real DraftKings output broken
into hole scoring, each bonus and the finish component, which is what the
scoring rules are checked against.

## How it works

Four layers. The interface between them is a single `(n_sims, n_golfers)`
matrix of DraftKings scores.

### 1. Simulation, hole by hole

DraftKings golf is not scored on strokes. A birdie is +3 and a par is +0.5,
so two rounds of 68 can differ by six fantasy points depending on how they
were made, and three of the four bonuses are properties of the *sequence* of
holes rather than the total. A model of round scores has to bolt those on
with a fudge factor. Simulating eighteen holes gets them for free, and
correlated with the score the way they actually are.

Each hole is a proportional-odds categorical over `{eagle or better, birdie,
par, bogey, double bogey or worse}`, with one set of cutpoints per par, a
difficulty offset per hole, and a latent shift per golfer per round.
Proportional odds is the right shape for a reason that can be checked
against tour data: a better golfer does not convert bogeys into pars
uniformly, they make more birdies *and* fewer doubles, and the effect is
bigger in the tails than in the middle.

Scoring variance is decomposed into a persistent week-long form component
shared by all four rounds, round-to-round noise, and a shock shared by
everyone in a tee wave. The first is what decides who wins: four independent
rounds spread out far less than a real 72-hole total does.

A cumulative logit is a latent logistic cut at the thresholds, so the draw
is one logistic variable per hole and a `searchsorted` per par type rather
than four sigmoids and four comparisons. Fifty golfers, four rounds, 20,000
simulations takes about six seconds.

### 2. Calibration

Three things have to be right, and none of them is guessed.

**The stroke scale.** A golfer DataGolf rates one stroke per round better
has to average one stroke per round better in simulation. Solved by
bisection against a Gauss-Hermite integral over the round noise; the fitted
scale is linear to within 1% across the whole talent range in the field.

**How the course plays.** Fitted against DataGolf's published DraftKings
projections for the five golfers it does not mask -- the only unambiguous
anchor available, because it is denominated in exactly what this package
produces. Taking DataGolf's `predicted_score` at face value instead puts
every projection thirteen points too high, which is about a birdie a round.

**The spread.** How often the best golfer in a field actually wins is not a
free parameter, it is priced, and both DataGolf's model and the sportsbooks
publish it. Fitting two parameters -- a talent multiplier and the week-long
variance -- to the sorted win / top-5 / top-10 / top-20 curves takes about
three minutes and is cached.

DataGolf masks names on the odds page, so those prices cannot be joined to a
golfer. They do not need to be: what the simulator has to reproduce is the
*shape* of the field's win distribution, and that comparison is
identity-free, as long as the ordering itself is not also being fitted. It
isn't -- the ordering comes from the skill ratings.

The fit puts the talent multiplier at 1.16: the market believes this field
is about 16% more spread out than DataGolf's general skill ratings say,
which is what you would expect of ratings compressed by the strength of the
opposition an elite no-cut field has been facing.

### 3. Ownership and the field

DataGolf publishes projected ownership for every golfer, so the level does
not have to be modelled. Three things do.

**It has to be possible.** Ownership projections are made one golfer at a
time and nothing forces them to describe a set of *lineups*. Expected lineup
salary is exactly `sum(ownership x salary)`, and that has to fit inside the
cap -- for any distribution over legal entries, no exceptions. DataGolf's
BMW projections imply $50,512 against a $50,000 cap, so no field of legal
lineups has those marginals. They are pulled onto the constraint with a
single exponential tilt in salary -- the Kullback-Leibler projection onto
it, so the adjustment a golfer takes depends on nothing but what he costs,
and golfers on the same salary keep their relative ownership exactly. It is
not a small adjustment at the top: Scheffler goes from 24.9% to 17.6%.

**It is uncertain.** Ownership is drawn from a Dirichlet, which preserves
the constraint that ownership sums to six roster spots and gets the sign of
the correlation right -- if the chalk comes in low, that ownership went
somewhere else. Blocks of the field are drawn under different samples, so
the uncertainty lands in the field rather than being averaged away.

**Marginals are not enough.** Tournaments are decided by which lineups the
field actually built, and six golfers under a cap combine in a very
particular way. So 35,294 opponent lineups get built, golfer by golfer, with
the ones that would strand the budget removed at each pick. A third of them
are drawn as the best of six by projection, standing in for the part of the
field that uses an optimizer. Sampling weights are then fitted by iterative
proportional fitting until the field's realised ownership matches the
projection, to a root-mean-square error of 0.4 points against the mean --
about 1.8 in the pipeline, where the Dirichlet draws deliberately spread it
back out again.

Sampling freely and filtering on salary afterwards does not work here, and
the reason is worth knowing: entries spend better than 98% of the cap, fewer
than one in two hundred ownership-weighted draws lands in that window, and
the survivors are not a random sample. A lineup containing the $14,400
golfer is far likelier to be rejected than one without him, so he shows up
at a tenth of his real ownership and the fit spends its time fighting the
sampler instead of the projection.

### 4. Lineups

Candidates come from CP-SAT under randomized objectives: each solve
maximises the total under one sampled simulation, so the pool contains
lineups that win in different futures rather than slightly worse versions of
the same chalk. Every candidate is then priced against the simulated field
through the contest's own payout curve, and the portfolio is chosen greedily
by *marginal* value -- a candidate is charged for the places it takes off the
lineups already selected, which is the only reason twenty entries are worth
less than twenty times one entry.

Two things stop that from being a machine for measuring luck:

**The candidate pool and the pricing use disjoint simulations.** Otherwise a
lineup that happens to look good on the draws it was optimised against gets
evaluated on those same draws.

**The payout curve is smoothed before lineups are ranked on it.** First place
is $200,000, a lineup reaches it about three times in a hundred thousand
simulations, and pricing that from a few thousand draws is a lottery rather
than an estimate: one lucky win adds tens of dollars per simulation to a
candidate's mean, far more than any real difference between candidates. The
smoothed curve replaces each prize with a geometric mean over neighbouring
ranks, preserving the pool exactly and leaving the curve steep -- first is
still 150 times thousandth -- while giving up the pretence of telling first
from fourth. Before smoothing, the chosen portfolio's measured edge was
+500% where it was chosen and roughly nothing on fresh draws. After, it is
+62% and +53%.

`optimize` prints both numbers every run. Believe the second.

## Validation

`pgadfs diagnose` checks the simulator against numbers it was never fitted
on. The bonus rates are compared against a real DraftKings scoring file --
the 2021 PGA Championship, a hard week, so the model should sit a little
above it at Bellerive:

| metric | simulated | 2021 PGA (real) |
|---|---|---|
| birdies or better per round | 3.30 | |
| eagles per round | 0.134 | |
| aces per par-3 round | 0.0005 | |
| doubles or worse per round | 0.43 | |
| bogey-free rounds | 2.9% | 1.2% |
| rounds with a 3-birdie streak | 6.9% | 4.6% |
| hole scoring points per round | 14.31 | 13.85 |
| 72-hole total SD (strokes) | 6.92 | |

Against prices, on the sorted field:

| | simulated | DataGolf | sportsbooks |
|---|---|---|---|
| best golfer, win | 18.3% | 17.5% | 20.5% |
| best golfer, top 20 | 84.9% | 83.1% | 84.8% |
| fifth best, win | 4.4% | 4.1% | 3.8% |

And against DataGolf's projected DraftKings score SD, which is published
unmasked for every golfer on the slate -- fifty independent checks on the
variance the model was not fitted to:

**mean 18.10 vs 18.07, correlation 0.917, RMSE 0.72.**

Two rules DraftKings does not document are settled from the real scoring
file rather than inferred. Every finish-position value in `config.py`
reproduces the real output exactly, and ties take the points for the top of
the band -- a T2 is 20, not the 19 that averaging would give.

## Things to know before trusting the output

**The scoring constants.** They agree across two independent secondary
sources and every finish value is confirmed against real DraftKings output,
but DraftKings' own rules page is a client-rendered SPA and could not be
read directly. Check `config.py` against it before staking money.

**Absolute ROI is soft, and not for the reason you would expect.** The
obvious suspect is the simulated field being too weak, so it was measured:

| `sharp_share` / `sharp_pool` | ROI where chosen | ROI held out |
|---|---|---|
| 0.00 / 1 | +66.5% | +59.3% |
| 0.35 / 6 | +62.2% | +53.3% |
| 0.60 / 10 | +62.7% | +53.8% |
| 0.85 / 20 | +59.9% | +54.5% |

Turning the field from random entrants into hard optimizers moves the
answer by five points. That is not a bug, it is golf: once the ownership
marginals are held fixed, mean lineup score is linear in them, and there is
no correlation between golfers for a sharper opponent to exploit -- no
stacking, no game environment, six independent players. How the field
*combines* the golfers it is on barely matters. What the field is *on* is
everything.

Which puts the whole of a reported ROI on one thing: whether these
projections are better than the ownership they are being levered against.
That cannot be checked from inside the package -- the same model produces
both the projections and the scores they are graded on, so any error in it
shows up as edge. Treat +50% as "this portfolio is well-levered against the
projected field", not as a forecast of returns. Rankings between candidate
lineups are much more robust than the level.

**The ownership tilt is consequential.** Making DataGolf's projections fit
under the cap moves the most expensive golfer by seven points of ownership,
which is enough to change whether he is worth being overweight on. If you
think real entries spend closer to the cap than
`OwnershipConfig.target_lineup_spend` assumes, that number is where to say
so.

**The model disagrees with the market on individual golfers, by
construction.** Talent ordering comes from DataGolf's skill ratings; the
market has Åberg, Schauffele and Young higher and Fleetwood, Burns and
Fitzpatrick lower. The variance fit uses the market's *shape* but cannot use
its ordering, because the odds page masks the names. Where the projections
sit above DataGolf's own, that disagreement is the reason.

**Bellerive's hole difficulties are a shape, not a measurement.** The PGA
Tour has not been here since 2018. `config.BELLERIVE.hole_offsets` is a
plausible spread that sums to zero; the level is calibrated, the spread is
assumed. Swap in real hole averages once rounds are in the books.

**No late swap.** DraftKings golf locks the whole lineup at the first tee
time, so the wave model matters only through weather, and the forecast for
Thursday is benign -- 6 mph, no rain. In a windy week, raise
`SimConfig.wave_sd` and set `wave_edge`.

## Layout

```
pgadfs/
  config.py          scoring constants, roster rules, the course, every tunable
  slate.py           the field, joined across sources
  pipeline.py        end to end, including the held-out ROI check
  data/              DraftKings and DataGolf clients, name joining, snapshots
  sim/holes.py       the proportional-odds hole model
  sim/engine.py      four rounds, vectorized across simulations and golfers
  projections/       talent to strokes, and the calibration
  ownership/         projected ownership, feasibility, the opponent field
  optimize/          CP-SAT lineups, payout curve, ROI, portfolio
  tools_diagnose.py  the realism battery
tests/               60 offline tests
docs/DESIGN.md       why the modelling decisions went the way they did
```
