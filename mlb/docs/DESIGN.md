# Design notes

Why the project is built the way it is, and what was learned building it.
The README covers what it does; this covers the reasoning, including the
places where measurement contradicted the plan.

---

## The central decision: simulate games, not players

MLB DFS tournaments are won by stacks. A stack is a bet on correlation, so
any model that cannot represent correlation cannot price the thing the game
is actually about.

The straightforward approach — project each player's mean and variance, then
add noise — gets stacks systematically wrong. Five independent bets have a
much thinner right tail than five correlated ones at the same mean, so an
independent model will consistently rate a five-stack below a diversified
lineup that projects the same. That is backwards.

So the simulator runs an actual base-out Markov chain per team-game, walking
the lineup batter by batter through the 24 base-out states. This produces:

- **Runs and RBI as emitted quantities.** They are the hardest thing to
  project directly, because they depend on teammates. Here they are simply
  what happens.
- **Correlation with the right shape.** Teammates score together because
  they literally bat in the same innings. The dependency is generated, not
  assumed, and it has the correct non-linear structure — the correlation is
  strongest exactly in the high-scoring tail that tournaments pay for.
- **Pitcher anticorrelation for free**, since the opposing pitcher is scored
  in the same pass.

Measured: a five-stack's 99th percentile is about 14% higher than the same
five players decorrelated, at an identical mean.

### Correlation is lower than intuition suggests

Same-team hitters land around **+0.10** pairwise, not the +0.25 one might
guess. This is real and worth internalizing: DraftKings scoring is dominated
by discrete events, and a home run is ten points. Each hitter's own
Bernoulli noise swamps a good deal of the shared game state.

Two shared per-game shocks were added after measuring correlation without
them, because the base-out chain alone produced only ~+0.09:

- **Starter form.** A pitcher has a true talent level *and* a day. When he
  does not have it, all nine opposing hitters benefit at once. This is the
  dominant real-world driver of stack correlation.
- **Game environment.** Weather, umpire, conditions — shared by both
  lineups, which is what makes opposing hitters mildly positively correlated
  and gives game stacks a statistical basis.

Both are discretized into a handful of quantile bins so the rate lookup
table stays a few hundred rows instead of one row per simulation.

### Why it is fast

The performance idea is to vectorize **across** simulations rather than over
them. All N simulations of a game advance through the same plate appearance
together as numpy arrays; sims that finish a half-inning early are masked
out rather than compacted. The Python loop therefore runs about 200 times
per game instead of N times. A 15-game slate at 20,000 sims takes 14
seconds.

Two details that matter more than they look:

- Points are scattered into per-slot accumulators via flat fancy indexing.
  Within one plate appearance each simulation appears at most once, so the
  indices are unique and plain `+=` is correct — and several times faster
  than `np.add.at`.
- Runner *identity* is tracked (batting slot per base), not just occupancy,
  because runs have to be credited to whoever actually scores.

---

## Where Monte Carlo belongs in the ownership problem

The original question was whether to run a Monte Carlo to simulate
ownership. The answer is yes, but not at the step it sounds like.

**Not for the point estimate.** Ownership is not a random process. It is a
near-deterministic function of things visible before lock: salary, projected
points, points per dollar, Vegas team total, batting order, opposing pitcher
quality, and how many alternatives exist at the position. That is a
regression problem.

**The model must add up.** Total ownership at a position equals the number
of roster spots there — 300% across outfielders, 100% at catcher.
Independent per-player predictions violate this and produce incoherent
slates. A conditional logit satisfies it by construction and encodes that
ownership is *competitive*: chalk on one catcher necessarily suppresses
every other catcher.

**Monte Carlo earns its place one level up, twice:**

1. **Uncertainty in ownership itself.** Being wrong about ownership is a
   first-order tournament risk — a leverage play that arrives 30% owned is
   not a leverage play. Dirichlet draws preserve the sum constraint and the
   negative correlation: if the chalk comes in low, that ownership went
   somewhere else rather than evaporating.

2. **The field.** Ownership marginals cannot tell you what to do, because
   tournaments are decided by joints. Two slates with identical ownership
   demand different lineups depending on whether the field's exposure is
   concentrated in five-stacks or scattered. So the field is generated as
   actual lineups under DraftKings rules, then calibrated by iterative
   proportional fitting until its marginals reproduce the projection.

### Field strength is set by ownership, not by the sampler

This one was learned the hard way. An attempt was made to sharpen the field
by generating several candidate lineups per opponent and keeping the
best-projecting one. It did essentially nothing, and the reason is an
identity:

> Because ownership sums to the roster slots, the expected projection of a
> field lineup equals `Σ(ownershipᵢ × projectionᵢ)`.

The field's mean strength is therefore **pinned by the ownership model**. No
sampling mechanism can change it while still matching the marginals. A
per-player value term cannot work either — the calibration cancels it
exactly. The knob was removed rather than shipped as a lever that does
nothing.

What the sampler *does* control is shape: which players appear together, how
often a five-stack shows up. That is the part worth tuning, and it is tuned.

Field strength is instead controlled by a **temperature** on the logit
utilities, targeted at an observable: the average score in the contests you
actually enter. Note that field strength is **not monotone** in temperature
— the utility is dominated by points per dollar, so extreme concentration
piles ownership onto cheap high-value players whose absolute projections are
low, and strength falls again. It peaks in the middle. The calibration
searches a grid for this reason, and reports when a target is unreachable.

### Ownership projections need not be jointly feasible

`implied_field_mean` (the identity above) and the field the generator
actually builds can differ, because there may be no way to fill legal
rosters that puts every chalk play at its projected rate — the salary cap
will not allow it. The heuristic model prices each player independently and
nothing enforces slate-wide affordability.

It is tempting to conclude the identity is therefore an upper bound. It is
not: the calibration residual leaves error in both directions and the
realized field regularly comes out slightly above. Use it as a reference
point.

A model fitted to real contest data would not have this problem at all,
being feasible by construction.

---

## Optimizer

CP-SAT rather than CBC through PuLP, and the reason is entirely
performance: PuLP writes an LP file to disk and spawns CBC as a subprocess
on every solve, costing about two seconds regardless of how easy the model
is. Building a few hundred candidates that way takes twenty minutes. CP-SAT
runs in process at roughly 150ms — a 14 to 20 times speedup on identical
constraints. Models are cached per stack shape and only the objective is
swapped.

Two formulation choices worth keeping:

- Hitters-versus-own-pitcher is one aggregated constraint per pitcher, not
  one per (pitcher, hitter) pair.
- The games-represented indicator needs only its upper bound. It cannot be
  forced up without a player from that game, and the `>= 2` requirement is
  what pushes it.

**Diversity comes from randomized objectives, not overlap constraints.**
Each solve maximizes points under one sampled draw from the simulation — a
world where this pitcher was sharp and that lineup got to the bullpen early
— so the pool naturally contains lineups that win in different futures.
Overlap limits produce lineups that differ but have no particular reason to
be good. A few draws are averaged into each "world" so the solve is not
chasing one lucky home run while still preserving the correlation structure.

**The objective is expected ROI, not points.** Candidates are scored across
every simulation, ranked against field lineups scored in the *same*
simulation, and paid through the contest's payout curve. On a typical slate,
lineups selected by ROI project *fewer* points than lineups selected by
projection and win two to four times as often. That gap is the entire
argument for the ownership and field layers.

---

## Bugs worth not reintroducing

Each of these produced output that looked entirely plausible.

**Field lineups scored with the wrong players.** `Field.lineups` holds
indices into the ownership frame, which comes out sorted by ownership, but
they were used to index the simulation's player order. Every opponent lineup
was scored from the wrong players. Scoring now goes through an explicit
remap and there is a test pinning it.

**Evaluating on the training simulations.** The candidate pool is built by
optimizing against sampled draws; scoring those candidates on those same
draws measures how well the optimizer fit its own noise. The pipeline now
runs an independent holdout simulation. Measured bias turned out modest here
(rank correlation 0.98) but that is a property of this setup, not a
guarantee.

**Payout bands summing to 197% of the prize pool**, making the rake −67%.
Bands are now relative weights normalized to the pool, so rake is correct at
any contest size.

**League rates summing to 1.03.** Everything downstream renormalizes, so
this silently deflated every non-out outcome by 3% and cost half a run per
game. There is now an assertion at import.

**A hook model with no lookahead.** Checking the pitch limit only after an
inning completes means a starter always finishes the inning that crosses his
limit, running two thirds of an inning deep. The manager is deciding whether
to send him back out, so the comparison is against the count he would finish
the *next* inning on.

---

## Zeros, and why start probability is modelled

The first real contest run produced poor results, and the standings file
explained why with unusual clarity.

A hitter who does not start scores zero. So does one who goes hitless with
nothing else, and the simulator already priced that correctly — a
league-average starter posts an empty line 21.6% of the time here against a
theoretical ~20%. What the model did *not* price was the chance the player
never took the field at all.

The evidence that this dominates everything else:

| | top 200 | field |
|---|---|---|
| mean zero-scoring players | 0.47 | 2.13 |
| share with no zeros | 60.5% | 10.1% |
| share with 3+ zeros | 0.5% | 36.7% |

Each zero cost about 13 points of mean score and moved median finish by
roughly 5,500 places in a 35,671-entry field. The zero-rate by ownership
tier confirms the mechanism rather than blaming variance: hitters owned
under 2% posted zeros 62% of the time, three times the rate for a starter
who simply had a bad night. Those players were not in the lineup.

The old `resolve_lineup` did three things wrong when no lineup was posted,
and its docstring called the result "a crude but surprisingly effective
proxy", which the data does not support:

1. It took each team's nine highest-salary hitters and treated all nine as
   **certain** starters, assigning zero probability to the single most
   damaging outcome available.
2. It assigned batting order by **descending salary**. Real leadoff hitters
   are frequently cheap contact-and-speed players while the expensive bats
   hit second through fourth, so plate appearances were misallocated at both
   ends — and the consecutive-order stack constraint was then optimizing
   over a fictional lineup card.
3. Any hitter outside the top nine by salary never entered the simulation at
   all, so a cheap genuine starter was invisible to the optimizer.

The fix carries `start_probability` on each player, set to 1.0 when a lineup
is posted and estimated from salary rank otherwise, and the simulator draws
against it per simulation. This is the honest shape of the uncertainty: it
lowers the projection, roughly doubles the modelled chance of a zero, and
widens the downside exactly where the losses came from. Salary rank now maps
to batting slots through `SALARY_RANK_TO_ORDER`.

The replacement bat is not re-simulated when a player is scratched — the
other eight keep their run environment. That understates a second-order
effect and is worth far less than pricing the zero at all.

None of this makes running early correct. `run_pipeline` refuses a slate
that is mostly unconfirmed unless explicitly overridden, because the right
answer is to wait for the lineups.

## What to do next

In rough order of expected value:

0. **Run after lineups post.** No modelling change substitutes for it.
1. **Log real ownership.** Everything about the ownership layer improves
   with data, and DraftKings contest standings are exact and free for
   contests you entered. `mlbdfs log-ownership` writes them to a training
   store. This is the single highest-value thing to start now, because it
   only accumulates with time.
2. **Fit the conditional logit** on that data and swap it in behind the same
   interface, then fit the Dirichlet concentration from realized residuals
   rather than a prior.
3. **Backtest calibration.** `sim.engine.calibration_report` produces PIT
   values against realized scores; a flat histogram means the intervals are
   honest, U-shaped means too narrow. Worth running over a month of slates
   before trusting the tails.
4. **Weather.** Temperature and wind are real second-order park effects and
   the park factor structure already has a place for them.
5. **Platoon splits.** The blending function exists in `rates.py` but is not
   wired into the live data path.
6. **Reached-on-error and pinch hitting**, the two known simulator gaps.
