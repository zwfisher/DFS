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

**Arizona had no lineup history, on every slate.** Statcast spells the club
`AZ` and DraftKings spells it `ARI`, so the join found nothing and all nine
Arizona hitters silently fell back to the salary-rank guess. It looked
exactly like a team whose lineup had not posted. Both sides now go through
the same normalizer.

**A bullpen game cost the opposing team its whole lineup projection.**
Probable starters were matched only against pitchers DraftKings flags `SP`,
but an opener or a converted reliever is listed `RP` and is still starting.
Missing him does not merely lose that pitcher — the *other* team's projected
lineup is conditioned on his throwing hand, so nine hitters lose their
batting order. Two of sixteen teams on a real slate hit this.

**The start-probability model was switched off in every run that needed
it.** `resolve_lineup` forced `start_probability = 1.0` on every hitter
carrying a batting order. That is correct for a posted order and wrong for
a projected one, and `projected_lineups.apply_to_slate` assigns a batting
order to projected starters too — so the entire zero-risk model, validated
over 498 team-games and documented with a calibration table, was overwritten
before the simulator ever saw it. It only applied on slates run *after*
lineups post, which are exactly the slates that do not need it.

Nothing looked wrong: the salary-rank fallback still discounted the slots
the projection had not filled, so some hitters did carry a probability
below one. Measured on a real 8-game slate, the fix moves the mean projected
starter from 7.18 points to 6.17 — a 14% overstatement of every guessed
hitter. `resolve_lineup` now keys on `Player.confirmed`.

**The seed did not reproduce the run.** Two identical invocations produced
different portfolios, which was noticed only because a refactor was being
checked against a previous run and the numbers moved in the third decimal.
CP-SAT with eight workers on a wall-clock limit returns different
equally-good lineups; `max_deterministic_time` does not fix it and only
`num_workers = 1` does, which was tested rather than assumed. The default
stays fast — a different draw from a sampled pool is not a worse one — but
it is now a named option rather than a surprise.

**Vegas totals filtered on the wrong calendar.** A slate date is an Eastern
date; The Odds API stamps `commence_time` in UTC, where a night game rolls
over. Comparing the UTC prefix to the slate date therefore dropped every game
starting at or after 8pm ET. On draft group 152195 that was 2 of 7 games — the
Cubs/White Sox at 20:06 ET and, more expensively, Dodgers at Coors at 20:41 —
so four teams silently took the league-average run environment on the very
slate the integration was added for. Nothing in the output says so: the run
prints a count of teams it *did* price and the rest look ordinary.

The related trap is that the count printed is the size of the odds frame, not
the number of *slate* teams matched. Those differ whenever the API's day
includes a game the slate does not, which on this slate it did.

**The upload file used the wrong id for every flexible player.** The
draftables feed carries a separate draftable id per eligible roster slot, so
`upload.py` looked up `(player, slot)` and wrote the id belonging to the slot
the optimizer had assigned. Reasonable, and wrong: DraftKings' own
`DKSalaries.csv` entry template lists exactly **one** id per player — the
lower — and instructs you to paste it into whichever position you want.
Checked against draft group 152195, all 61 multi-slot players are listed
under their lowest id and the per-slot alternate appears nowhere in the
file. Ohtani in an outfield slot was being written as `43854284`, an
identifier the entry page never offers.

The failure mode is the worst kind: it only touches multi-position players,
which is a minority of the pool but a majority of what a stacked lineup
leans on for salary flexibility, and a rejected import gives no indication
which row was at fault. `canonical_draftable_ids` now returns the lowest id
per player, and `--template` fills the user's own file so every id written
is one that file already lists.

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

## Projected lineups, and why handedness is the whole model

Lineups post one to three hours before first pitch. Projecting them earlier
is worth doing, but only if it is done conditionally: **managers platoon**,
so the useful question is not "who has started recently" but "who starts
against a left-hander". A modal lineup taken from ten games against righties
is confidently wrong about precisely the players a platoon decides, and
those are the ones worth knowing about.

The estimator is recency-weighted by *games* rather than days -- an off day
should not age a lineup -- and every quantity is computed against the
probable starter's hand, then shrunk toward the player's own overall rate.
Start probabilities are normalized to sum to nine, since exactly nine
hitters start. That is the same "it has to add up" discipline the ownership
model uses, for the same reason.

No new data source was needed. The starting nine are the first nine distinct
batters a team sends up in a game, and Statcast carries `p_throws` on every
pitch, so both the lineups and the handedness fall out of the Statcast pull
the projections already make.

### The shrinkage prior was set by measurement, and the result is lopsided

Sweeping the hand prior against synthetic history with known platoon
structure gives a clear asymmetry. When a team genuinely platoons, a heavy
prior is about five times worse than a light one; when it does not platoon,
a heavy prior is only marginally better. Missing a real platoon means
rostering someone who does not play, which the contest data prices at about
13 points. Imagining a platoon that is not there only mis-weights two
players who both might start. So the prior is light.

Two things that were assumed and turned out to be false, both caught by
writing tests:

* **The prior changes selection, not just confidence.** Shrinkage is toward
  each player's *own* overall rate rather than a shared constant, so it is
  not a common monotone transform across players and the projected nine can
  change with it. The everyday core is stable; the platoon spot is exactly
  what moves.
* **The denominator has to respect availability.** Charging every player for
  every team game meant a hitter promoted ten games ago was charged with the
  twenty before he was on the roster. Because a short recent stretch is
  often lopsided by pitcher hand, the hand-specific denominator was where it
  bit hardest, and an everyday call-up read as a part-timer -- 0.40 where it
  should have been 0.94. The denominator now runs back only to a player's
  first appearance.

### Handedness applies twice

Which hitters are in the lineup is one question; how they hit the arm they
are facing is another, and the second is worth more per plate appearance.
Both are now wired: `blend_platoon` shrinks a hitter's split toward his own
overall line rather than league average, because platoon skill is largely a
league-level effect and individual splits are small samples.

Worth noting `blend_platoon` had been written but never called, and it was
broken -- it relied on merge suffixes for a column name that did not
collide. Dead code is untested code.

### Validate it before trusting it

`mlbdfs lineup-accuracy` runs walk-forward: for each team-game, project
using only prior games, then compare against who actually started. On
synthetic history it lands around 8.5 of nine with slot error under 0.1, and
accuracy against left-handed starters holds up against right-handed ones,
which is the check that the platoon conditioning is doing its job.

### What it actually scores on real data

Backtested over 498 real team-games: **7.55 of nine**, against 6.89 for
"yesterday's card" and 7.11 for a hand-agnostic modal nine of the last ten.
The conditioning is worth about +0.44 players over a decent baseline, which
is real but a long way from the 8.5 the synthetic fixture suggested. The
synthetic generator is far simpler than a manager, and it flattered the
model by about a full player.

Re-tuning on real data changed one default and confirmed another. The hand
prior held up -- anything between 0.5 and 1.0 performs the same, and it
degrades above 3, exactly as the synthetic sweep said. The recency half life
did not: 4.5 games beats the 12 originally chosen by about 6% of Brier.
Lineups churn faster than a season-long view suggests.

The finding that matters most is the ceiling. `start_probability` is
reasonably calibrated but tops out near 0.94: players the model is most
confident about still fail to start 6% of the time, and tightening the
threshold from 0.90 to 0.95 buys nothing (93.5% to 93.7%). There is no such
thing as a lock before the card is posted. At roughly 13 points per zero
that is about half a zero per lineup against waiting, which is why
`OPTIMIZER.min_start_probability` exists -- the practical control is to
decline the uncertain players rather than to try to be more certain.

## Fitting ownership, and what the guesses got wrong

The hand-set weights were replaced by maximum likelihood against a real
35,671-entry contest. The likelihood is the one the sum-to-slots structure
implies: within a position group the counts across players are multinomial,
so it is a conditional logit weighted by how many entries rostered each
player.

Against the original guesses on that contest, mean absolute error went
0.0222 to 0.0115, correlation with realized ownership 0.22 to 0.81, and
error on the twenty chalkiest plays 0.155 to 0.081. Holding out whole
position groups reproduces the coefficients — held-out error 0.0272 against
in-sample 0.0270 on hitters — so they transfer across positions. They are
still from one slate and cannot be shown to transfer across slates.

Two guesses were wrong in kind, not degree:

* **Salary.** Set at -0.10 on the theory that the field is price averse.
  The fitted coefficient is strongly positive. The field pays up for good
  players rather than hunting bargains.
* **Value.** Set at 1.85, dominating every other term. Fitted at
  approximately zero. Points per dollar has essentially no influence once
  projection and ceiling are in the model.

Value-seeking does still appear in the output, and the reason is worth
stating: it is an emergent consequence of the salary cap, not a preference.
An average lineup has to fit under $50,000, and the tilt enforcing that is
what makes cheap productive players popular. The original model had the
mechanism backwards -- it encoded as taste what is actually a constraint.

### Two feasibility identities

Because ownership sums to the roster slots, two ownership-weighted sums are
not statistics but identities about the field:

* `implied_field_mean` -- ownership-weighted projection is the expected
  score of a field lineup.
* `expected_lineup_salary` -- ownership-weighted salary is the expected
  salary of a field lineup, and above the cap **no distribution over legal
  lineups can produce those marginals**.

The second only started to bite once the fitted weights made salary
attractive; the price-averse guesses kept projections affordable by
accident. Without it the field generator discarded 99% of what it built and
returned 16 lineups out of 1500. Ownership is now tilted against salary by
a single coefficient, solved to bring expected lineup salary just under the
cap, applied on top of the fitted utilities and renormalized per group.

### The metric that hid a broken fit

A ridge penalty of 1.0 produced a model with better mean absolute error and
correlation of 0.74 that was nonetheless useless: it put the chalk at 5-9%
against a realized 20-35%. Shrinking coefficients flattens the softmax
toward uniform, ordering survives so correlation holds, and error improves
because most players on a slate are owned near zero and predicting near zero
is close for them. The chalk breaks, and the chalk is the part that matters.
`implied_field_mean` caught it -- 50.8 against 76.5 from real ownership --
which is what a feasibility identity is for.

## Contest selection is a separate optimization

Everything above optimizes the lineup given a contest. The contest itself is
a decision made earlier, with better information and no simulation required,
and on the current DraftKings MLB lobby it moves expected return further
than most lineup changes do.

The arithmetic is trivial and worth stating precisely because it is easy to
carry a vague version of it. Let `s` be the fraction of the prize pool an
entrant captures and `N` the field size. Expected return per entry is
`(1 - rake) · s · N`, so breaking even requires capturing `1 / (1 - rake)`
times an equal share. Rake is a fixed multiplicative drag on whatever edge
the rest of the project produces; nothing downstream can recover it.

DraftKings prices rake by buy-in, monotonically, from 15.9% at a dollar to
5.7% above a thousand. Measured, not assumed — `mlbdfs contests` computes it
from the lobby's own numbers, and the lobby figure agrees with the published
payout bands to the cent on every contest checked.

### The lobby field names are a trap

`m` is the maximum field size and `nt` is entries so far. Read the wrong way
round, rake inverts and every screen output is confidently backwards. There
is a test asserting the $15K mini-MAX comes out at DraftKings' published
15.9%, which is the cheapest possible guard against that.

### What a payout curve is actually for

Two contests with identical rake are not equivalent. A flat double-up pays
for clearing a threshold; a top-heavy tournament pays almost entirely for
the top thousandth. `breakeven_rank_factor` collapses rake and shape into
one comparable number by asking what proportional rank improvement pays for
the rake: 0.87 for a double-up on this slate, 0.76 for the $150K Bat Flip.

That model assumes skill lifts an entrant a constant *fraction* of the way
up the field, which is the assumption least favourable to tournaments,
because the edge this project builds is deliberately tail-concentrated —
correlated stacks buy right-tail outcomes specifically. So the number is the
bar for a *generic* edge, and the honest way to settle a top-heavy contest
is to run the portfolio against its real published curve, which
`optimize --contest-id` now does.

`edge_sharpe` is the counterweight, and it is the number most likely to
change behaviour: roughly `1/sharpe²` entries are needed before the edge is
one standard error from zero, which for the Bat Flip is about 85,000. An
edge can be real and remain invisible for a full season.

### The field-strength gap, and two wrong explanations for it

A target of 98.8 — the mean entry score in a real 35,671-entry contest —
cannot be reached by the ownership model. On the 8-game slate of
2026-08-16 the reachable range is 66.0 to 82.0 and
`calibrate_to_field_strength` clamps to 82.0. The generated field sits 16 to
23 points below that contest at every quantile from the median to the
99.9th, with roughly the right spread (sd 27.1 against 30.6).

Two explanations were offered for this and both were wrong. They are
recorded because each is superficially reasonable and the second is the
kind of mistake that survives a long time.

**Wrong answer one: a units mismatch from start probability.** The theory
was that a settled contest's average is built from posted batting orders
while an early run discounts every hitter by his chance of not starting, so
the two scales cannot meet. The discount is real and large — 7.18 projected
points per starter against 6.17 — but it is not this. Rerun on the same
slate with lineups posted and 14 of 16 teams confirmed, the reachable range
moved from 69.1–82.6 to 66.0–82.0. Essentially unchanged.

**Wrong answer two: the field generator builds weaker lineups than real
opponents.** Superficially compelling: the optimized candidates project 98
to 102, straddling the real 98.8, while the sampled field projects 82, and
real entrants do spend the cap and stack while an ownership sampler does
not. But an identity rules it out. Ownership sums to the number of roster
slots, so the mean score over *any* field whose marginals match the
projection is exactly the ownership-weighted projection sum — joints cannot
move a mean. How the generator assembles lineups is therefore incapable of
producing this gap.

That the optimized lineups land near 98.8 is not reassurance either. Those
are the best 20 of 194 candidates; a field's *average* entry should score
well below a tuned lineup, not level with it. It is another statement of
the same anomaly.

**What is actually left.** The identity applies to real ownership too, and
that measurement exists: ownership read directly off the 193621106
standings gives an implied field mean of 76.5 against a realized 98.8. With
observed ownership and the identity holding exactly, only two possibilities
remain — the projection level is roughly 20% low, or that slate scored
about 20% above expectation.

One slate cannot separate those, and this is the point at which to say
plainly that the earlier claim of the shortfall being "seen twice,
independently" was wrong: the backtest, the quantile comparison and the
98.8 target are three views of the same contest, and a hot slate produces
all three. Settling it needs contests from other slates, which is now the
highest-value open item in the project. Until then absolute ROI is not a
number to act on; the ranking between candidate lineups is.

### Vegas totals, finally measured

`scale_to_team_total` existed from the start and no run had ever supplied it a
number, so every team on every slate carried the league-average run
environment. With a key configured, draft group 152195 (7 games, 2026-08-17)
run with and without `--no-odds` at 8,000 sims and a common seed:

| team | Vegas total | hitter points, odds | no odds | change |
|---|---|---|---|---|
| LAD | 6.24 | 83.85 | 74.66 | **+12.3%** |
| COL | 5.26 | 59.42 | 55.61 | **+6.9%** |
| ARI | *(none)* | 66.78 | 66.45 | +0.5% |
| CHC | 4.46 | 59.49 | 59.51 | −0.0% |
| DET | 4.04 | 58.32 | 59.71 | −2.3% |
| NYM | 4.07 | 57.00 | 59.01 | −3.4% |
| PIT | 3.96 | 50.58 | 52.69 | −4.0% |
| SD | 3.93 | 49.95 | 52.70 | −5.2% |

Simulated team runs move the same way: LAD +1.04, COL +0.49, SD −0.31. Four
teams had no line posted (books had not put up KC/ATH or BOS/ARI a day out)
and moved by −0.9% to +0.5%, which is the noise floor and a free control on
the measurement.

The effect on **stack preference is larger than the effect on levels**:
Spearman between the two stack-ownership vectors is 0.72, against 0.92 for
team points. Colorado moves from the 7th most-stacked team to the 2nd, San
Diego from 10th to last, Detroit and the White Sox each drop three places.
LAD was already the top stack and its stack ownership more than doubles, 0.86
to 1.96. So the totals do not merely re-level the slate — they change what you
would build.

Two cautions the measurement itself raised.

**Partial coverage is not neutral.** An unpriced team keeps the league-average
4.45, and on this slate the median real total was 4.06, because one Coors game
pulls the mean up while eight of ten teams sit below it. The unpriced teams
therefore float upward for no reason at all: Athletics 14th to 9th, Kansas City
12th to 8th, on no information. Better to know which teams are priced than to
read the ranking as if the slate were uniformly covered.

**This slate understates the `FAVOURITE_RUN_SHARE` problem.** Five of six
games had totals between 8.0 and 8.625, so nearly all the dispersion came from
the game total rather than the favourite split. Excluding Coors, the implied
totals span only 3.80 to 4.46 — a 0.66-run band across ten teams, where a real
MLB card runs closer to 3.3–5.5. At the measured 0.49 the same slate would
span 3.51 to 4.68, a band 77% wider, with the largest single-team move being
0.53 runs at Coors. The compression is real; how much of it is the constant
and how much is a genuinely bunched card takes a second slate to separate.

### Stacking, and the experiment that nearly overturned it

Stacking is the project's founding assumption, and the football analogy
that motivates it does not transfer cleanly: there is no
quarterback-to-receiver link in baseball, and measured same-team hitter
correlation is only about +0.10. `tools/stacking_value.py` therefore tests
it directly, holding everything constant except team concentration.

The result replicates across a real slate and the synthetic fixture:
stacking costs roughly a point of mean and buys 4–5% at the 99.9th
percentile. It then wins tournaments by 15–35% and loses flat double-ups by
14–15%. The loss is the more consistent figure, and the more useful one:
it says plainly that the stack shapes are a tournament setting, not a
universal one.

The mechanism worth internalising is that +0.10 is a *small* correlation
and a top-heavy payout curve is a large amplifier. Nothing about the tail
gain is dramatic; what is dramatic is a contest that pays only for the
99.97th percentile.

**The methodological lesson is the more valuable half.** An early version
of this experiment ran 40 candidates at 3,000 simulations and reported
stacking losing in all three contests — the opposite sign, in every cell.
It was underpowered: 40 candidates leave 37 after de-duplication, the best
eight of 37 is a thin selection, and P(win) near 0.001 is barely resolvable
in 3,000 draws. That result was believed long enough to be written up as a
contradiction before the full-fidelity run reversed it.

Two rules follow. Any ROI comparison in this project needs at least the
tool's defaults (120 candidates, 8,000 simulations, a 12,000-lineup field),
and a result that overturns a core assumption deserves a power check before
it is acted on, not after.

### The field-strength gap is real: measured clean, it is about 19%

Three contests now, on three different slates, and the cleanest of them
settles it.

| contest | slate | entries | implied | realized | gap |
|---|---|---|---|---|---|
| 193621106 | 151927, 8/12 | 35,671 | 76.5 | 98.8 | −22.6% |
| 193887495 | 152178, 8/16 | 594 | 84.7 | 101.1 | −16.2% |
| **193891712** | **152195, 8/17** | **7,134** | **92.3** | **114.5** | **−19.4%** |

The third is the one that counts, because its features were captured
*before lock* — `project --out` at 17:30 ET against a card that was 12 of 14
posted — rather than reconstructed from a finished slate. Everything the
earlier two were criticised for is absent: rostered players projecting 0.00
carry **0.173 of 9.96 roster slots**, against 1.28 on 8/16. Drop them
entirely and the gap moves from −19.4% to −19.0%. The artifact explains
essentially none of it.

**This corrects the conclusion recorded here after the 8/16 contest.** That
analysis found two thirds of an apparent 16.2% shortfall was batting orders
lost to rebuilding a finished slate, and concluded that "the second slate
does not reproduce a 20% projection shortfall" and that the residual was
about 6%. The contamination was real and the arithmetic was right, but the
inference was wrong: removing the lost players also removed precisely the
players whose projections were most wrong, and the surviving 6% was a
property of that subset rather than an estimate of the gap. Measured where
no subsetting is needed, the gap is ~19% — consistent with the original
−22.6%, not with the −6% that replaced it.

So the projection level really is roughly a fifth low, and it has now been
seen on three slates, two contest structures and field sizes from 594 to
35,671. `target_field_mean_score` is not a nuisance parameter to be avoided;
it is absorbing a genuine and stable bias, and until the projections
themselves are re-levelled it should be set from the realized mean of
contests actually entered.

What is *not* yet known is where the shortfall lives. The obvious candidates
are the per-plate-appearance rates, the plate-appearance counts the Markov
chain generates, and the DraftKings scoring constants — which are still
unconfirmed and would move every projection by a constant factor if wrong.
That last one is cheap to check and would explain the stability of the bias
better than anything else.

### The coefficients do not transfer across slates

The open question since the ownership model was fitted was whether weights
estimated on one slate carry to another. Holding out whole position groups
reproduced them, but that only shows transfer across *positions* within one
slate. A second fittable slate now exists -- 8/17, features captured pre-lock
-- and the two fits disagree in kind, not degree:

| feature | hitters 8/12 | hitters 8/17 | pitchers 8/12 | pitchers 8/17 |
|---|---|---|---|---|
| value | 0.01 | **0.85** | −0.40 | 0.11 |
| proj | 0.29 | 0.41 | **−0.62** | **+0.82** |
| ceiling | 0.46 | 0.31 | 2.31 | 0.58 |
| team_total | 0.32 | 0.19 | 0.00 | 0.11 |
| salary | 0.45 | 0.05 | 0.06 | 0.06 |

`value` was the term the 8/12 fit specifically overturned -- set at 1.85 by
hand, fitted to approximately zero, and written up here as "what the field
buys is ceiling and price, not bargains." On 8/17 it is the dominant hitter
term at 0.85 while salary collapses from 0.45 to 0.05. The pitcher projection
coefficient changes sign.

The 8/17 fit is not bad on its own terms. Against the current weights on that
slate it takes mean absolute error from 0.0138 to 0.0080, chalk error from
0.108 to 0.064, and correlation from 0.751 to 0.916; leave-one-position-out
holds; and the `implied_field_mean` guard that caught the ridge-penalty
failure passes at 90.9 against 91.1. It is a good fit *to that slate*.

That is the point. Two good single-slate fits that disagree on sign are
evidence about the *method*, not about which slate is right: six features and
roughly 180 rostered players carrying the signal is enough to fit a slate and
not enough to fit the game. **Do not ship a refit from one slate.** What would
settle it is several slates fitted jointly, with a whole slate held out --
which needs pre-lock feature captures accumulated over time, and is exactly
what `project --out` now exists to collect.

The 8/12 raw data no longer exists, incidentally: no standings file, no
feature frame, nothing in the repository or the cache. That fit survives only
as the numbers in `OwnershipConfig`, so it cannot rejoin a pooled fit. Slates
are only as recoverable as the files someone kept, which is the same lesson as
the pre-lock capture, arriving from the other direction.

### An unposted card is not a discount, it is a different lineup

The 8/17 build entered six Arizona hitters across twenty lineups because
Arizona was the one team whose card had not posted at lock. The
projected-lineup model gave them start probabilities of 0.92 to 0.98.
Arizona then rested its entire core:

| projected | start prob | actually |
|---|---|---|
| Corbin Carroll | 0.98 | did not start |
| Gabriel Moreno | 0.97 | started |
| Nolan Arenado | 0.97 | did not start |
| Geraldo Perdomo | 0.96 | did not start |
| Ryan Waldschmidt | 0.95 | started |
| Ketel Marte | 0.92 | did not start |

The real card was Vargas, Barrosa, Moreno, McCann, Lawlar, Tawa, Fernandez,
Nootbaar, Waldschmidt — four of the six most confident projections sat, and
the lineup that stacked five of them scored 7 points from those five and
finished 5,869th of 7,134.

The calibration table in the README says `start_probability` tops out near
0.94 and that players above 0.90 start about 90% of the time. That is an
average over ordinary day-to-day variation, and it is the wrong model for
the failure that actually happens: a manager does not independently bench
four regulars, he writes a rest-day lineup, and then the errors are perfectly
correlated. A per-player probability cannot represent that, so the portfolio
sees six independent 0.95s where the truth is closer to one shared coin.

The practical rule that follows is stronger than `min_start_probability`:
**treat a team whose card has not posted at lock as unrosterable, rather than
discounted.** Not because the per-player estimate is badly calibrated on
average, but because its errors arrive all at once and a stack converts that
into a single catastrophic outcome. On a slate where one team is unposted and
thirteen are, declining the one costs almost nothing.

### Realized ownership is usable; realized *features* are not reconstructable

Contest standings give exact ownership, and ownership is fixed at lock — it
does not move while the games play out, so even a live export carries a final
`%Drafted` column. Contest 193887495 (594 entries, draft group 152178)
confirmed both structural claims on a second slate: ownership sums to the
roster slots (0.997 per single slot, 2.990 across outfielders, 1.994 across
pitchers) and `Σ(ownership × points) = mean entry score` held to four
decimals, 70.7141 against 70.7082.

**That does not make the contest fittable.** `fit_ownership` needs the six
pre-lock features the field reacted to — `salary`, `proj`, `ceiling`,
`value`, `team_total`, `batting_order` — and after the slate starts they
cannot be recovered:

| feature | recoverable after lock? |
|---|---|
| `salary` | yes — fixed at slate creation, identical across rebuilds |
| `batting_order` | mostly, but post-lock scratches leak in |
| `team_total` | **no** |
| `proj`, `ceiling`, `value` | no — all downstream of `team_total` |

Re-pulling odds for a slate in progress returns live in-game lines: on
2026-08-16 the run printed a range of 1.2 to 7.4 implied runs, where 1.2 is a
team losing late. The Odds API's historical endpoint would fix this and is not
on the free plan (401). Falling back to `--no-odds` is not a fix either — it
makes `team_total` a constant, so its coefficient becomes unidentifiable and
the other five silently absorb its signal. Measured between the two builds,
`proj` shifts by a median 8.3% and up to 51%.

A fit on those features would look entirely reasonable and be wrong, in the
same way the ridge-penalty fit did. So: log the ownership, use it for the
identities and for diagnosing the model, and do not re-fit from it.

**The fix is operational, not analytical.** `mlbdfs project --out` writes the
feature frame; run it *before lock* and pair it afterwards with
`log-ownership --projected`. A slate is fittable only if someone captured its
features while they still existed.

The model **under-concentrates on chalk**, and this is the most reliably
replicated finding in the project:

| slate | contest | chalk error | corr | real top-20 mass | model |
|---|---|---|---|---|---|
| 8/12 | 35,671, in sample | 0.081 | 0.81 | — | — |
| 8/16 | 594, single entry | 0.166 | 0.705 | 4.88 | 2.27 |
| 8/17 | 7,134, 20-max, pre-lock | 0.109 | 0.779 | 4.34 | 2.54 |

The 8/17 row is measured against features captured before lock, so it is the
honest out-of-sample number, and it lands between the in-sample fit and the
contaminated one. The direction is unambiguous and it does not depend on
contest structure: a 7,134-entry 20-max field and a 594-entry single-entry
field both put roughly 4.3 to 4.9 of ten roster slots on their top twenty
plays where the model puts 2.3 to 2.5.

It is specifically the top of the board. Blake Snell came in at 56.2% against
a projected 23.0%, Shota Imanaga 41.6% against 14.1%, Framber Valdez 30.8%
against 13.8% — the three most popular pitchers, each under-predicted by more
than half. Temperature is the concentration knob and it does not fix it:
forcing it lower makes the chalk error *worse*, because the extra mass lands
on the wrong players. Whatever is missing is not concentration.

### The rank fix changed levels, not the ordering

`contest_rank` used to scale beaten-counts by `n_entries / field_size` and
round, which made ranks 2 and 3 unreachable on a large contest. Because the
bias grows with that ratio, the worry was that big-field contests had been
flattered relative to small ones and the *ranking* in
`optimize --compare-contests` was wrong, not just the levels.

Measured directly: one pipeline run on draft group 152195, one portfolio, one
field standing, `compare_contests` evaluated twice over 12 contests with only
`contest_rank` swapped.

| contest | entries | ratio to field | ROI inflation | P(win) ratio |
|---|---|---|---|---|
| $20K mini-MAX | 47,562 | 3.96 | **+31.0%** | **2.07x** |
| $200K Rally Cap | 29,411 | 2.45 | +18.3% | 1.43x |
| $15K mini-MAX | 17,835 | 1.49 | +7.0% | 1.09x |
| the other nine | ≤ 7,431 | ≤ 0.62 | 0.00% | 1.00x |

The inflation tracks the ratio almost exactly (correlation 0.98) and
reproduces the previously reported 32% / 2x on the contests whose ratio
matches the one those figures came from.

**The ordering did not change: Kendall tau 1.0, zero of twelve contests
moved.** Two reasons, and only the first generalizes. A contest with fewer
entries than the sampled field never enters the extrapolation branch at all —
nine of twelve here — so its ROI is identical to the last decimal. And among
the three that were affected, ROI already increased with field size, so a bias
monotone in field size widened the existing gaps rather than crossing any: the
third-placed contest gained 0.31 of ROI against a 0.83 gap to fourth.

That second reason is a property of this slate. The bias is monotone in
`n_entries / field_size`, so it can only reorder when a large-field contest
sits *below* a smaller-field one by less than its inflation. Nothing here was
close enough. Raising `--field` above the largest contest's entry count
removes the extrapolation entirely and is the cheapest way to stop having to
think about it.

The ROI *levels* in that run remain unusable for the reason above — no
`--field-mean` was supplied, so they carry the whole field-strength gap.

### What is deliberately not measured

Field strength. Max-entries-per-user is the visible proxy — 150-max
contests let a few professionals blanket the roster space, single-entry
contests give them one bullet — but the size of that effect is not in the
lobby data, and a fabricated coefficient would be worse than naming the
gap. The same applies to duplication: `evaluate_lineups` assumes no ties, so
pot-splitting is unpriced, and it is unpriced in exactly the place it hurts
most, which is a small slate where the winning lineup is not unique.

## What to do next

In rough order of expected value:

0. **Run after lineups post.** No modelling change substitutes for it.
1. **Log more contests.** The ownership fit rests on a single slate.
   Everything about it improves with more, and DraftKings standings are
   exact and free for contests you entered -- but only for those, since the
   export endpoint requires authentication. `mlbdfs log-ownership` collects
   them; `mlbdfs fit-ownership` re-fits.
2. **Fit the Dirichlet concentration** from realized residuals rather than
   a prior, which needs several slates.
3. **Re-derive the 76.5.** A second slate arrived and most of its apparent
   shortfall turned out to be batting orders lost to rebuilding a finished
   slate, leaving about 6% rather than 20%. If 76.5 was computed the same
   way it is biased down by the same mechanism, and the "projection is ~20%
   low" conclusion goes with it. Check whether the 193621106 rebuild
   projected any rostered players at 0.00; if it did, that number is wrong
   and every ROI level derived from it moves.
3. **Backtest calibration.** `sim.engine.calibration_report` produces PIT
   values against realized scores; a flat histogram means the intervals are
   honest, U-shaped means too narrow. Worth running over a month of slates
   before trusting the tails.
4. **Confirm the scoring point values.** The roster rules are verified
   against DraftKings' game-type endpoint; the scoring table is rendered
   client side and is still taken from secondary sources.
4. **Settle `FAVOURITE_RUN_SHARE`, and prefer not to need it.** One day's
   published `team_totals` puts it near 0.49 against the 0.235 in the code,
   consistently across dispersion assumptions. Two things would close this
   properly: repeat the calibration over a week of cards, and route
   `fetch_team_totals` through the per-event endpoint so a posted team total
   is used when it exists, which is what the module already says it prefers
   and cannot currently do. The second costs one API request per event
   instead of one per slate and makes the constant matter only for the games
   with no team-total market.
5. **Weather.** Temperature and wind are real second-order park effects and
   the park factor structure already has a place for them.
6. **Reached-on-error and pinch hitting**, the two known simulator gaps.
7. **A news-driven scratch feed.** The projected-lineup model is backward
   looking by construction and cannot see a late scratch. That gap needs a
   licensed feed; nothing free closes it.
