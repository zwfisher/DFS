# Design notes

Why the modelling went the way it did, and the mistakes worth not making
again. The README says what the package does; this says what it costs to
believe it.

## Why holes and not rounds

The obvious model is a distribution over each golfer's DraftKings score:
DataGolf even publishes a mean and a standard deviation per golfer, and a
lognormal through those two numbers would take an afternoon.

It cannot work, because DraftKings golf scoring is not a function of the
score. Consider two rounds of 68 on a par 70:

| | birdies | pars | bogeys | hole points |
|---|---|---|---|---|
| A | 6 | 10 | 2 | 18 + 5 - 1 = 22.0 |
| B | 2 + 1 eagle | 15 | 0 | 6 + 8 + 7.5 - 0 = 21.5, plus 3 bogey-free |

Same score, different points, and the gap widens once the streak bonus and
the all-four-rounds-under-70 bonus are in play. Three of the four bonuses
are properties of the *sequence* of holes. A round-level model has to
estimate them as a function of the score, which means estimating a
conditional expectation that is exactly the thing a hole-level model
produces for nothing.

The cost is a factor of eighteen in the inner loop. That is bought back in
[Sampling](#sampling-an-ordered-logit-without-sigmoids).

## Why proportional odds

The hole model needs one knob -- how good the golfer is -- to move a
five-category distribution. Proportional odds moves all four cutpoints
together on the log-odds scale, which has the property the data actually
shows: improvement is concentrated in the tails. A golfer a stroke a round
better than the field does not convert bogeys to pars uniformly; he makes
noticeably more birdies *and* noticeably fewer doubles, and barely changes
his par rate.

It is also invertible in the direction that matters. `HoleModel.expected_strokes`
is analytic, so the stroke scale and the course difficulty can be solved by
bisection instead of by simulating and hoping.

The assumption it makes is that one latent dimension is enough -- that a
golfer cannot be simultaneously more likely to make birdies and more likely
to make doubles than the shift implies. Real golfers can be, and volatile
players are worth more in a tournament than their mean says. Against the
2021 PGA scoring file the model comes out slightly *generous* on both
bonuses rather than short, so there was no evidence to justify adding a
second latent dimension for it. If a second one is ever added, the place is
a round-level effect that shifts the good cutpoints and the bad cutpoints in
opposite directions; the sampler already splits them by par type and could
split them by tail with two `searchsorted` calls instead of one.

## Sampling an ordered logit without sigmoids

The naive draw evaluates four cumulative probabilities per hole and compares
a uniform to each: for fifty golfers, four rounds and 20,000 simulations
that is 288 million sigmoid evaluations.

A cumulative logit is a latent logistic variable cut at the thresholds. Draw
the latent directly, and the category is the count of thresholds below it.
There are only three distinct threshold vectors -- one per par -- so the
whole categorical draw is three `searchsorted` calls over sorted length-four
arrays, and the per-hole difficulty comes out of the thresholds and into the
latent where it costs one subtraction. Six times faster, and it made the
variance calibration (dozens of full simulations inside a Nelder-Mead loop)
practical rather than overnight.

## What the calibration is anchored on, and what it is not

Anchors, in descending order of how much they should be trusted:

1. **Real DraftKings output.** The historical sample settles the finish
   table and the tie rule outright. No inference.
2. **DataGolf's published DK score SD**, unmasked for all fifty golfers.
   Fifty checks on the variance structure, none of them fitted to.
3. **Sorted win / top-N prices**, from DataGolf's model and the books.
   Identity-free, so they constrain the shape of the field's win
   distribution but not any individual golfer.
4. **DataGolf's five unmasked DK projections.** The only thing denominated
   in output units, so the scoring level is fitted to them -- but five
   points, and the residual RMSE of 3.1 is mostly this model and DataGolf's
   disagreeing about who is good, not about how the course plays.

Not anchored on anything: Bellerive's hole-by-hole difficulty (no tour
rounds since 2018), and the strength of the simulated opponent field.

### The `predicted_score` trap

DataGolf's fantasy page carries `predicted_score: -1.0` for the event, and
the natural reading -- the field's expected score to par -- is wrong. Using
it puts every DraftKings projection about thirteen points above DataGolf's
own for the same golfers, which is roughly a birdie a round. Fitting the
scoring level to the projections instead lands at +0.53, meaning Bellerive
plays about 70.5 for this field.

The general lesson: when a source publishes both an intermediate quantity
and a final one, fit to the final one. The intermediate is defined by
someone else's pipeline and the definition is not in the payload.

## Ownership has to add up twice

The first constraint is the familiar one: across a six-golfer slate, total
ownership is 600%. A Dirichlet respects it; independent per-golfer noise
does not, and quietly invents a contest that could not exist.

The second is less familiar and was the single biggest bug in this package.
Expected lineup salary is `sum(ownership x salary)` -- exactly, for any
distribution over lineups, because expectation is linear and each golfer
contributes his salary with probability equal to his ownership. So that
number has to be inside the salary cap. DataGolf's BMW projections give
$50,512 against a $50,000 cap.

Until that was fixed, the field builder was being asked for something
impossible and did what optimisers do when the feasible set is empty: it
oscillated. Iterative proportional fitting drove the most expensive golfer
from 2% to 24% to 14% to 20% and never settled, and the best it could manage
was five points of ownership error on the chalk. Correcting the projection
first -- one exponential tilt, the KL projection onto the constraint --
dropped the field's ownership error to 0.4 points and made the fit converge
in a few passes.

The correction is not cosmetic. It moves the most expensive golfer by seven
points of ownership, which is enough to change whether he is a leverage play
or a trap. `OwnershipConfig.target_lineup_spend` is where to argue with it.

## Why the field is sampled sequentially

Sampling six golfers by ownership and rejecting the ones that miss the
salary window is correct in principle and useless in practice. At this event
the acceptance rate was 0.5%, and -- worse -- the rejections are not a random
sample. A lineup containing the $14,400 golfer needs the other five to
average $7,120, so it is far likelier to be thrown out than one without him;
the survivors had him at a tenth of his projected ownership, and the fit
that was supposed to correct that spent its iterations fighting the sampler.

Drawing golfer by golfer, with the ones that would strand the budget removed
before each pick, takes the acceptance rate to 99% and the whole field build
from five minutes to thirteen seconds. The feasibility test uses the
cheapest and most expensive remaining tails computed globally rather than
per lineup, which is a relaxation -- so the exact window is still checked at
the end, and it rejects about 1%.

## Why the payout curve is smoothed

This was the last bug and the most instructive one. The reported ROI of the
chosen portfolio was +500%. It should have been obvious that a portfolio
projecting 1% above the field average cannot return five times the stake.

Two things were wrong. The candidate pool and the ROI evaluation shared
simulations, so selection was partly fitting noise -- fixed by splitting the
draws. But the larger problem survived the split: first place is $200,000, a
lineup reaches it about three times in a hundred thousand simulations, and
the evaluation ran on a few thousand. One lucky win adds $133 per simulation
to a candidate's mean. Selecting the maximum over 400 candidates therefore
selects, almost entirely, the candidates that happened to win a simulation.
Their measured edge evaporated on fresh draws.

More simulations is not the answer -- pricing a 3e-5 event to useful
precision needs six figures of them, and the field matrix is already
gigabytes. The answer is to stop claiming resolution the estimator does not
have. Smoothing the payout curve over a multiplicative window in rank keeps
the economics (first is still 150 times thousandth, the pool is preserved
exactly) and drops the pretence of distinguishing first from fourth. In-
sample +62%, held out +53%: a gap worth reporting, rather than a number
worth ignoring.

The general lesson is worth stating on its own, because it applies to any
tournament model: **the variance of an expected-value estimator is set by the
most extreme prize, not by the average one.** A selection step that
maximises such an estimator is a machine for finding the luckiest sample
unless the objective is deliberately blunted.

`pipeline.run` reports both numbers on every run. The gap between them is
the diagnostic.

## The draw cancels, and it took building it to know that

The instinct that tee times matter is right, and at this event the answer
turns out to be "in round one, by a fifth of a stroke, and then not at all".
Worth writing down *why*, because the reasoning generalises and the number
does not.

Three things have to line up for a draw to be worth anything over 72 holes:

1. **The days have to differ.** Round two reverses round one off a single
   tee, so a golfer who gets the good half of Thursday gets the bad half of
   Friday. What survives is not the shape of either day's curve, it is the
   *difference* between them. Thursday and Friday here have nearly identical
   drying curves, so 0.244 and 0.254 strokes of within-day spread net out to
   0.132 across the pair and about 0.01 once the weekend is included.
2. **Something has to actually vary.** Wind is the variable that moves tour
   scoring, and it never gets above six miles an hour on any of the four
   days -- below the threshold where it does anything measurable. The entire
   remaining signal is greens firming as humidity falls from 97% to 62%,
   which is real but small.
3. **The field has to be split.** Fifty golfers in twosomes off one tee is a
   five-hour ramp, not two waves. Modelling it as a binary flag would have
   thrown away most of the structure and then found nothing anyway.

The layer is still worth having, for the same reason a thermometer is worth
having on a mild day: the answer "this is worth nothing" is only useful if
it was measured. `pgadfs conditions` prints it, and on a week at a links
course in October it will print something else.

The weekend mechanism is the one piece with no analogue in a normal model.
Rounds three and four are paired off the leaderboard with the leaders out
last, so the tee sheet is *endogenous*: leading after two rounds buys you
the firmest greens. It cannot be precomputed, because it depends on the
simulation, so the engine resolves it per simulation from the running
leaderboard. Here it is worth about a tenth of a stroke to the leaders and
changes nothing. At a venue with a real afternoon wind it would be a
systematic drag on exactly the golfers whose finish position pays the most
DraftKings points, which is not a small thing to have structurally absent
from a model.

## Course fit is small, course history is smaller

It is tempting to make the venue do a lot of work, because "who fits this
course" feels like the question with the most edge in it. Measured, it is
not.

**Course fit** at Bellerive spans ±0.09 strokes a round across the field --
McIlroy and Woodland at one end, Si Woo Kim and Eric Cole at the other. Over
72 holes that is about a third of a stroke, or a point and a half of
DraftKings scoring. It is real, it is worth including, and it is roughly a
tenth the size of the talent spread it sits on top of.

The fit is rebuilt from the venue's skill weights rather than lifted from
DataGolf's summary column, and then rescaled so its spread matches theirs.
That is deliberate: DataGolf fits those weights properly, against results,
and this does not, so their magnitude is the one to trust. What the rebuild
buys is the decomposition -- being able to say that McIlroy's fit is driving
distance and nothing else, and that Justin Thomas's negative fit is entirely
around-the-green skill that this course will not pay him for. The two agree
at r = 0.87, which is the right amount of agreement for a reconstruction:
close enough to trust, different enough to be worth printing.

**Course history** is smaller still, and the interesting part is that
DataGolf's own shrinkage says so. Seventeen of the fifty have played here,
almost all four rounds in 2018. The cap on the adjustment is 0.16 strokes a
round and the largest value in this field is 0.032. It is in the model
because it is free and correctly shrunk, not because it is expected to do
anything. Anyone quoting Gary Woodland's T6 from 2018 as a reason to play
him is quoting 0.026 strokes a round.

**The par-type profile is the one venue input that earns its place**, and it
is the one nobody talks about. Bellerive's par 5s played 0.10 strokes harder
than a typical tour par 5 in 2018 -- long, and not the birdie holes the
field is used to. At an identical stroke total that costs about 0.3
DraftKings hole-scoring points a round, because the birdies it removes are
worth +3 each and the pars it substitutes are worth +0.5. Over four rounds
that is more than the entire course-fit adjustment for anyone in the field,
and a model that works in strokes cannot see it at all.

## Known gaps

- **Course fit is thin.** DataGolf's Bellerive adjustment spans ±0.09
  strokes a round across the field, so it moves projections by well under a
  point. Course history is not used at all -- there is none worth using.
- **No within-round leaderboard effects.** Golfers do not press when they are
  behind on Sunday, and the leader does not play conservatively. Both are
  real and both mostly affect the tails of the finish distribution, which is
  where the DraftKings finish bonus lives.
- **The forecast is hourly, so the tee sheet quantises.** Golfers going off
  inside the same hour get identical conditions. At a quarter of a stroke of
  spread this is invisible; in wind it would be worth interpolating.
- **Weekend tee times are assumed, not published.** The leaderboard pairing
  is modelled; the hour it starts is a guess, and with a fifty-man field in
  twosomes the whole sheet fits inside four and a half hours, so it barely
  matters.
- **Conditions coefficients are assumed.** There is no per-event conditions
  data on the free pages to fit wind or firmness against, so every number in
  `ConditionsConfig` is a documented prior with a `scale` knob over it.
- **The opponent field has no multi-entry structure.** A 150-max contest
  contains people entering 150 correlated lineups, which changes the shape
  of the top of the leaderboard. Every simulated entry here is independent.
- **Ownership is a projection, not a fit.** There is no historical
  DraftKings golf ownership in the package, so nothing is fitted --
  DataGolf's numbers are taken and made feasible. Contest standings are
  downloadable after the fact, and enough of them would let the ownership
  layer be fitted for real behind the same interface.
