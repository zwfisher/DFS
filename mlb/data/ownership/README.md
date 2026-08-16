# Logged contest ownership

Real ownership read off DraftKings standings exports. The ownership fit rests
on these, and the container this project runs in is ephemeral —
`mlbdfs log-ownership` writes to `~/.cache/mlbdfs/ownership_history.parquet`,
which does not survive. Anything worth keeping is exported here.

## `ownership_history.csv`

The accumulated log. Re-import with `pd.read_csv` and pass to
`mlbdfs fit-ownership`.

| contest | slate | draft group | entries | state |
|---|---|---|---|---|
| MLB $500 Solo Shot (193887495) | 2026-08-16 | 152178 | 594 | **live when captured** |

## `contest-standings-193887495-LIVE-SNAPSHOT.csv`

**The FPTS and Points columns in this file are partial.** It was exported
while the contest was still running: DraftKings reported
`contestState: Live`, and only 2 of 594 entries had `TimeRemaining == 0`. The
mean entry score in it is 70.71, which is not a final score and must not be
used as a field-strength target — doing so would bias the field mean *down*,
toward what the ownership model already predicts, and appear to resolve the
open field-strength question in the model's favour.

The `%Drafted` column *is* final. Ownership is fixed at lock and does not
move as games play out; the identity
`Σ(ownership × points) = mean entry score` holds on this file to four
decimal places (70.7141 against 70.7082), which is a property of the
ownership column, not of the games being over.

Re-export this contest once it settles to get a usable realized field mean.
Contest 193621106 (draft group 151927, 2026-08-12) is a **different slate**,
so a settled version of this one is what the field-strength gap has been
waiting for.
