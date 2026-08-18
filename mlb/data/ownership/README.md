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
| MLB $500 Solo Shot (193887495) | 2026-08-16 | 152178 | 594 | settled, post-hoc features |
| MLB $6K Solo Shot (193891712) | 2026-08-17 | 152195 | 7,134 | settled, **pre-lock features** |

## `contest-standings-193887495-FINAL.csv`

The settled export. Mean entry score **101.11**; the identity
`Σ(ownership × points) = mean entry score` holds at 101.12.

The mid-slate export of the same contest read 70.71 — 70% of final — while
its `%Drafted` column was already byte-identical to this one across all 165
rostered players. Ownership is fixed at lock; scores are not. Only the
settled file may be used for a field-strength target.

**Do not derive an implied field mean by rebuilding this slate now.** 31 of
the 165 rostered players project at exactly 0.00 in a post-hoc rebuild
because their batting orders are no longer retrievable, and they carry 1.28
of 9.97 roster slots and really scored 5.32 apiece. That single artifact
accounts for two thirds of the apparent projection shortfall. See
`docs/DESIGN.md`.

## `contest-standings-193891712-FINAL.csv`

The 8/17 contest, and the first one whose features were captured *before*
lock — `features-152195-prelock.csv`, written at 17:30 ET with 12 of 14 cards
posted. That pairing is what makes it the usable measurement:

- realized field mean **114.50**, identity holding at 114.495
- implied from real ownership x pre-lock projections **92.29**, a **-19.4%** gap
- rostered players projecting 0.00 carry **0.17 of 9.96 roster slots**, against
  1.28 on the 8/16 rebuild; dropping them moves the gap only to -19.0%

Use this pair, not the 8/16 one, for anything about the projection level.
