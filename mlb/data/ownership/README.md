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
| MLB $500 Solo Shot (193887495) | 2026-08-16 | 152178 | 594 | settled |

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
