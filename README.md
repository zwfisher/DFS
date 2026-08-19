# DFS

## Projects

**`mlb/`** -- MLB DFS for DraftKings: plate-appearance projections, a base-out
Markov game simulator, simulated ownership, and a stack-aware lineup
optimizer ranked by expected ROI. See `mlb/README.md`.

**`pga/`** -- PGA DFS for DraftKings: hole-by-hole tournament simulation,
calibrated against market prices and real DraftKings scoring output, with a
simulated opponent field and an ROI-ranked lineup portfolio. See
`pga/README.md`.

## NFL scripts (older work, R)

**calc_dfs_points.R** contains the code to calculate DFS points given the play-by-play data from nflfastR package

**fantasylabs_ownership_scraping.R** collects historical ownership data from fantasylabs.com's API. This data is not very clean and is incomplete, but rotogrinders API has been giving 403 errors so they may have shut off public access to that endpoint.

**script.R** contains all of the main data cleansing and organization code as well as a few weak attempts at modeling ownership.

**vegas_data.R** merges & cleans up the vegas over/under data from https://www.sportsbookreviewsonline.com/scoresoddsarchives/nfl/nfloddsarchives.htm
