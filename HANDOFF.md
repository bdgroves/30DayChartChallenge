# Handoff: #30DayChartChallenge

For a new Claude chat picking this up. `git pull --rebase` first.

## State (Nov 12, 2026)
- `2027/` is set up for April 2027, ported from `bdgroves/30DayMapChallenge` (`2026/toolkit`):
  `toolkit/dcc.py` (house style, with chart axes; `axes=False` for map-like days), `build.py`
  (READMEs, days.json, thumbs, share pages, card), `post.py` (X + Bluesky), `render.py`,
  `index.html` (gallery, reads days.json), `days.template.yml` (30 placeholder days).
- Workflows: `dcc-2027-build.yml` (on push to 2027/), `dcc-2027-post.yml` (April mornings,
  posts only `status: done`; preview via workflow_dispatch).
- Brooks decided (Nov 12, 2026) to wait for the official prompts before planning any charts.
  A scheduled task checks for them each Monday in March 2027.
- `2024/` and `Death_Valley_Ecosystems.ipynb` are older experiments; leave them.

## 2026 (done Nov 12, 2026, for fun, after the fact)
- `2026/`: all 30 official 2026 prompts made, status done, never posted (posting is 2027 only).
- Each day: `make.py` (house style via `2026/toolkit/dcc.py`), data in `data/`. Days needing the web have a `fetch.py`
  run by `dcc-2026-fetch.yml` on Actions (this workspace can only reach GitHub).
- Substitutions, honestly labeled on the charts: day 14 uses UN Comtrade (Census API now needs a key); day 30 uses UN WPP
  2024 via the wpp2024 R package, catalogued in GHDx (IHME's own GBD files need a login); day 19 is US electricity because
  Plate Hunter's WA data lists 30 plates at a placeholder year of 1996.
- Data stops at the end of 2025, as if made in April 2026: geocaches, quakes, sea ice, Mono Lake and Paradise are cut at 2025;
  RSF is the 2025 index; Zillow compares 2016 and 2025 yearly averages; Whitman is the 2025 season (fetched by day 27's fetch.py).
- Day 27 also writes `out/animation.gif`. Day 28 models the 2025 NCAA tournament from the regular season and marks the real result.

## To do
- [ ] When the prompts are out: `days.template.yml` → `days.yml` with the prompts, then plan with Brooks.
- [ ] Posting keys in this repo's secrets before April 1.
- [ ] GitHub Pages for the gallery at brooksgroves.com/30DayChartChallenge/2027/ (main, root).
- [ ] Lessons from the map challenge: GitHub's scheduled runs start late, so posting retries every
      30 minutes; the backup scheduled task idea (one per morning) worked well in November 2026.
