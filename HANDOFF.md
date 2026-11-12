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

## To do
- [ ] When the prompts are out: `days.template.yml` → `days.yml` with the prompts, then plan with Brooks.
- [ ] Posting keys in this repo's secrets before April 1.
- [ ] GitHub Pages for the gallery at brooksgroves.com/30DayChartChallenge/2027/ (main, root).
- [ ] Lessons from the map challenge: GitHub's scheduled runs start late, so posting retries every
      30 minutes; the backup scheduled task idea (one per morning) worked well in November 2026.
