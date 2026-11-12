# #30DayChartChallenge 2027

One chart a day, April 1–30, 2027, on the official prompts. Same look as brooksgroves.com and the
November map challenge: parchment and ink, Playfair Display titles, DM Mono labels.

**Status:** waiting for the 2027 prompts (usually announced in March by the challenge organisers,
[github.com/30DayChartChallenge](https://github.com/30DayChartChallenge)).

When they're out:

1. Copy `days.template.yml` to `days.yml` and put each day's prompt in `theme`.
2. Push. The build Action makes the day folders (`day-NN-prompt/`), their READMEs, `days.json`
   and the gallery card. This README becomes the overview table.
3. Each day: write `day-NN-*/make.py` with `toolkit/dcc.py`, render, set `status: done`.
   The post Action shares done days to X and Bluesky each April morning.

```python
import sys; sys.path.insert(0, "toolkit"); import dcc
fig, ax = dcc.figure("square")           # square, portrait (4:5) or wide (16:9); axes=False for no axes
ax.bar(...)
dcc.frame(fig, 1, subtitle="...", source="...")
dcc.save(fig, 1, alt="Alt text for screen readers.")   # day-01-*/out/chart.png + alt.txt
```

Before April: add the posting keys to this repo's secrets (same names as the map challenge:
`TWITTER_API_KEY`, `TWITTER_API_SECRET`, `TWITTER_ACCESS_TOKEN`, `TWITTER_ACCESS_SECRET`,
`BLUESKY_HANDLE`, `BLUESKY_APP_PASSWORD`). A platform without keys is skipped.
