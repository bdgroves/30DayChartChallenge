"""Day 1 · Part-to-Whole: 1,190 geocache finds as a waffle, one square each, by cache type."""
import sys
from pathlib import Path
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "toolkit"))
import dcc
import pandas as pd
from matplotlib.patches import Rectangle

df = pd.read_csv(HERE / "data" / "finds.csv")
counts = df["type"].value_counts()
NAMES = {"Unknown Cache": "Mystery cache", "Multi-cache": "Multi-cache", "Traditional Cache": "Traditional",
         "Virtual Cache": "Virtual", "Earthcache": "EarthCache"}
main = ["Traditional Cache", "Virtual Cache", "Earthcache", "Multi-cache", "Unknown Cache"]
groups = [(NAMES[t], int(counts[t])) for t in main] + [("Everything else", int(counts.drop(main).sum()))]
colors = [dcc.LAKE, dcc.GOLD, dcc.SAGE, dcc.LAVA, "#8a5a83", dcc.INK]
rest = counts.drop(main)

COLS, ROWS = 35, 34
assert COLS * ROWS == len(df) == sum(n for _, n in groups)
fig, ax = dcc.figure("portrait", axes=False, box=(0.07, 0.25, 0.86, 0.55))
cells = [c for (_, n), c in zip(groups, colors) for _ in range(n)]
for i, c in enumerate(cells):
    col, row = i % COLS, i // COLS
    ax.add_patch(Rectangle((col, ROWS - 1 - row), 0.84, 0.84, color=c, lw=0))
ax.set_xlim(-0.2, COLS); ax.set_ylim(-0.2, ROWS); ax.set_aspect("equal")

# legend: name, count and share, under the waffle
y0 = 0.205
for k, ((name, n), c) in enumerate(zip(groups, colors)):
    x = 0.07 + (k % 3) * 0.30
    y = y0 - (k // 3) * 0.05
    fig.patches.append(Rectangle((x, y - 0.006), 0.016, 0.0128, transform=fig.transFigure, color=c, lw=0))
    fig.text(x + 0.026, y, name, family=dcc.TEXT, size=10.5, color=dcc.INK, va="center")
    fig.text(x + 0.026, y - 0.019, f"{n:,} · {n / len(df):.1%}", family=dcc.MONO, size=8.5, color=dcc.STONE, va="center")
SMALL = {"Event Cache": "event", "Locationless": "locationless", "Letterbox Hybrid": "letterbox hybrid",
         "Project APE": "Project APE", "Groundspeak HQ": "Groundspeak HQ", "Wherigo Cache": "Wherigo",
         "Webcam Cache": "webcam", "CITO Event": "CITO event"}
other = ", ".join(f"{n} {SMALL.get(t, t)}" for t, n in rest.items())
dcc.frame(fig, 1, subtitle=f"Every geocache I've found, {df['found'].min()[:4]}–{df['found'].max()[:4]}, one square each.\n"
          "Five in six are the classic kind: a container, a logbook, a set of coordinates.",
          source="My geocaching.com finds", note=f"Everything else: {other}.")
dcc.save(fig, 1, alt=(f"A waffle chart of {len(df):,} squares, one per geocache found. "
                      + "; ".join(f"{name}: {n}" for name, n in groups) + "."))
