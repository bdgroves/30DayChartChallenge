"""Day 12 · FlowingData Theme: every cache I've found on the geocacher's 9×9 grid of difficulty and terrain,
drawn the FlowingData way: plain shapes, counts you can read, and notes in the margin that talk to you."""
import sys
from pathlib import Path
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "toolkit"))
import dcc
import numpy as np
import pandas as pd
from matplotlib.patches import Rectangle

df = pd.read_csv(HERE.parent / "day-01-part-to-whole" / "data" / "finds.csv")
levels = np.arange(1, 5.01, 0.5)
grid = df.groupby(["difficulty", "terrain"]).size().unstack(fill_value=0).reindex(index=levels, columns=levels, fill_value=0)
filled = int((grid > 0).sum().sum())
fig, ax = dcc.figure("square", axes=False, box=(0.15, 0.17, 0.53, 0.53))
mx = grid.values.max()
for i, d in enumerate(levels):
    for j, t in enumerate(levels):
        n = grid.loc[d, t]
        if n == 0:
            ax.add_patch(Rectangle((j + 0.06, i + 0.06), 0.88, 0.88, fill=False, ec=dcc.MIST, lw=0.8, ls=(0, (2, 2))))
            continue
        side = 0.25 + 0.69 * np.sqrt(n / mx)
        o = (1 - side) / 2
        ax.add_patch(Rectangle((j + o, i + o), side, side, color=dcc.LAVA if n >= 50 else "#e07b5a" if n >= 10 else "#f0b49a", lw=0))
        ax.text(j + 0.5, i + 0.5, str(n), ha="center", va="center", family=dcc.MONO, size=6.8 if n < 100 else 7.5,
                color=dcc.WHITE if n >= 10 else dcc.INK)
ax.set_xlim(0, 9); ax.set_ylim(0, 9); ax.set_aspect("equal")
for k, v in enumerate(levels):
    lab = f"{v:g}"
    ax.text(k + 0.5, -0.35, lab, ha="center", va="top", family=dcc.MONO, size=8, color=dcc.ASH)
    ax.text(-0.3, k + 0.5, lab, ha="right", va="center", family=dcc.MONO, size=8, color=dcc.ASH)
ax.text(4.5, -1.0, "TERRAIN: from a stroll to climbing gear", ha="center", va="top", family=dcc.MONO, size=7.5, color=dcc.INK)
ax.text(-1.15, 4.5, "DIFFICULTY: from obvious to a real puzzle", ha="center", va="center", rotation=90, family=dcc.MONO, size=7.5, color=dcc.INK)
# margin notes, FlowingData style
top = grid.stack().idxmax()
notes = [
    (0.72, 0.33, f"Most of my finds sit down here\nin the easy corner. The {grid.loc[top]:,}\nat {top[0]:g}/{top[1]:g} are the walk-up kind,\noften on a road trip."),
    (0.72, 0.52, f"I've filled {filled} of the 81 squares.\nGeocachers call filling all of them\n\"the Fizzy challenge.\""),
    (0.72, 0.68, "Up in the far corner: hard puzzles\nwith hard hikes. Mostly empty.\nSomeday."),
]
for x, y, t in notes:
    fig.text(x, y, t, family=dcc.TEXT, size=8.6, color=dcc.INK, va="top", linespacing=1.45)
fig.text(0.72, 0.725, "HOW TO READ IT", family=dcc.MONO, size=7.5, color=dcc.LAVA, va="top")
dcc.frame(fig, 12, subtitle=(f"Every cache comes with two ratings from 1 to 5: how hard it is to find and how hard it is to get to. "
                             f"Here are my {len(df):,} finds on that 9×9 grid. Bigger, redder squares mean more caches."),
          source="My geocaching.com finds",
          note="After Nathan Yau's FlowingData: show the data plainly, then talk the reader through it.")
dcc.save(fig, 12, alt=(f"A 9 by 9 grid of geocache difficulty and terrain ratings with counts; {filled} of 81 cells are filled, "
                       f"the most at difficulty {top[0]:g}, terrain {top[1]:g} with {grid.loc[top]} finds."))
print(grid)
