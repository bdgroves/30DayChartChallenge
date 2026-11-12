"""Day 5 · Experimental: twenty-five years of geocaching as tree rings.
One ring per year from the first find in 2001 (the pith) outward; each find is a mark on its ring
at its day of the year, clockwise from January at the top. Big days stack outward within the ring."""
import sys
from pathlib import Path
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "toolkit"))
import dcc
import numpy as np
import pandas as pd

df = pd.read_csv(HERE.parent / "day-01-part-to-whole" / "data" / "finds.csv", parse_dates=["found"])
y0, y1 = df["found"].dt.year.min(), df["found"].dt.year.max()
years = np.arange(y0, y1 + 1)
rng = np.random.default_rng(7)

fig, ax = dcc.figure("square", axes=False, box=(0.15, 0.115, 0.70, 0.665))
ax.set_aspect("equal"); ax.axis("off")
R0, DR = 0.7, 1.0
wood = "#b89a72"
ax.add_patch(__import__("matplotlib.patches", fromlist=["Circle"]).Circle((0, 0), R0 + DR * len(years) + 0.35,
                                                                         color="#efe3cf", lw=0, zorder=0))
t = np.linspace(0, 2 * np.pi, 720)
for k, y in enumerate(years):
    r = R0 + DR * (k + 1)
    wob = 0.07 * np.sin(3 * t + k * 0.9) + 0.05 * np.sin(5 * t + k * 1.7)     # a ring is never quite round
    ax.plot((r + wob) * np.sin(t), (r + wob) * np.cos(t), color=wood, lw=0.9 if y % 5 else 1.6, zorder=1)
    if y % 5 == 0 or y in (y0, y1):
        ax.text(0.0, r - 0.5, str(y), ha="center", va="center", family=dcc.MONO, size=6.2, color=dcc.ASH, zorder=4)
bark = R0 + DR * len(years)
ax.plot((bark + 0.3) * np.sin(t), (bark + 0.3) * np.cos(t), color="#6e5640", lw=3.2, zorder=1)

for (day, n) in df.groupby(df["found"].dt.date).size().items():
    d = pd.Timestamp(day)
    k = d.year - y0
    a = 2 * np.pi * (d.dayofyear - 0.5) / (366 if d.is_leap_year else 365)
    for j in range(n):
        r = R0 + DR * k + 0.22 + min(j, 7) * 0.09 + rng.uniform(-0.03, 0.03)
        aa = a + (j // 8) * 0.012
        ax.scatter(r * np.sin(aa), r * np.cos(aa), s=3.2, color=dcc.LAVA if n >= 10 else dcc.INK,
                   lw=0, zorder=3, alpha=0.85)
for m, lab in enumerate(["JAN", "APR", "JUL", "OCT"]):
    a = m * np.pi / 2
    ax.text((bark + 0.8) * np.sin(a), (bark + 0.9) * np.cos(a), lab, ha={1: "left", 3: "right"}.get(m, "center"), va="center",
            family=dcc.MONO, size=7.5, color=dcc.STONE)
lim = bark + 1.6
ax.set_xlim(-lim, lim); ax.set_ylim(-lim, lim)

per = df.groupby(df["found"].dt.year).size()
big = df.groupby(df["found"].dt.date).size()
best = big.idxmax()
dcc.frame(fig, 5, subtitle=(f"Every geocache I've found, {y0}–{y1}: one ring a year from the middle out, one mark a find,\n"
                            f"January at the top and the year going clockwise. Red: days with ten finds or more."),
          source="My geocaching.com finds",
          note=f"Busiest year {per.idxmax()} ({per.max()} finds). Busiest day {pd.Timestamp(best):%B %-d, %Y} ({big.max()} finds).")
dcc.save(fig, 5, alt=(f"Tree rings, one per year from {y0} to {y1}, with {len(df):,} small marks for geocache finds placed by day of year. "
                      f"The busiest year was {per.idxmax()} with {per.max()} finds."))
