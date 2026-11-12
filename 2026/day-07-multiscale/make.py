"""Day 7 · Multiscale: Washington's earthquakes since 1970 by magnitude, on a log scale.
Counts fall about tenfold with each step of magnitude (Gutenberg–Richter); the energy rises about 32-fold."""
import sys
from pathlib import Path
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "toolkit"))
import dcc
import json
import numpy as np
import pandas as pd
from shapely import contains_xy
from shapely.geometry import shape

q = pd.read_csv(HERE / "data" / "quakes.csv.gz", parse_dates=["time"])
wa = shape(json.load(open(HERE / "data" / "wa.geojson"))["features"][0]["geometry"])
q = q[contains_xy(wa, q["longitude"].values, q["latitude"].values) & (q["mag"] >= -1)]
y0, y1 = q["time"].dt.year.min(), q["time"].dt.year.max()

edges = np.arange(-1.0, 7.01, 0.2)
counts, _ = np.histogram(q["mag"], bins=edges)
mid = edges[:-1] + 0.1
fig, ax = dcc.figure("wide", box=(0.08, 0.17, 0.86, 0.53))
ax.bar(mid, np.where(counts > 0, counts, np.nan), width=0.17, color=dcc.LAKE, lw=0)
ax.set_yscale("log"); ax.set_ylim(0.7, 1.2e5); ax.set_xlim(-1.1, 7.1)
dcc.style_axes(ax, grid="y")
ax.set_xlabel("Magnitude", family=dcc.TEXT, size=9); ax.set_ylabel("Earthquakes since 1970 (log scale)", family=dcc.TEXT, size=9)
ax.yaxis.set_major_formatter(__import__("matplotlib.ticker", fromlist=["FuncFormatter"]).FuncFormatter(lambda v, _: f"{v:,.0f}"))

# Gutenberg–Richter line through the complete part of the catalog (M2 and up)
m = (mid >= 2.0) & (counts > 0)
b, a = np.polyfit(mid[m], np.log10(counts[m]), 1)
xx = np.linspace(1.4, 7, 50)
ax.plot(xx, 10 ** (a + b * xx), color=dcc.LAVA, lw=1.4, ls=(0, (4, 2)))
ax.text(4.1, 10 ** (a + b * 4.1) * 2.2, f"about {10 ** -b:.0f}× fewer\nfor each step up", family=dcc.TEXT, size=9,
        color=dcc.LAVA, ha="left")

for t, label, dy in [("2001-02-28", "Nisqually, 2001\nM6.8", 8), ("1980-05-18", "Mount St. Helens\nerupts, 1980 · M5.7", 14),
                     ("1999-07-03", "Satsop, 1999\nM5.8", 40)]:
    e = q[q["time"].dt.strftime("%Y-%m-%d") == t].nlargest(1, "mag").iloc[0]
    c = np.floor((e["mag"] + 1) / 0.2 + 1e-6) * 0.2 - 1 + 0.1          # the bar it sits in
    ax.annotate(label, (c, 1.05), xytext=(e["mag"] - (0.05 if "Satsop" not in label else 0.6), dy),
                ha="center", family=dcc.TEXT, size=7.5, color=dcc.INK,
                arrowprops=dict(arrowstyle="-", color=dcc.STONE, lw=0.6))
ax.axvspan(-1.1, 1.4, color=dcc.MIST, alpha=0.35, lw=0)
ax.text(-1.0, 8e4, "Too small to catch every one:\nthe network misses some of these", family=dcc.TEXT, size=7.5,
        color=dcc.ASH, va="top")
ax.axvline(2.5, color=dcc.STONE, lw=0.6, ls=":")
ax.text(2.55, 8e4, "roughly where people\nstart to feel them", family=dcc.TEXT, size=7.5, color=dcc.ASH, va="top")
# energy along the top: each whole magnitude is about 31.6x the energy
top = ax.secondary_xaxis("top", functions=(lambda x: x, lambda x: x))
top.set_xticks(range(0, 8))
top.set_xticklabels(["1×", "32×", "1,000×", "32,000×", "1 million×", "32 million×", "1 billion×", "32 billion×"],
                    family=dcc.MONO, size=7, color=dcc.STONE)
top.tick_params(length=0); top.spines["top"].set_visible(False)
ax.text(-1.05, 2.6e5, "ENERGY, COMPARED WITH A MAGNITUDE 0", family=dcc.MONO, size=6.5, color=dcc.STONE)
dcc.frame(fig, 7, subtitle=(f"All {len(q):,} earthquakes recorded inside Washington, {y0}–{y1}, by magnitude. Each step up the scale is "
                            f"about {10 ** -b:.0f} times rarer\nand about 32 times stronger, from tremors no one feels to the Nisqually quake that cracked the Capitol dome."),
          source="USGS ComCat (Pacific Northwest Seismic Network)")
dcc.save(fig, 7, alt=(f"A histogram of {len(q):,} Washington earthquakes since {y0} on a log scale. Counts peak near magnitude 1 and "
                      f"fall about {10 ** -b:.0f}-fold per magnitude step; the largest is the 2001 Nisqually M6.8."))
print("b-value", round(-b, 2))
