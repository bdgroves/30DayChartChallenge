"""Day 20 · Global Change: Arctic sea ice at its September low, every year since satellites began in 1979."""
import sys
from pathlib import Path
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "toolkit"))
import dcc
import numpy as np
import pandas as pd

d = pd.read_csv(HERE / "data" / "N_09_extent.csv", skipinitialspace=True)
d.columns = [c.strip() for c in d.columns]
d = d[d["extent"] > 0]
b, a = np.polyfit(d["year"], d["extent"], 1)
fig, ax = dcc.figure("wide", box=(0.08, 0.15, 0.86, 0.60))
dcc.style_axes(ax, grid="y")
cols = [dcc.SEQ_WATER(0.35 + 0.6 * (v - d["extent"].min()) / (d["extent"].max() - d["extent"].min())) for v in d["extent"]]
ax.bar(d["year"], d["extent"], color=cols, width=0.78, lw=0)
ax.plot(d["year"], a + b * d["year"], color=dcc.LAVA, lw=1.4, ls=(0, (4, 2)))
low = d.loc[d["extent"].idxmin()]
y0, y1 = d["year"].min(), d["year"].max()
ax.annotate(f"record low, {low['year']:.0f}\n{low['extent']:.2f} million km²", (low["year"], low["extent"]),
            xytext=(low["year"] - 8, 1.6), family=dcc.TEXT, size=8, color=dcc.INK, ha="center",
            arrowprops=dict(arrowstyle="-", color=dcc.STONE, lw=0.6))
ax.text(y0 + 1, a + b * (y0 + 1) + 0.45, f"trend: {-b * 10 * 1e6 / 1e3:,.0f} thousand km² lost per decade", family=dcc.TEXT,
        size=8.5, color=dcc.LAVA)
ax.set_ylim(0, 8.5); ax.set_xlim(y0 - 0.8, y1 + 0.8)
ax.set_ylabel("September average extent (million km²)", family=dcc.TEXT, size=9)
first = d[d["year"] <= 1988]["extent"].mean(); last = d[d["year"] >= y1 - 9]["extent"].mean()
dcc.frame(fig, 20, subtitle=(f"Arctic sea ice at the end of every summer, {y0}–{y1}. The last ten Septembers averaged {last:.1f} million km², "
                             f"against {first:.1f} million in the first ten: {1 - last / first:.0%} less, a loss of about "
                             f"{(first - last) / 1.72:.1f} Alaskas."),
          source=f"NSIDC Sea Ice Index, version 4 (September monthly extent, northern hemisphere)",
          note="Extent counts every ocean cell at least 15% covered by ice. Alaska is about 1.72 million km².")
dcc.save(fig, 20, alt=(f"Bars of September Arctic sea-ice extent from {y0} to {y1}, falling from about {d['extent'].iloc[0]:.1f} to "
                       f"{d['extent'].iloc[-1]:.1f} million km², with a record low of {low['extent']:.2f} in {low['year']:.0f}."))
print(round(first, 2), round(last, 2), d.tail(3).to_string())
