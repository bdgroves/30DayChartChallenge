"""Day 9 · Wealth: typical home values in Washington's 39 counties, September 2016 and September 2026
(Zillow Home Value Index, mid-tier, all homes, not adjusted for inflation)."""
import sys
from pathlib import Path
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "toolkit"))
import dcc
import numpy as np
import pandas as pd
from matplotlib.ticker import FuncFormatter

A, B = "2016-09-30", "2026-09-30"
z = pd.read_csv(HERE / "data" / "zhvi_wa_counties.csv").copy()
z["name"] = z["RegionName"].str.replace(" County", "")
z = z.sort_values(B).reset_index(drop=True)
fig, ax = dcc.figure("portrait", box=(0.20, 0.08, 0.70, 0.67))
dcc.style_axes(ax, grid="x")
for i, r in z.iterrows():
    hl = r["name"] in ("Pierce", "King", "Garfield", "San Juan")
    ax.plot([r[A], r[B]], [i, i], color=dcc.LAVA if r["name"] == "Pierce" else dcc.MIST, lw=2.4 if hl else 1.8,
            solid_capstyle="round", zorder=1)
    ax.scatter(r[A], i, s=18, color=dcc.STONE, zorder=2, lw=0)
    ax.scatter(r[B], i, s=26, color=dcc.LAVA if r["name"] == "Pierce" else dcc.LAKE, zorder=3, lw=0)
    ax.text(r[A] - 12000, i, r["name"], ha="right", va="center", family=dcc.TEXT, size=7.6,
            color=dcc.INK, fontweight="bold" if r["name"] == "Pierce" else "normal")
    if hl:
        ax.text(r[B] + 14000, i, f"${r[B] / 1000:,.0f}k  (+{r[B] / r[A] - 1:.0%})", ha="left", va="center",
                family=dcc.MONO, size=7, color=dcc.INK)
ax.set_yticks([]); ax.spines["left"].set_visible(False)
ax.set_xlim(0, 1_000_000); ax.set_ylim(-1, len(z) + 0.5)
ax.xaxis.set_major_formatter(FuncFormatter(lambda v, _: f"${v / 1000:,.0f}k"))
ax.xaxis.tick_top()
ax.scatter([], [], s=18, color=dcc.STONE, label="September 2016"); ax.scatter([], [], s=26, color=dcc.LAKE, label="September 2026")
ax.legend(loc="lower right", prop={"family": dcc.TEXT, "size": 8})
gap16 = z[A].max() / z[A].min(); gap26 = z[B].max() / z[B].min()
med = z[B].median(); med16 = z[A].median()
dcc.frame(fig, 9, subtitle=(f"The typical home in each Washington county, 2016 and 2026. Values about doubled almost everywhere: "
                            f"the median county went from \\${med16 / 1000:,.0f}k to \\${med / 1000:,.0f}k, and the gap from cheapest "
                            f"to dearest narrowed from {gap16:.1f}:1 to {gap26:.1f}:1."),
          source="Zillow Home Value Index (ZHVI), all homes, mid-tier, seasonally adjusted",
          note="Dollars of the day, not adjusted for inflation. I work at Zillow; this is the public research data anyone can download.")
dcc.save(fig, 9, alt=(f"A dumbbell chart of 39 Washington counties' typical home values in 2016 and 2026. San Juan is highest at "
                      f"${z[B].max():,.0f}, Garfield lowest at ${z[B].min():,.0f}; Pierce County is ${z.set_index('name').loc['Pierce', B]:,.0f}."))
print(round(gap16, 2), round(gap26, 2), (z[B] / z[A]).describe().round(2).to_dict())
