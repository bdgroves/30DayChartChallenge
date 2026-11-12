"""Day 21 · Historical: Mono Lake's surface on October 1 of every year since 1850, through the Los Angeles diversions
(1941 on) and the 1994 State Water Board decision that set a target of 6,392 feet."""
import sys
from pathlib import Path
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "toolkit"))
import dcc
import pandas as pd

d = pd.read_csv(HERE / "data" / "mono_lake_oct1.csv")
fig, ax = dcc.figure("wide", box=(0.08, 0.15, 0.86, 0.60))
dcc.style_axes(ax, grid="y")
ax.fill_between(d["year"], 6360, d["level_ft"], color=dcc.LAKE, alpha=0.18, lw=0)
ax.plot(d["year"], d["level_ft"], color=dcc.LAKE, lw=1.8)
ax.axhline(6392, color=dcc.SAGE, lw=1.2, ls=(0, (4, 2)))
ax.text(1852, 6393.2, "6,392 ft: the level the 1994 decision ordered the lake back to", family=dcc.TEXT, size=8, color=dcc.SAGE)
ax.axvspan(1941, 1994, color=dcc.LAVA, alpha=0.06, lw=0)
low = d.loc[d["level_ft"].idxmin()]; high = d.loc[d["level_ft"].idxmax()]
for x, y, t, dx, dy in ((1941, d.set_index("year").loc[1941, "level_ft"], "1941 · Los Angeles starts\ndiverting Mono's streams", 12, 8),
                        (low["year"], low["level_ft"], f"{low['year']:.0f} · low point,\n{low['level_ft']:,.1f} ft", 2, -6),
                        (1994, d.set_index("year").loc[1994, "level_ft"], "1994 · State Water Board\nDecision 1631", 6, 10),
                        (1919, 6427, "July 1919 · highest\nrecorded, 6,428 ft", -16, 2)):
    ax.annotate(t, (x, y), xytext=(x + dx, y + dy), family=dcc.TEXT, size=8, color=dcc.INK, ha="center",
                arrowprops=dict(arrowstyle="-", color=dcc.STONE, lw=0.6))
ax.set_ylim(6360, 6435); ax.set_xlim(1850, d["year"].max() + 1)
ax.yaxis.set_major_formatter(__import__("matplotlib.ticker", fromlist=["FuncFormatter"]).FuncFormatter(lambda v, _: f"{v:,.0f} ft"))
last = d.iloc[-1]
dcc.frame(fig, 21, subtitle=(f"The surface of Mono Lake every October 1 since 1850. Forty years of diversions to Los Angeles dropped it about "
                             f"{d.set_index('year').loc[1941, 'level_ft'] - low['level_ft']:.0f} feet; the lake has climbed back about "
                             f"{last['level_ft'] - low['level_ft']:.0f} of them since, and in {last['year']:.0f} still sits "
                             f"{6392 - last['level_ft']:.0f} feet short of its target."),
          source="Mono Basin Clearinghouse / Mono Lake Committee (Stine 1850–1912; LADWP and USGS 1912–1979; LA Aqueduct reports since)",
          note="Before 1912 the levels are reconstructed from occasional observations; the shaded years are 1941–1994.")
dcc.save(fig, 21, alt=(f"A line of Mono Lake's October 1 level from 1850 to {last['year']:.0f}: about 6,417 ft in 1941, a low of "
                       f"{low['level_ft']:,.1f} ft in {low['year']:.0f}, and {last['level_ft']:,.1f} ft in {last['year']:.0f}, below the 6,392 ft target."))
