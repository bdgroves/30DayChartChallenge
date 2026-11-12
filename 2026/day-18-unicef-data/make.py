"""Day 18 · UNICEF Data: under-five mortality against income for every country, 1990 to the latest year.
Each country is an arrow from 1990 to now; the countries I've visited are labeled."""
import sys
from pathlib import Path
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "toolkit"))
import dcc
import numpy as np
import pandas as pd
from places import VISITED
from matplotlib.ticker import FuncFormatter

u = pd.read_csv(HERE / "data" / "unicef_u5mr.csv", low_memory=False)
u = u[(u["WEALTH_QUINTILE"] == "_T") & (u["DATA_SOURCE"] == "UN_IGME") & (u["REF_AREA"].str.len() == 3)].copy()
u["year"] = u["TIME_PERIOD"].astype(str).str[:4].astype(int)
g = pd.read_csv(HERE / "data" / "wb_gdppc_ppp.csv")
# wb_countries.csv has unquoted commas in some region names, so read it line by line
lines = (HERE / "data" / "wb_countries.csv").read_text(encoding="utf-8").splitlines()[1:]
real = {l.split(",")[0] for l in lines if "Aggregates" not in l}
A = 1990
B = int(min(u["year"].max(), g["year"].max()))
m = lambda df, y, col: df[df["year"] == y].set_index("REF_AREA" if "REF_AREA" in df else "iso3")[col]
d = pd.DataFrame({"u0": m(u, A, "OBS_VALUE"), "u1": m(u, B, "OBS_VALUE"), "g0": m(g, A, "gdppc"), "g1": m(g, B, "gdppc")}).dropna()
d = d[d.index.isin(real)]

fig, ax = dcc.figure("wide", box=(0.08, 0.17, 0.64, 0.58))
dcc.style_axes(ax, grid="both")
ax.set_xscale("log"); ax.set_yscale("log")
for iso, r in d.iterrows():
    v = iso in VISITED
    ax.annotate("", (r["g1"], r["u1"]), (r["g0"], r["u0"]),
                arrowprops=dict(arrowstyle="-|>", color=dcc.LAVA if v else dcc.MIST, lw=1.0 if v else 0.6,
                                mutation_scale=6 if v else 4, shrinkA=0, shrinkB=0, alpha=1 if v else 0.9))
LAB = {"USA": (6, 4), "ZAF": (6, 2), "NAM": (6, -6), "ETH": (6, 0), "TZA": (-4, -10), "IDN": (6, -2), "MEX": (6, 0),
       "AGO": (6, 0), "ZWE": (-6, 6), "BWA": (6, 0), "ITA": (-6, -8), "ISL": (6, -4), "GBR": (-6, 7), "NZL": (6, 0)}
for iso, (dx, dy) in LAB.items():
    if iso in d.index:
        r = d.loc[iso]
        ax.annotate(VISITED[iso].replace("United Kingdom", "UK"), (r["g1"], r["u1"]), xytext=(dx, dy), textcoords="offset points",
                    family=dcc.TEXT, size=7.4, color=dcc.INK, ha="left" if dx > 0 else "right", va="center")
ax.set_xlim(d[["g0", "g1"]].min().min() * 0.8, d[["g0", "g1"]].max().max() * 1.25)
ax.set_ylim(d[["u0", "u1"]].min().min() * 0.8, d[["u0", "u1"]].max().max() * 1.25)
ax.xaxis.set_major_formatter(FuncFormatter(lambda v, _: f"\\${v:,.0f}"))
ax.yaxis.set_major_formatter(FuncFormatter(lambda v, _: f"{v:g}"))
ax.set_xlabel("GDP per person, 2021 international dollars (log scale)", family=dcc.TEXT, size=9)
ax.set_ylabel("Children who die before age 5, per 1,000 births (log)", family=dcc.TEXT, size=9)
fell = (d["u1"] < d["u0"]).mean()
w0 = u[(u["REF_AREA"] == "_T") | (u["REF_AREA"] == "WLD")]
fig.text(0.75, 0.70, "READING THE ARROWS", family=dcc.MONO, size=7.5, color=dcc.LAVA, va="top")
fig.text(0.75, 0.665, (f"Each arrow runs from {A} to {B}.\nRed ones are countries I've visited.\n\n"
                       f"{fell:.0%} of the arrows point down:\nchild deaths fell almost everywhere,\nincluding in places that "
                       "barely\ngrew richer. Vaccines, clean water\nand oral rehydration don't wait\nfor GDP.\n\n"
                       f"Ethiopia went from {d.loc['ETH', 'u0']:.0f} to {d.loc['ETH', 'u1']:.0f}.\n"
                       f"The US from {d.loc['USA', 'u0']:.1f} to {d.loc['USA', 'u1']:.1f}."),
         family=dcc.TEXT, size=8.4, color=dcc.INK, va="top", linespacing=1.45)
dcc.frame(fig, 18, subtitle=(f"Under-five mortality against income for {len(d)} countries, {A} to {B}. Richer countries lose fewer children, "
                             "but the bigger story is that nearly every arrow points down."),
          source="UNICEF Data Warehouse (UN IGME under-five mortality); World Bank (GDP per capita, PPP)")
dcc.save(fig, 18, alt=(f"Arrows for {len(d)} countries from {A} to {B} on log axes of GDP per person and under-five mortality; "
                       f"{fell:.0%} point down. Ethiopia fell from {d.loc['ETH', 'u0']:.0f} to {d.loc['ETH', 'u1']:.0f} deaths per 1,000 births."))
print(A, B, len(d), round(fell, 3))
