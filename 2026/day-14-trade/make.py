"""Day 14 · Trade: US goods trade in 2024 with the countries I've visited. What America sells to each one
against what it buys, on log scales; above the diagonal, the US buys more than it sells."""
import sys
from pathlib import Path
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "toolkit"))
import dcc
import json
import numpy as np
import pandas as pd
from places import VISITED
from matplotlib.ticker import FuncFormatter

pm = {r["id"]: r for r in json.load(open(HERE / "data" / "partners.json"))["results"]}
def load(flow):
    d = pd.DataFrame(json.load(open(HERE / "data" / f"us_2024_{flow}.json"))["data"])
    d = d.drop_duplicates(["partnerCode"])
    d["iso"] = d["partnerCode"].map(lambda c: pm.get(c, {}).get("PartnerCodeIsoAlpha3"))
    return d
X, M = load("X"), load("M")
world_x = X.loc[X["partnerCode"] == 0, "primaryValue"].iloc[0]; world_m = M.loc[M["partnerCode"] == 0, "primaryValue"].iloc[0]
t = pd.DataFrame({"x": X.set_index("iso")["primaryValue"], "m": M.set_index("iso")["primaryValue"]})
t = t[t.index.isin(VISITED) & ~t.index.isin(["USA", "VAT"])].dropna() / 1e9
t["name"] = t.index.map(VISITED).str.replace("Saint Kitts and Nevis", "St Kitts & Nevis").str.replace("Antigua and Barbuda", "Antigua & Barbuda").str.replace("Dominican Republic", "Dominican Rep.").str.replace("United Kingdom", "UK")
share = (t["x"].sum() + t["m"].sum()) / ((world_x + world_m) / 1e9)

fig, ax = dcc.figure("square", box=(0.13, 0.13, 0.79, 0.62))
dcc.style_axes(ax, grid="both")
ax.set_xscale("log"); ax.set_yscale("log")
lim = (0.001, 1000)
ax.plot(lim, lim, color=dcc.INK, lw=0.8, ls=(0, (3, 2)))
ax.fill_between(lim, lim, [lim[1]] * 2, color=dcc.LAVA, alpha=0.05, lw=0)
size = 18 + 900 * np.sqrt((t["x"] + t["m"]) / (t["x"] + t["m"]).max())
ax.scatter(t["x"], t["m"], s=size, color=np.where(t["m"] > t["x"], dcc.LAVA, dcc.LAKE), alpha=0.75, lw=0.6, edgecolor=dcc.WHITE)
OFF = {  # label offsets in points, for the crowded ones (dx, dy, ha)
    "MEX": (-14, 10, "right"), "CAN": (12, -12, "left"), "DEU": (14, 0, "left"), "IRL": (-30, 16, "right"),
    "ITA": (-16, -2, "right"), "FRA": (6, -16, "left"), "GBR": (14, 6, "left"), "NLD": (16, 0, "left"),
    "BEL": (12, -6, "left"), "AUS": (12, -4, "left"), "HRV": (-8, -10, "right"), "LUX": (10, -4, "left"),
    "ISL": (-8, 4, "right"), "IDN": (10, 0, "left"), "KNA": (8, -8, "left")}
for iso, r in t.iterrows():
    dx, dy, ha = OFF.get(iso, (8, 0, "left"))
    lead = abs(dx) > 14 or abs(dy) > 12
    ax.annotate(r["name"], (r["x"], r["m"]), xytext=(dx, dy), textcoords="offset points", ha=ha, va="center",
                family=dcc.TEXT, size=7.2 if r["x"] + r["m"] < 50 else 8.4, color=dcc.INK,
                arrowprops=dict(arrowstyle="-", color=dcc.STONE, lw=0.5) if lead else None)
ax.set_xlim(*lim); ax.set_ylim(*lim)
fmt = FuncFormatter(lambda v, _: f"${v:,.0f}B" if v >= 1 else f"${v * 1000:,.0f}M")
ax.xaxis.set_major_formatter(fmt); ax.yaxis.set_major_formatter(fmt)
ax.set_xlabel("What the US sold them (exports)", family=dcc.TEXT, size=9)
ax.set_ylabel("What the US bought from them (imports)", family=dcc.TEXT, size=9)
ax.text(0.0016, 300, "The US buys more\nthan it sells", family=dcc.TEXT, size=8.5, color=dcc.LAVA, va="top")
ax.text(300, 0.002, "The US sells more\nthan it buys", family=dcc.TEXT, size=8.5, color=dcc.LAKE, ha="right")
fmt_dollars = lambda v: f"\\${v:,.0f} billion"
dcc.frame(fig, 14, subtitle=(f"America's goods trade in 2024 with the {len(t)} other countries I've visited, which make up {share:.0%} of all of it. "
                             "Canada and Mexico dwarf the rest. Germany, Ireland and Italy sell the US far more than they buy."),
          source="UN Comtrade, United States reported trade, 2024 (log scales)",
          note="Dot size is total trade. Goods only; services aren't in these numbers. Vatican City trade is too small to show.")
dcc.save(fig, 14, alt=(f"A log-log scatter of US exports versus imports in 2024 for {len(t)} countries. Canada: exports "
                       f"${t.loc['CAN', 'x']:.0f}B, imports ${t.loc['CAN', 'm']:.0f}B; Mexico ${t.loc['MEX', 'x']:.0f}B and ${t.loc['MEX', 'm']:.0f}B."))
print(round(share, 3)); print(t.sort_values("m").round(2).to_string())
