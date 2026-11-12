"""Day 19 · Evolution: how the United States made its electricity, 1985-2025, as shares of the total."""
import sys
from pathlib import Path
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "toolkit"))
import dcc
import numpy as np
import pandas as pd

u = pd.read_csv(HERE / "data" / "us_electricity.csv").set_index("year")
SRC = [("Coal", ["coal_electricity"], "#3b3530"), ("Oil", ["oil_electricity"], "#a8988a"), ("Gas", ["gas_electricity"], dcc.STONE),
       ("Nuclear", ["nuclear_electricity"], "#8a5a83"), ("Hydro", ["hydro_electricity"], dcc.LAKE),
       ("Bioenergy & geothermal", ["biofuel_electricity", "other_renewable_exc_biofuel_electricity"], dcc.SAGE),
       ("Wind", ["wind_electricity"], "#7fa6b8"), ("Solar", ["solar_electricity"], dcc.GOLD)]
parts = pd.DataFrame({lab: u[c].fillna(0).sum(axis=1) for lab, c, _ in SRC})
share = parts.div(parts.sum(axis=1), axis=0) * 100
fig, ax = dcc.figure("wide", box=(0.07, 0.15, 0.72, 0.60))
dcc.style_axes(ax, grid="")
ax.stackplot(share.index, share.T.values, colors=[c for *_, c in SRC], lw=0)
ax.set_xlim(share.index.min(), share.index.max()); ax.set_ylim(0, 100)
ax.yaxis.set_major_formatter(__import__("matplotlib.ticker", fromlist=["FuncFormatter"]).FuncFormatter(lambda v, _: f"{v:.0f}%"))
y0, y1 = share.index.min(), share.index.max()
cum = share.cumsum(axis=1)
from repel import spread
mids = [cum[lab].iloc[-1] - share[lab].iloc[-1] / 2 for lab, _, _ in SRC]
for (lab, _, c), m, y in zip(SRC, mids, spread(mids, 4.2)):
    ax.annotate(f"{lab}  {share[lab].iloc[-1]:.0f}%", (y1, m), xytext=(y1 + 0.9, y), va="center", family=dcc.TEXT, size=8.4,
                color=dcc.INK, annotation_clip=False, arrowprops=dict(arrowstyle="-", color=dcc.MIST, lw=0.5))
for yr, txt, yy in ((2007, "coal generation peaks", 30), (2016, "gas passes coal", 55)):
    ax.axvline(yr, color=dcc.WHITE, lw=0.8, ls=":")
    ax.text(yr - 0.3, yy, txt, rotation=90, va="center", ha="right", family=dcc.TEXT, size=7.6, color=dcc.WHITE)
dcc.frame(fig, 19, subtitle=(f"Where America's electricity came from, {y0}–{y1}. Coal went from {share.loc[y0, 'Coal']:.0f}% to "
                             f"{share.loc[y1, 'Coal']:.0f}%; gas from {share.loc[y0, 'Gas']:.0f}% to {share.loc[y1, 'Gas']:.0f}%; "
                             f"wind and solar from almost nothing to {share.loc[y1, 'Wind'] + share.loc[y1, 'Solar']:.0f}%. "
                             f"Total generation grew {u.loc[y1, 'electricity_generation'] / u.loc[y0, 'electricity_generation'] - 1:.0%}."),
          source="Our World in Data, energy dataset (Ember; Energy Institute Statistical Review)")
dcc.save(fig, 19, alt=(f"A stacked area chart of US electricity shares from {y0} to {y1}: coal {share.loc[y0, 'Coal']:.0f}% to "
                       f"{share.loc[y1, 'Coal']:.0f}%, gas {share.loc[y0, 'Gas']:.0f}% to {share.loc[y1, 'Gas']:.0f}%, nuclear about "
                       f"{share.loc[y1, 'Nuclear']:.0f}%, wind {share.loc[y1, 'Wind']:.0f}% and solar {share.loc[y1, 'Solar']:.0f}% in {y1}."))
print(share.loc[[1985, 2007, 2015, 2016, 2025], ["Coal", "Gas"]].round(1))
