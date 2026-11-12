"""Day 3 · Mosaic: how the countries I've visited made their electricity in 2024.
Each column is a country, as wide as its total generation; each column is split by source."""
import sys
from pathlib import Path
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "toolkit"))
import dcc
import pandas as pd
from matplotlib.patches import Rectangle

df = pd.read_csv(HERE / "data" / "electricity.csv").set_index("country")
SOURCES = [  # label, columns, color
    ("Coal", ["coal_electricity"], "#3b3530"),
    ("Gas", ["gas_electricity"], dcc.STONE),
    ("Oil", ["oil_electricity"], "#a8988a"),
    ("Nuclear", ["nuclear_electricity"], "#8a5a83"),
    ("Hydro", ["hydro_electricity"], dcc.LAKE),
    ("Wind", ["wind_electricity"], "#7fa6b8"),
    ("Solar", ["solar_electricity"], dcc.GOLD),
    ("Bioenergy & geothermal", ["biofuel_electricity", "other_renewable_exc_biofuel_electricity"], dcc.SAGE),
]
parts = pd.DataFrame({lab: df[cols].fillna(0).sum(axis=1) for lab, cols, _ in SOURCES})
parts = parts[parts.sum(axis=1) > 0]
TOP = 12
order = parts.sum(axis=1).sort_values(ascending=False)
big = parts.loc[order.index[:TOP]]
small = parts.loc[order.index[TOP:]]
others = f"{len(small)} more"
big.loc[others] = small.sum()
total = big.sum(axis=1)
share = big.div(total, axis=0)
grand = total.sum()

fig, ax = dcc.figure("wide", axes=False, box=(0.06, 0.20, 0.72, 0.50))
x = 0.0
GAP = grand * 0.004
for country in big.index:
    w = total[country]
    y = 0.0
    for lab, _, color in SOURCES:
        h = share.loc[country, lab]
        if h > 0:
            ax.add_patch(Rectangle((x, y), w, h, color=color, lw=0))
        y += h
    name = {"United States": "United States", "United Kingdom": "UK"}.get(country, country)
    if w > grand * 0.035:
        ax.text(x + w / 2, -0.025, name, ha="center", va="top", family=dcc.TEXT, size=7.5 if w < grand * 0.06 else 9,
                color=dcc.INK, rotation=0 if w > grand * 0.06 else 90)
        ax.text(x + w / 2, 1.02, f"{w:,.0f}", ha="center", va="bottom", family=dcc.MONO, size=7, color=dcc.STONE)
    else:
        ax.text(x + w / 2, -0.025, name, ha="center", va="top", family=dcc.TEXT, size=7, color=dcc.INK, rotation=90)
    x += w + GAP
ax.text(0, 1.085, "TERAWATT-HOURS GENERATED IN 2024", family=dcc.MONO, size=7, color=dcc.STONE, va="bottom")
ax.set_xlim(0, x); ax.set_ylim(0, 1)

# legend, right side, top to bottom in stacking order (reversed so it reads like the columns)
for k, (lab, _, color) in enumerate(reversed(SOURCES)):
    yy = 0.74 - k * 0.062
    fig.patches.append(Rectangle((0.81, yy - 0.012), 0.016, 0.028, transform=fig.transFigure, color=color, lw=0))
    s = big[lab].sum() / grand
    fig.text(0.835, yy + 0.002, lab, family=dcc.TEXT, size=9.5, color=dcc.INK, va="center")
    fig.text(0.835, yy - 0.025, f"{s:.0%} of all of it", family=dcc.MONO, size=7, color=dcc.STONE, va="center")

us = share.loc["United States"]
dcc.frame(fig, 3, subtitle=("Electricity in the 32 countries I've visited, 2024. Each column is as wide as the country's generation; "
                            "the United States\nmakes more than all the others together. Canada runs on water, "
                            "France on nuclear, South Africa on coal."),
          source="Our World in Data, energy dataset (Ember, Energy Institute)",
          note=f"{others}: {', '.join(small.index)}. Vatican City has no grid of its own.")
dcc.save(fig, 3, alt=("A mosaic chart of electricity generation in 2024 for the countries Brooks has visited. "
                      f"The United States is the widest column at {total['United States']:,.0f} TWh, then "
                      + ", ".join(f"{c} {total[c]:,.0f}" for c in big.index[1:6])
                      + ". Each column is split into coal, gas, oil, nuclear, hydro, wind, solar and bioenergy."))
