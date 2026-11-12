"""Day 4 · Slope: CO2 per person in 1994 and 2024 for the countries I've visited (log scale)."""
import sys
from pathlib import Path
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "toolkit"))
import dcc
import numpy as np
import pandas as pd
from repel import spread

A, B = 1994, 2024
df = pd.read_csv(HERE / "data" / "co2_per_capita.csv")
w = df[df["year"].isin([A, B])].pivot(index="country", columns="year", values="co2_per_capita").dropna()
w = w.rename(index={"Saint Kitts and Nevis": "St Kitts & Nevis", "Antigua and Barbuda": "Antigua & Barbuda",
                    "Dominican Republic": "Dominican Rep.", "United States": "United States"})
world = w.loc["World"]; w = w.drop("World")
fig, ax = dcc.figure("portrait", box=(0.31, 0.06, 0.38, 0.70))
ax.set_yscale("log")
ax.set_xlim(0, 1); ax.set_ylim(0.03, 40)
ax.axis("off")
up, down = dcc.LAVA, dcc.LAKE
for c, r in w.iterrows():
    col = up if r[B] > r[A] else down
    lw = 2.2 if c in ("United States", "South Africa") else 1.1
    ax.plot([0, 1], [r[A], r[B]], color=col, lw=lw, alpha=0.9, solid_capstyle="round")
    ax.scatter([0, 1], [r[A], r[B]], s=10 if lw < 2 else 24, color=col, zorder=3, lw=0)
ax.plot([0, 1], [world[A], world[B]], color=dcc.INK, lw=1.6, ls=(0, (3, 2)))

def labels(side, x, ha):
    vals = w[side].tolist() + [world[side]]
    names = w.index.tolist() + ["World"]
    pos = spread([np.log10(v) for v in vals], 0.062)
    for v, p, nm in zip(vals, pos, names):
        bold = nm in ("United States", "South Africa", "World")
        col = dcc.INK if bold else dcc.ASH
        f = f"{v:.2f}" if v < 0.2 else f"{v:.1f}"
        txt = f"{nm}  {f}" if ha == "left" else f"{f}  {nm}"
        ax.annotate(txt, (x, v), xytext=(x + (0.06 if ha == "left" else -0.06), 10 ** p), ha=ha, va="center",
                    family=dcc.TEXT, size=7.2, color=col, fontweight="bold" if bold else "normal",
                    arrowprops=dict(arrowstyle="-", color=dcc.MIST, lw=0.5, shrinkA=0, shrinkB=2))
labels(A, 0, "right"); labels(B, 1, "left")
ax.text(0, 46, str(A), ha="center", family=dcc.MONO, size=11, color=dcc.INK)
ax.text(1, 46, str(B), ha="center", family=dcc.MONO, size=11, color=dcc.INK)
fell = (w[B] < w[A]).sum()
dcc.frame(fig, 4, subtitle=(f"Tonnes of CO₂ per person in {A}, the year I finished at UNR, and {B}. It fell in {fell} of the\n"
                            f"{len(w)} countries I've visited (blue) and rose in the rest (red). Dashed: the world. Log scale."),
          source="Our World in Data, CO2 dataset (Global Carbon Project)")
dcc.save(fig, 4, alt=(f"A slope chart of CO₂ per person in {A} and {B} for {len(w)} countries. "
                      f"The United States fell from {w.loc['United States', A]:.1f} to {w.loc['United States', B]:.1f} tonnes; "
                      f"Luxembourg from {w.loc['Luxembourg', A]:.1f} to {w.loc['Luxembourg', B]:.1f}; "
                      f"Indonesia rose from {w.loc['Indonesia', A]:.1f} to {w.loc['Indonesia', B]:.1f}. "
                      f"The world went from {world[A]:.1f} to {world[B]:.1f}."))
