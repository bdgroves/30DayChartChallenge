"""Day 17 · Remake: the chart I practised on in 2024 (2024/experiments, a ggplot2 rebuild of The Economist's
"Escape artists" bar chart, after the R Graph Gallery tutorial), redone in this year's style with the same numbers."""
import sys
from pathlib import Path
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "toolkit"))
import dcc
import pandas as pd

d = pd.read_csv(HERE / "data" / "lab_infections.csv").sort_values("infections")
fig, ax = dcc.figure("wide", box=(0.20, 0.17, 0.50, 0.58))
dcc.style_axes(ax, grid="x")
ys = range(len(d))
for y, (_, r) in zip(ys, d.iterrows()):
    c = dcc.LAVA if r["pathogen"] == "Brucella" else dcc.LAKE
    ax.plot([0, r["infections"]], [y, y], color=c, lw=3, solid_capstyle="butt", alpha=0.35)
    ax.scatter(r["infections"], y, s=90, color=c, zorder=3, lw=0)
    ax.text(r["infections"] + 1.4, y, str(r["infections"]), va="center", family=dcc.MONO, size=9, color=dcc.INK)
    ax.text(-1.2, y, r["pathogen"], ha="right", va="center", family=dcc.TEXT, size=10, color=dcc.INK,
            style="italic" if r["pathogen"] in ("Brucella", "Salmonella", "E. coli") else "normal")
ax.set_yticks([]); ax.spines["left"].set_visible(False)
ax.set_xlim(0, 60); ax.set_ylim(-0.6, len(d) - 0.4)
ax.set_xlabel("Laboratory-acquired infections, 1970–2021", family=dcc.TEXT, size=9)
total = d["infections"].sum()
fig.text(0.74, 0.70, "WHAT CHANGED", family=dcc.MONO, size=7.5, color=dcc.LAVA, va="top")
fig.text(0.74, 0.665, ("In 2024 I rebuilt The Economist's\nbar chart line by line in ggplot2.\nSame nine numbers here.\n\n"
                       "Dots instead of heavy bars, labels\nwhere the eye starts, and one color\nfor the point of the chart:\n"
                       f"Brucella alone is {d['infections'].max() / total:.0%} of these {total}.\n\n"
                       "It's a livestock bacterium that\nspreads easily through the air in a\nlab, which is why it tops lists\nlike this one."),
         family=dcc.TEXT, size=8.4, color=dcc.INK, va="top", linespacing=1.45)
dcc.frame(fig, 17, subtitle=("Escape artists, again: infections caught in laboratories, by pathogen, as The Economist charted them. "
                             "My first chart-challenge attempt, two years on."),
          source="Laboratory-Acquired Infection Database; American Biological Safety Association (as charted by The Economist)",
          note="The 2024 original is the R script in 2024/experiments in this repo.")
dcc.save(fig, 17, alt=("A dot chart of laboratory-acquired infections from 1970 to 2021 by pathogen: "
                       + ", ".join(f"{p} {n}" for p, n in zip(d["pathogen"][::-1], d["infections"][::-1])) + "."))
