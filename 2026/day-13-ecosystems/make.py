"""Day 13 · Ecosystems: Lake Tahoe's food web, simplified: who eats whom, by feeding level,
with the species people brought in and the ones that are gone. Compiled by hand from UC Davis TERC's
State of the Lake reports and the Tahoe fisheries literature."""
import sys
from pathlib import Path
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "toolkit"))
import dcc
import pandas as pd
from matplotlib.patches import FancyArrowPatch

sp = pd.read_csv(HERE / "data" / "species.csv").set_index("name")
web = pd.read_csv(HERE / "data" / "foodweb.csv")
COL = {"native": dcc.LAKE, "introduced": dcc.LAVA, "gone": dcc.STONE}
fig, ax = dcc.figure("wide", axes=False, box=(0.10, 0.12, 0.66, 0.64))
for _, e in web.iterrows():
    a, b = sp.loc[e["prey"]], sp.loc[e["predator"]]
    gone = sp.loc[e["predator"], "status"] == "gone"
    ax.add_patch(FancyArrowPatch((a["x"], a["level"]), (b["x"], b["level"]), arrowstyle="-|>", mutation_scale=7,
                                 color=dcc.STONE if not gone else dcc.MIST, lw=0.7, alpha=0.8, shrinkA=9, shrinkB=11,
                                 connectionstyle="arc3,rad=0.08", ls="-" if not gone else (0, (2, 2))))
for name, s in sp.iterrows():
    c = COL[s["status"]]
    ax.scatter(s["x"], s["level"], s=150, color=c if s["status"] != "gone" else dcc.PARCHMENT, edgecolor=c,
               lw=1.4, zorder=3, linestyle="--" if s["status"] == "gone" else "-")
    ax.text(s["x"], s["level"] + 0.13, name, ha="center", va="bottom", family=dcc.TEXT, size=7.6, color=dcc.INK, zorder=4,
            bbox=dict(boxstyle="round,pad=0.1", fc=dcc.PARCHMENT, ec="none", alpha=0.85))
    if isinstance(s["note"], str):
        ax.text(s["x"], s["level"] - 0.12, s["note"], ha="center", va="top", family=dcc.TEXT, size=5.9,
                style="italic", color=dcc.ASH, zorder=4,
                bbox=dict(boxstyle="round,pad=0.15", fc=dcc.PARCHMENT, ec="none", alpha=0.85))
for lv, lab in ((1, "PRODUCERS"), (2, "GRAZERS"), (3, "SMALL FISH"), (4, "TOP PREDATORS")):
    ax.text(-5.9, lv, lab, ha="right", va="center", family=dcc.MONO, size=6.8, color=dcc.STONE)
ax.set_xlim(-7.2, 4.9); ax.set_ylim(0.6, 4.55)
for k, (st, lab) in enumerate((("native", "Native"), ("introduced", "Brought in by people"), ("gone", "Gone from the lake"))):
    y = 0.62 - k * 0.06
    fig.add_artist(__import__("matplotlib.lines", fromlist=["Line2D"]).Line2D([0.807], [y], marker="o", ms=8, ls="",
                   color=COL[st] if st != "gone" else dcc.PARCHMENT, mec=COL[st], mew=1.4, transform=fig.transFigure))
    fig.text(0.825, y, lab, family=dcc.TEXT, size=9, color=dcc.INK, va="center")
fig.text(0.80, 0.42, "Arrows point from the eaten\nto the eater.\n\nWhen mysis shrimp arrived,\nthey ate the lake's Daphnia\nand Bosmina out of open\nwater, and the kokanee that\nfed on them lost out.",
         family=dcc.TEXT, size=8.4, color=dcc.INK, va="top", linespacing=1.45)
dcc.frame(fig, 13, subtitle=("Lake Tahoe's food web, simplified, from the algae up. Six of these sixteen arrived with people: lake trout in the 1880s, "
                             "crayfish around 1900, kokanee in the 1940s, mysis shrimp in the 1960s, Asian clams in 2002."),
          source="Compiled from UC Davis Tahoe Environmental Research Center, State of the Lake reports, and Tahoe fisheries studies",
          note="Simplified: real diets overlap more, and the warm-water fish near shore (bass, bluegill) are left out.")
dcc.save(fig, 13, alt=("A food-web diagram of Lake Tahoe in four feeding levels, with native species in blue, introduced species in red "
                       "and the extinct Lahontan cutthroat trout dashed. Lake trout sit at the top; mysis shrimp eat Daphnia and Bosmina."))
