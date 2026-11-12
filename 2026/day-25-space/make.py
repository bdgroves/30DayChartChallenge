"""Day 25 · Space: exoplanets with a measured mass and radius, each with its error bars, on log scales,
with the solar system's planets for scale. Only true masses (not minimum masses, M sin i)."""
import sys
from pathlib import Path
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "toolkit"))
import dcc
import numpy as np
import pandas as pd
from matplotlib.ticker import FuncFormatter

p = pd.read_csv(HERE / "data" / "exoplanets.csv")
p = p[(p["pl_bmassprov"] == "Mass") & p["pl_bmasseerr1"].notna() & p["pl_radeerr1"].notna()].copy()
p["mrel"] = (p["pl_bmasseerr1"] + p["pl_bmasseerr2"].abs()) / 2 / p["pl_bmasse"]
p["rrel"] = (p["pl_radeerr1"] + p["pl_radeerr2"].abs()) / 2 / p["pl_rade"]
p = p[(p["pl_bmasse"] > 0) & (p["pl_rade"] > 0)]
sure = p["mrel"] <= 0.2
fig, ax = dcc.figure("wide", box=(0.08, 0.17, 0.64, 0.58))
dcc.style_axes(ax, grid="both")
ax.set_xscale("log"); ax.set_yscale("log")
for flag, c, a, z in ((False, dcc.MIST, 0.55, 1), (True, dcc.LAKE, 0.7, 2)):
    g = p[sure == flag]
    ax.errorbar(g["pl_bmasse"], g["pl_rade"], xerr=[g["pl_bmasseerr2"].abs().clip(upper=g["pl_bmasse"] * 0.999), g["pl_bmasseerr1"]],
                yerr=[g["pl_radeerr2"].abs().clip(upper=g["pl_rade"] * 0.999), g["pl_radeerr1"]],
                fmt="o", ms=2.2, color=c, ecolor=c, elinewidth=0.45, alpha=a, zorder=z, capsize=0)
SOL = {"Mercury": (0.055, 0.383), "Venus": (0.815, 0.949), "Earth": (1, 1), "Mars": (0.107, 0.532),
       "Jupiter": (317.8, 11.21), "Saturn": (95.2, 9.45), "Uranus": (14.5, 4.01), "Neptune": (17.1, 3.88)}
for n, (m, r) in SOL.items():
    ax.scatter(m, r, s=46, color=dcc.LAVA, zorder=4, lw=0.6, edgecolor=dcc.WHITE)
    off = {"Neptune": (6, -9), "Venus": (-6, 4), "Uranus": (-6, 6)}.get(n, (6, -2))
    ax.annotate(n, (m, r), xytext=off, textcoords="offset points", family=dcc.TEXT, ha="right" if off[0] < 0 else "left",
                size=8, color=dcc.LAVA, zorder=5, fontweight="bold",
                path_effects=[__import__("matplotlib.patheffects", fromlist=["withStroke"]).withStroke(linewidth=2.5, foreground=dcc.PARCHMENT)])
ax.set_xlim(0.03, 2e4); ax.set_ylim(0.25, 30)
ax.xaxis.set_major_formatter(FuncFormatter(lambda v, _: f"{v:g}"))
ax.yaxis.set_major_formatter(FuncFormatter(lambda v, _: f"{v:g}"))
ax.set_xlabel("Mass (Earths, log scale)", family=dcc.TEXT, size=9)
ax.set_ylabel("Radius (Earths, log scale)", family=dcc.TEXT, size=9)
fig.text(0.75, 0.70, "THE CROSSES ARE THE POINT", family=dcc.MONO, size=7.5, color=dcc.LAVA, va="top")
fig.text(0.75, 0.665, (f"Every planet here has its error bars\ndrawn: the range the measurement\nallows.\n\n"
                       f"{sure.sum():,} of the {len(p):,} have a mass known\nto within 20% (blue). The rest are\nwide grey crosses: "
                       "mostly small\nworlds whose weight is still uncertain.\n\nRadius comes from how much starlight\na planet blocks; "
                       "mass from how much\nit tugs its star. Small planets\nbarely tug at all."),
         family=dcc.TEXT, size=8.3, color=dcc.INK, va="top", linespacing=1.45)
dcc.frame(fig, 25, subtitle=(f"{len(p):,} exoplanets with both a measured mass and a measured radius, with their uncertainties, "
                             "and our own planets in red for scale. The giants are pinned down; the small, rocky ones much less so."),
          source="NASA Exoplanet Archive, Planetary Systems Composite Parameters (pscomppars)",
          note="True masses only: planets known just by their minimum mass (M sin i) are left out.")
dcc.save(fig, 25, alt=(f"A log-log scatter of {len(p):,} exoplanets' mass against radius, each with error bars; {sure.sum():,} have masses "
                       "known to within 20%. The solar system's planets are marked in red."))
print(len(p), sure.sum(), p["mrel"].median().round(2))
