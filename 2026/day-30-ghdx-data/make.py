"""Day 30 · GHDx Data: life expectancy at birth from 1950 to 2100 for nine countries I've visited, from the UN's World
Population Prospects 2024 (found through IHME's GHDx catalog), with the UN's 80% and 95% projection intervals."""
import sys
from pathlib import Path
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "toolkit"))
import dcc
import pandas as pd
import matplotlib.pyplot as plt

est = pd.read_csv(HERE / "data" / "wpp2024_e0_estimates.csv")
proj = pd.read_csv(HERE / "data" / "wpp2024_e0_projections.csv")
PICK = ["USA", "ZAF", "NAM", "ETH", "ZWE", "MEX", "IDN", "ITA", "AUS"]
NAMES = {"USA": "United States", "ZAF": "South Africa", "NAM": "Namibia", "ETH": "Ethiopia", "ZWE": "Zimbabwe",
         "MEX": "Mexico", "IDN": "Indonesia", "ITA": "Italy", "AUS": "Australia"}
w_e = est[est["iso3"] == "WLD"]; w_p = proj[proj["iso3"] == "WLD"]
fig, _ = dcc.figure("square", axes=False); fig.axes[0].remove()
gs = fig.add_gridspec(3, 3, left=0.08, right=0.96, top=0.74, bottom=0.135, hspace=0.42, wspace=0.12)
for k, iso in enumerate(PICK):
    ax = fig.add_subplot(gs[k // 3, k % 3])
    dcc.style_axes(ax, grid="y")
    e, p = est[est["iso3"] == iso], proj[proj["iso3"] == iso]
    ax.plot(w_e["year"], w_e["e0B"], color=dcc.MIST, lw=1); ax.plot(w_p["year"], w_p["e0B"], color=dcc.MIST, lw=1, ls=(0, (2, 2)))
    ax.fill_between(p["year"], p["e0B_95l"], p["e0B_95u"], color=dcc.LAKE, alpha=0.18, lw=0)
    ax.fill_between(p["year"], p["e0B_80l"], p["e0B_80u"], color=dcc.LAKE, alpha=0.32, lw=0)
    ax.plot(p["year"], p["e0B"], color=dcc.LAKE, lw=1.2)
    ax.plot(e["year"], e["e0B"], color=dcc.INK, lw=1.4)
    ax.set_title(NAMES[iso], family=dcc.TEXT, size=9.5, color=dcc.INK, loc="left", pad=4)
    last_e = e[e["year"] == 2023]["e0B"].iloc[0]; e2100 = p[p["year"] == 2100].iloc[0]
    ax.text(2100, e2100["e0B_95l"] - 2.5, f"2100: {e2100['e0B']:.0f}\n({e2100['e0B_95l']:.0f}–{e2100['e0B_95u']:.0f})",
            ha="right", va="top", family=dcc.MONO, size=6.3, color=dcc.LAKE)
    ax.text(2023, last_e + 3, f"{last_e:.0f}", ha="center", family=dcc.MONO, size=6.3, color=dcc.INK)
    ax.set_xlim(1950, 2100); ax.set_ylim(20, 100)
    ax.set_xticks([1960, 2000, 2040, 2080])
    if k % 3: ax.set_yticklabels([])
    if k < 6: ax.set_xticklabels([])
from matplotlib.lines import Line2D
from matplotlib.patches import Patch
fig.legend(handles=[Line2D([], [], color=dcc.INK, lw=1.4, label="estimate, 1950–2023"),
                    Patch(color=dcc.LAKE, alpha=0.45, label="UN projection, 80% and 95% ranges"),
                    Line2D([], [], color=dcc.MIST, lw=1.2, label="world")],
           loc="upper left", bbox_to_anchor=(0.07, 0.80), ncol=3, frameon=False, prop={"family": dcc.TEXT, "size": 8})
zaf = est[(est["iso3"] == "ZAF")].set_index("year")["e0B"]
dcc.frame(fig, 30, subtitle=(f"Life expectancy since 1950 and the UN's projections to 2100, with their ranges. Southern Africa's "
                             f"AIDS years show plainly: South Africa fell from {zaf.loc[1990]:.0f} in 1990 to {zaf.loc[2000:2010].min():.0f} "
                             f"before climbing back."),
          source="UN World Population Prospects 2024, probabilistic projections (via GHDx)",
          note="IHME's own Global Burden of Disease files need a login, so this uses the UN series that GHDx catalogs.")
dcc.save(fig, 30, alt=("Nine small charts of life expectancy from 1950 to 2100 with projection bands for the United States, South Africa, Namibia, "
                       "Ethiopia, Zimbabwe, Mexico, Indonesia, Italy and Australia, against the world in grey."))
