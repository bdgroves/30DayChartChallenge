"""Day 15 · Correlation: Washington SNOTEL stations, elevation against median April 1 snow water (1991-2020).
Higher stations hold more snow, but which side of the Cascades you're on matters as much."""
import sys
from pathlib import Path
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "toolkit"))
import dcc
import numpy as np
import pandas as pd

s = pd.read_csv(HERE / "data" / "snotel_wa_apr1.csv", comment="#")
s.columns = ["date", "id", "name", "elev", "lat", "lon", "swe", "med"]
n_all = len(s)
s = s.dropna(subset=["med"])
s["west"] = s["lon"] < -121.2           # roughly the Cascade crest (Olympics included)
fig, ax = dcc.figure("wide", box=(0.08, 0.17, 0.62, 0.58))
dcc.style_axes(ax, grid="both")
r_all = np.corrcoef(s["elev"], s["med"])[0, 1]
out = []
for west, color, lab in ((True, dcc.LAKE, "West of the crest"), (False, dcc.GOLD, "East of the crest")):
    g = s[s["west"] == west]
    r = np.corrcoef(g["elev"], g["med"])[0, 1]
    b, a = np.polyfit(g["elev"], g["med"], 1)
    xx = np.array([g["elev"].min(), g["elev"].max()])
    ax.plot(xx, a + b * xx, color=color, lw=1.6)
    ax.scatter(g["elev"], g["med"], s=34, color=color, alpha=0.85, lw=0.5, edgecolor=dcc.WHITE, label=f"{lab}: r = {r:.2f}")
    out.append((lab, r, b * 1000, len(g)))
for name in ("Paradise", "Easy Pass", "Stevens Pass", "Snoqualmie Pass", "Harts Pass", "Bumping Ridge"):
    m = s[s["name"] == name]
    if len(m):
        r = m.iloc[0]
        ax.annotate(name, (r["elev"], r["med"]), xytext=(6, 5), textcoords="offset points", family=dcc.TEXT, size=7.5, color=dcc.INK)
ax.set_xlabel("Station elevation (feet)", family=dcc.TEXT, size=9)
ax.set_ylabel("Median snow water on April 1, 1991–2020 (inches)", family=dcc.TEXT, size=9)
ax.xaxis.set_major_formatter(__import__("matplotlib.ticker", fromlist=["FuncFormatter"]).FuncFormatter(lambda v, _: f"{v:,.0f}"))
ax.legend(loc="upper left", prop={"family": dcc.TEXT, "size": 8.5})
(lw, rw, bw, nw), (le, re_, be, ne) = out
fig.text(0.74, 0.70, "WHAT THE LINES SAY", family=dcc.MONO, size=7.5, color=dcc.LAVA, va="top")
fig.text(0.74, 0.665, (f"Across all {len(s)} stations, r = {r_all:.2f}:\nhigher does mean snowier, loosely.\n\n"
                       f"Split by the mountains and the link\ntightens on the wet west side\n(r = {rw:.2f}), where each 1,000 ft\n"
                       f"adds about {bw:.0f} inches of water.\n\nEast of the crest the storms have\nalready dropped their snow, so even\n"
                       f"high stations hold less (r = {re_:.2f})."),
         family=dcc.TEXT, size=8.4, color=dcc.INK, va="top", linespacing=1.45)
dcc.frame(fig, 15, subtitle=("Every Washington SNOTEL snow station with a 30-year normal: how high it sits against how much water is in the snow "
                             "on April 1 in a typical year. Correlated, yes, but geography sorts the dots into two families."),
          source="NRCS SNOTEL via the AWDB report generator (median 1991–2020)",
          note=f"{n_all - len(s)} newer stations have no 30-year median and are left out. The split at 121.2°W is a rough line for the Cascade crest.")
dcc.save(fig, 15, alt=(f"A scatter of {len(s)} Washington SNOTEL stations, elevation versus median April 1 snow water equivalent, "
                       f"with fits for west (r {rw:.2f}) and east (r {re_:.2f}) of the Cascades; overall r {r_all:.2f}."))
