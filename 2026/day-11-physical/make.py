"""Day 11 · Physical: how much land lies at each elevation in three boxes around ranges I love
(Copernicus 30 m elevation): the Ruby Mountains (NV), Yosemite's high country (CA) and Mount Rainier (WA)."""
import sys
from pathlib import Path
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "toolkit"))
import dcc
import numpy as np
import pandas as pd

h = pd.read_csv(HERE / "data" / "hypsometry.csv")
FT = 3.28084
ORDER = [("Ruby Mountains", "Ruby Dome", 11387, dcc.GOLD), ("Yosemite high country", "Mount Lyell", 13114, dcc.SAGE),
         ("Mount Rainier", "Columbia Crest", 14410, dcc.LAKE)]      # official summit heights, feet
fig, ax = dcc.figure("wide", box=(0.08, 0.18, 0.86, 0.57))
dcc.style_axes(ax, grid="y")
W = 0.42
for k, (name, peak, top_ft, color) in enumerate(ORDER):
    g = h[h["range"] == name].sort_values("elev_m")
    share = g["cells"] / g["cells"].sum()
    s = np.convolve(share, np.ones(3) / 3, mode="same")
    s = s / s.max() * W
    y = (g["elev_m"] + 25) * FT
    ax.fill_betweenx(y, k - s, k + s, color=color, lw=0, alpha=0.9)
    z = np.repeat(g["elev_m"].values + 25, g["cells"].values)
    med = np.median(z) * FT
    ax.plot([k - W * 0.9, k + W * 0.9], [med, med], color=dcc.INK, lw=0.8, ls=(0, (2, 2)))
    ax.text(k + W * 0.95, med, f"half the land is\nabove {med:,.0f} ft", va="center", family=dcc.TEXT, size=7.5, color=dcc.INK)
    ax.scatter(k, top_ft, marker="^", s=60, color=dcc.INK, zorder=4)
    ax.text(k, top_ft + 450, f"{peak}\n{top_ft:,} ft", ha="center", va="bottom", family=dcc.TEXT, size=7.5, color=dcc.INK)
    ax.text(k, -500, name, ha="center", va="top", family=dcc.TEXT, size=10, color=dcc.INK)
ax.set_xlim(-0.6, 2.95); ax.set_ylim(0, 16500)
ax.set_xticks([]); ax.spines["bottom"].set_visible(False)
ax.yaxis.set_major_formatter(__import__("matplotlib.ticker", fromlist=["FuncFormatter"]).FuncFormatter(lambda v, _: f"{v:,.0f} ft"))
dcc.frame(fig, 11, subtitle=("How much land lies at each elevation around three ranges I love: the wider the shape, the more ground at that height. "
                             "The Rubies rise from a high Great Basin floor; Yosemite's high country is a broad granite plateau; "
                             "Rainier is one great volcano standing over forested ridges."),
          source="Copernicus GLO-30 elevation (ESA, via AWS Open Data), 50 m bands",
          note="Boxes: Ruby Mountains 40.30–40.82°N, 115.22–115.62°W; Yosemite 37.65–37.98°N, 119.25–119.75°W; Rainier 46.72–47.00°N, 121.55–121.95°W. "
               "Summit heights are the official ones (NPS, USGS).")
dcc.save(fig, 11, alt="Three violin shapes of land elevation for boxes around the Ruby Mountains, Yosemite's high country and Mount Rainier, "
                      "with summits marked: Ruby Dome 11,387 ft, Mount Lyell 13,114 ft, Columbia Crest 14,410 ft.")
