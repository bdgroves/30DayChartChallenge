"""Day 24 · SCMP Theme: hours of daylight through the year in Lakewood, WA (47.2°N) and Hong Kong (22.3°N),
computed from solar geometry, in the bold, high-contrast spirit of the South China Morning Post's graphics desk."""
import sys
from pathlib import Path
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "toolkit"))
import dcc
import numpy as np
import pandas as pd

def daylight(lat, days):
    """Hours from sunrise to sunset (upper limb, refraction: the sun 0.833° below the horizon), NOAA-style declination."""
    g = 2 * np.pi / 365 * (days - 1)
    decl = (0.006918 - 0.399912 * np.cos(g) + 0.070257 * np.sin(g) - 0.006758 * np.cos(2 * g)
            + 0.000907 * np.sin(2 * g) - 0.002697 * np.cos(3 * g) + 0.00148 * np.sin(3 * g))
    phi = np.radians(lat)
    cos_h = (np.sin(np.radians(-0.833)) - np.sin(phi) * np.sin(decl)) / (np.cos(phi) * np.cos(decl))
    return 2 * np.degrees(np.arccos(np.clip(cos_h, -1, 1))) / 15

days = np.arange(1, 366)
dates = pd.date_range("2026-01-01", periods=365)
lw, hk = daylight(47.17, days), daylight(22.32, days)
YELLOW, BLACK, TEAL = "#ffc20e", "#111111", "#0b6e78"
fig, ax = dcc.figure("wide", box=(0.07, 0.15, 0.86, 0.60))
dcc.style_axes(ax, grid="y")
ax.fill_between(days, lw, hk, where=lw >= hk, color=YELLOW, alpha=0.55, lw=0)
ax.fill_between(days, lw, hk, where=lw < hk, color=TEAL, alpha=0.25, lw=0)
ax.plot(days, lw, color=BLACK, lw=3.2)
ax.plot(days, hk, color=TEAL, lw=3.2)
ax.text(182, lw.max() + 0.35, f"LAKEWOOD  {int(lw.max())}h {int(lw.max() % 1 * 60)}m", ha="center", family=dcc.MONO, size=11,
        color=BLACK, fontweight="medium")
ax.text(355, lw[354] - 0.7, f"{int(lw.min())}h {int(lw.min() % 1 * 60)}m", ha="center", family=dcc.MONO, size=10, color=BLACK, fontweight="medium")
ax.text(182, hk.max() - 0.75, f"HONG KONG  {int(hk.max())}h {int(hk.max() % 1 * 60)}m", ha="center", family=dcc.MONO, size=11,
        color=TEAL, fontweight="medium")
ax.text(10, hk[9] + 0.35, f"{int(hk.min())}h {int(hk.min() % 1 * 60)}m", ha="center", family=dcc.MONO, size=10, color=TEAL, fontweight="medium")
cross = [d for d in range(1, 364) if (lw[d - 1] - hk[d - 1]) * (lw[d] - hk[d]) < 0]
for c in cross:
    ax.axvline(c + 0.5, color=BLACK, lw=0.8, ls=":")
    ax.text(c + 2, 8.6, f"{dates[c]:%b} {dates[c].day}\nequal days", family=dcc.MONO, size=7.5, color=BLACK)
ax.set_xticks([1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335])
ax.set_xticklabels(["JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"])
ax.set_xlim(1, 365); ax.set_ylim(7.5, 17)
ax.yaxis.set_major_formatter(__import__("matplotlib.ticker", fromlist=["FuncFormatter"]).FuncFormatter(lambda v, _: f"{v:.0f}h"))
ax.text(182, 14.35, "Lakewood gets\nthe longer days", family=dcc.TEXT, size=9, color=BLACK, ha="center", fontweight="bold")
ax.text(355, 11.2, "Hong Kong gets\nthe longer days", family=dcc.TEXT, size=9, color=TEAL, ha="right", fontweight="medium")
diff = lw.max() - lw.min(), hk.max() - hk.min()
dcc.frame(fig, 24, subtitle=(f"Daylight through the year at 47° north, where I live, and 22° north, where I've only changed planes. "
                             f"Lakewood's day swings by {diff[0]:.1f} hours between June and December; Hong Kong's by just {diff[1]:.1f}."),
          source="Computed from solar geometry (NOAA declination formula; sunrise and sunset with standard refraction)",
          note="After the South China Morning Post graphics desk: one big comparison, bold labels, strong yellow and black.")
dcc.save(fig, 24, alt=(f"Two curves of daylight hours through the year: Lakewood from {lw.min():.1f} to {lw.max():.1f} hours, Hong Kong from "
                       f"{hk.min():.1f} to {hk.max():.1f}; they cross near the equinoxes."))
print([str(dates[c].date()) for c in cross], round(diff[0], 2), round(diff[1], 2))
