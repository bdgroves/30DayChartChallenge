"""Day 23 · Seasons: snow depth at Paradise on Mount Rainier (SNOTEL 679), one line per winter, by day of the water year."""
import sys
from pathlib import Path
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "toolkit"))
import dcc
import numpy as np
import pandas as pd

d = pd.read_csv(HERE / "data" / "paradise_daily.csv", comment="#", parse_dates=["Date"])
d.columns = ["date", "depth", "swe"]
d["wy"] = d["date"].dt.year + (d["date"].dt.month >= 10)
d["day"] = (d["date"] - pd.to_datetime((d["wy"] - 1).astype(str) + "-10-01")).dt.days
d.loc[(d["depth"] < 0) | (d["depth"] > 400), "depth"] = np.nan
good = d.groupby("wy")["depth"].apply(lambda s: s.notna().mean() > 0.8)
years = [y for y, ok in good.items() if ok]
fig, ax = dcc.figure("wide", box=(0.07, 0.15, 0.62, 0.60))
dcc.style_axes(ax, grid="y")
peaks = {}
for y in years:
    g = d[d["wy"] == y].set_index("day")["depth"].interpolate(limit=5)
    peaks[y] = g.max()
    ax.plot(g.index, g.values, color=dcc.MIST, lw=0.7, zorder=1)
clim = d[d["wy"].isin(years)].groupby("day")["depth"].median().rolling(7, center=True, min_periods=1).mean()
ax.plot(clim.index, clim.values, color=dcc.INK, lw=1.6, ls=(0, (3, 2)), zorder=3, label="median winter")
big = max(peaks, key=peaks.get); small = min(peaks, key=peaks.get)
last = years[-1]
for y, c, lab in ((big, dcc.LAKE, "deepest"), (small, dcc.LAVA, "thinnest"), (last, dcc.GOLD, "latest")):
    g = d[d["wy"] == y].set_index("day")["depth"].interpolate(limit=5)
    ax.plot(g.index, g.values, color=c, lw=2, zorder=4, label=f"{y - 1}–{str(y)[2:]}, {lab}: peak {g.max():.0f} in")
ticks = [0, 31, 61, 92, 123, 151, 182, 212, 243, 273, 304, 335]
ax.set_xticks(ticks); ax.set_xticklabels(["OCT", "NOV", "DEC", "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP"],
                                          family=dcc.MONO, size=8)
ax.set_xlim(0, 365); ax.set_ylim(0, None)
ax.set_ylabel("Snow on the ground (inches)", family=dcc.TEXT, size=9)
ax.legend(loc="upper left", bbox_to_anchor=(1.01, 1), prop={"family": dcc.TEXT, "size": 8.2})
fig.text(0.735, 0.47, (f"Paradise sits at 5,400 feet on\nthe south side of Rainier, one of\nthe snowiest places on Earth\nwhere snow is measured "
                       f"every day.\n\nThe snow usually starts piling up\nin November, tops out around\n"
                       f"{clim.max() / 12:.0f} feet in April and lingers into July."),
         family=dcc.TEXT, size=8.2, color=dcc.INK, va="top", linespacing=1.45)
dcc.frame(fig, 23, subtitle=(f"Snow depth at Paradise, Mount Rainier, through {len(years)} winters, {years[0] - 1}–{str(years[0])[2:]} to {last - 1}–{str(last)[2:]}: one grey line a winter, "
                             "October to September."),
          source="NRCS SNOTEL site 679, Paradise, WA (daily snow depth)",
          note="Winters missing more than a fifth of their daily depth readings are left out, which leaves 2006–07 onward.")
dcc.save(fig, 23, alt=(f"Lines of daily snow depth at Paradise for {len(years)} winters; the deepest peaked at {peaks[big]:.0f} inches in "
                       f"{big - 1}–{big}, the thinnest at {peaks[small]:.0f} in {small - 1}–{small}."))
print(years[0], years[-1], len(years), big, peaks[big], small, peaks[small], round(clim.max(), 1))
