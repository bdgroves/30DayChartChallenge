"""Day 29 · Monochrome: Lakewood's normal range of daily high temperatures (1991-2020) as nested bands, and how 2025 wandered
through them, in shades of a single blue."""
import sys
from pathlib import Path
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "toolkit"))
import dcc
import numpy as np
import pandas as pd

df = pd.read_csv(HERE.parent / "day-08-circular" / "data" / "lakewood_daily.csv", parse_dates=["time"])
df["doy"] = df["time"].dt.dayofyear.clip(upper=365)
base = df[df["time"].dt.year.between(1991, 2020)]
q = base.groupby("doy")["temperature_2m_max"].quantile([0.0, 0.1, 0.25, 0.5, 0.75, 0.9, 1.0]).unstack()
q = q.apply(lambda c: pd.concat([c.iloc[-7:], c, c.iloc[:7]]).rolling(15, center=True).mean().iloc[7:-7])
y = df[df["time"].dt.year == 2025].set_index("doy")["temperature_2m_max"]
BLUE = "#2f5f7a"
from matplotlib.colors import to_rgba
fig, ax = dcc.figure("wide", box=(0.07, 0.15, 0.86, 0.60))
dcc.style_axes(ax, grid="")
for (a, b), alpha, lab in (((0.0, 1.0), 0.12, "record range 1991–2020"), ((0.1, 0.9), 0.22, "8 days in 10"), ((0.25, 0.75), 0.35, "half of all days")):
    ax.fill_between(q.index, q[a], q[b], color=BLUE, alpha=alpha, lw=0, label=lab)
ax.plot(q.index, q[0.5], color=BLUE, lw=0.8, ls=(0, (2, 2)), alpha=0.8, label="median")
ax.plot(y.index, y.values, color=BLUE, lw=1.3, label="2025")
hot = (y > q[0.9].reindex(y.index)).sum(); cold = (y < q[0.1].reindex(y.index)).sum()
rec = (y > q[1.0].reindex(y.index)).sum()
ticks = [1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335]
ax.set_xticks(ticks); ax.set_xticklabels(["JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"])
ax.set_xlim(1, 365); ax.grid(False)
for sp in ("left", "bottom"):
    ax.spines[sp].set_color(BLUE)
for lab in ax.get_xticklabels() + ax.get_yticklabels():
    lab.set_color(BLUE)
ax.yaxis.set_major_formatter(__import__("matplotlib.ticker", fromlist=["FuncFormatter"]).FuncFormatter(lambda v, _: f"{v:.0f}°F"))
leg = ax.legend(loc="upper left", prop={"family": dcc.TEXT, "size": 8.2}, labelcolor=BLUE)
dcc.frame(fig, 29, subtitle=(f"Daily high temperatures in Lakewood: the bands are 1991–2020, from half of all days out to the records; the line is 2025. "
                             f"In 2025, {hot} days beat the 90th percentile for their date and {cold} fell below the 10th."),
          source="Open-Meteo historical weather API (ERA5 reanalysis), Lakewood city center",
          note="Bands smoothed over 15 days. One color only: everything is a shade of the same blue.")
dcc.save(fig, 29, alt=(f"Nested blue bands of normal daily high temperatures in Lakewood for 1991–2020 with the 2025 daily highs as a line; "
                       f"{hot} days above the 90th percentile and {cold} below the 10th; {rec} above the 1991–2020 range."))
print(hot, cold, rec)
