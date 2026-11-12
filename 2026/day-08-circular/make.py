"""Day 8 · Circular: 2025 in Lakewood, WA. Each day's low-to-high range is a spoke on a clock of months,
over the 1991-2020 normal range for that day (shaded)."""
import sys
from pathlib import Path
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "toolkit"))
import dcc
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

df = pd.read_csv(HERE / "data" / "lakewood_daily.csv", parse_dates=["time"])
df["doy"] = df["time"].dt.dayofyear.clip(upper=365)
norm = df[df["time"].dt.year.between(1991, 2020)].groupby("doy")[["temperature_2m_max", "temperature_2m_min"]].mean()
norm = norm.rolling(15, center=True, min_periods=1).mean()
y = df[df["time"].dt.year == 2025].set_index("doy")
theta = lambda d: 2 * np.pi * (np.asarray(d) - 1) / 365

fig, _ = dcc.figure("square", axes=False)
fig.axes[0].remove()
ax = fig.add_axes((0.19, 0.115, 0.62, 0.62), projection="polar")
ax.set_theta_zero_location("N"); ax.set_theta_direction(-1)
ax.set_facecolor(dcc.PARCHMENT)
R0 = 0                                     # temperature axis starts at 0 °F... at radius 0
ax.set_ylim(-25, 105)
ax.fill_between(theta(norm.index), norm["temperature_2m_min"], norm["temperature_2m_max"], color=dcc.MIST, alpha=0.8, lw=0)
cmap = plt.get_cmap("dmc_div")
for d, r in y.iterrows():
    mean = (r["temperature_2m_max"] + r["temperature_2m_min"]) / 2
    ax.plot([theta(d)] * 2, [r["temperature_2m_min"], r["temperature_2m_max"]], color=cmap((mean - 30) / 45), lw=1.3,
            solid_capstyle="round")
for t in (32, 50, 70, 90):
    ax.plot(np.linspace(0, 2 * np.pi, 400), [t] * 400, color=dcc.STONE if t != 32 else dcc.LAKE, lw=0.4,
            ls=":" if t != 32 else "-")
    ax.text(np.radians(4), t + 1.5, f"{t}°", family=dcc.MONO, size=6.5, color=dcc.ASH, ha="left", va="bottom")
ax.set_xticks(theta([1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335]))
ax.set_xticklabels(["JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"],
                   family=dcc.MONO, size=7.5, color=dcc.STONE)
ax.set_yticks([]); ax.spines["polar"].set_visible(False); ax.grid(axis="x", color=dcc.MIST, lw=0.5); ax.grid(axis="y", visible=False)
hot = y["temperature_2m_max"].idxmax(); cold = y["temperature_2m_min"].idxmin()
ax.text(0, -25, "2025", ha="center", va="center", family=dcc.TITLE, size=26, color=dcc.INK, fontweight="bold")
for d, key, lab in ((hot, "temperature_2m_max", "hottest"), (cold, "temperature_2m_min", "coldest")):
    v = y.loc[d, key]; date = y.loc[d, "time"]
    ax.annotate(f"{lab}: {v:.0f}°F\n{date:%B} {date.day}", (theta(d), v), xytext=(theta(d) + (0.3 if lab == "hottest" else 0.35), v + (16 if lab == "hottest" else 38)),
                family=dcc.TEXT, size=7.5, color=dcc.INK, ha="center", arrowprops=dict(arrowstyle="-", color=dcc.STONE, lw=0.6))
above = (y["temperature_2m_max"] > norm["temperature_2m_max"].reindex(y.index)).mean()
dcc.frame(fig, 8, subtitle=("Every day of 2025 in Lakewood, Washington: a spoke from the low to the high, January at the top.\n"
                            f"The grey band is the 1991–2020 normal. On {above:.0%} of the days the high beat the normal high."),
          source="Open-Meteo historical weather API (ERA5 reanalysis), Lakewood city center")
dcc.save(fig, 8, alt=(f"A circular chart of 2025 daily temperatures in Lakewood, WA. The hottest day reached {y.loc[hot, 'temperature_2m_max']:.0f}°F "
                      f"on {y.loc[hot, 'time']:%B} {y.loc[hot, 'time'].day}; the coldest night fell to {y.loc[cold, 'temperature_2m_min']:.0f}°F "
                      f"on {y.loc[cold, 'time']:%B} {y.loc[cold, 'time'].day}."))
