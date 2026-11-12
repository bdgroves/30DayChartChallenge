"""Day 26 · Trend: Lakewood's yearly average temperature, 1950-2025 (ERA5 reanalysis), with a least-squares trend line,
its 95% confidence band (where the trend itself could lie) and a 95% prediction band (where a single year could land)."""
import sys
from pathlib import Path
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "toolkit"))
import dcc
import numpy as np
import pandas as pd
from scipy import stats

df = pd.read_csv(HERE.parent / "day-08-circular" / "data" / "lakewood_daily.csv", parse_dates=["time"])
yr = df.groupby(df["time"].dt.year)["temperature_2m_mean"].mean()
x, y = yr.index.values.astype(float), yr.values
n = len(x)
res = stats.linregress(x, y)
xx = np.linspace(x.min(), x.max(), 200)
fit = res.intercept + res.slope * xx
s = np.sqrt(np.sum((y - (res.intercept + res.slope * x)) ** 2) / (n - 2))
t = stats.t.ppf(0.975, n - 2)
se_mean = s * np.sqrt(1 / n + (xx - x.mean()) ** 2 / np.sum((x - x.mean()) ** 2))
se_pred = s * np.sqrt(1 + 1 / n + (xx - x.mean()) ** 2 / np.sum((x - x.mean()) ** 2))
lo_s, hi_s = (res.slope - t * res.stderr) * 10, (res.slope + t * res.stderr) * 10

fig, ax = dcc.figure("wide", box=(0.08, 0.17, 0.66, 0.58))
dcc.style_axes(ax, grid="y")
ax.fill_between(xx, fit - t * se_pred, fit + t * se_pred, color=dcc.MIST, alpha=0.55, lw=0, label="95% of single years should land here")
ax.fill_between(xx, fit - t * se_mean, fit + t * se_mean, color=dcc.LAVA, alpha=0.25, lw=0, label="95% confidence band for the trend")
ax.plot(xx, fit, color=dcc.LAVA, lw=1.8, label=f"trend: +{res.slope * 10:.2f} °F a decade")
ax.plot(x, y, color=dcc.INK, lw=0.8, alpha=0.5)
ax.scatter(x, y, s=16, color=dcc.INK, zorder=3, lw=0)
for k in (yr.idxmax(), yr.idxmin()):
    ax.annotate(f"{k}: {yr[k]:.1f}°F", (k, yr[k]), xytext=(0, 9 if k == yr.idxmax() else -14), textcoords="offset points",
                ha="center", family=dcc.MONO, size=7.5, color=dcc.INK)
ax.set_xlim(x.min() - 1, x.max() + 1)
ax.set_ylabel("Yearly average temperature (°F)", family=dcc.TEXT, size=9)
ax.legend(loc="upper left", prop={"family": dcc.TEXT, "size": 8.2})
fig.text(0.77, 0.70, "HOW SURE?", family=dcc.MONO, size=7.5, color=dcc.LAVA, va="top")
fig.text(0.77, 0.665, (f"The trend is about +{res.slope * 10:.2f} °F per\ndecade. With this much year-to-year\nwobble, the data allow "
                       f"anything from\n+{lo_s:.2f} to +{hi_s:.2f} °F a decade\n(95% confidence).\n\n"
                       f"Even the low end means warming.\nA slope this steep would be very\nunlikely if there were no trend\n"
                       f"(p < 0.001).\n\nOver {n} years the line rose\n{res.slope * (x.max() - x.min()):.1f} °F."),
         family=dcc.TEXT, size=8.3, color=dcc.INK, va="top", linespacing=1.45)
dcc.frame(fig, 26, subtitle=(f"Lakewood's average temperature for every year from {int(x.min())} to {int(x.max())}, with a straight-line trend "
                             "and two kinds of uncertainty: where the trend could lie, and where any one year could fall."),
          source="Open-Meteo historical weather API (ERA5 reanalysis), Lakewood city center; ordinary least squares",
          note="A straight line is the simplest summary, not a claim that warming is steady; reanalysis is a model of the weather, not a thermometer.")
dcc.save(fig, 26, alt=(f"Yearly average temperatures in Lakewood from {int(x.min())} to {int(x.max())} with a trend of +{res.slope * 10:.2f} °F per "
                       f"decade (95% CI {lo_s:.2f} to {hi_s:.2f}) and shaded confidence and prediction bands."))
print(round(res.slope * 10, 3), round(lo_s, 3), round(hi_s, 3), res.pvalue, yr.idxmax(), yr.idxmin())
