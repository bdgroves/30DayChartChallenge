"""Day 16 · Causation: global temperature against carbon dioxide, 1959-2025. Here the correlation has a known cause:
CO2 absorbs outgoing infrared (Tyndall measured it in 1859; Arrhenius estimated the warming in 1896).
Warming scales with the logarithm of CO2, so the x axis is logarithmic and the fit is a straight line in log CO2."""
import sys
from pathlib import Path
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "toolkit"))
import dcc
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

co2 = pd.read_csv(HERE / "data" / "co2_annmean_mlo.csv", comment="#")
t = pd.read_csv(HERE / "data" / "gistemp_global.csv", skiprows=1, na_values="***")[["Year", "J-D"]].dropna()
d = co2.merge(t, left_on="year", right_on="Year")
d = d[d["year"] <= 2025]
x, y = np.log2(d["mean"] / 280), d["J-D"]
b, a = np.polyfit(x, y, 1)
r = np.corrcoef(x, y)[0, 1]

fig, ax = dcc.figure("wide", box=(0.08, 0.17, 0.60, 0.58))
dcc.style_axes(ax, grid="both")
ax.set_xscale("log", base=2)
sc = ax.scatter(d["mean"], y, c=d["year"], cmap="dmc_heat", s=38, lw=0.4, edgecolor=dcc.WHITE, zorder=3, vmin=1950)
xx = np.linspace(d["mean"].min() - 3, d["mean"].max() + 3, 50)
ax.plot(xx, a + b * np.log2(xx / 280), color=dcc.INK, lw=1.2, ls=(0, (4, 2)), zorder=2)
ax.set_xticks([320, 340, 360, 380, 400, 420]); ax.set_xticklabels([f"{v}" for v in [320, 340, 360, 380, 400, 420]])
ax.set_xlabel("Carbon dioxide at Mauna Loa, parts per million (log scale)", family=dcc.TEXT, size=9)
ax.set_ylabel("Global temperature vs 1951–1980 (°C)", family=dcc.TEXT, size=9)
for yr in (1959, 1998, 2016, 2024, 2025):
    rr = d[d["year"] == yr].iloc[0]
    ax.annotate(str(yr), (rr["mean"], rr["J-D"]), xytext=(-12 if yr == 2025 else 6, 7 if yr != 2025 else -12),
                textcoords="offset points", family=dcc.MONO, size=7, color=dcc.INK)
fig.text(0.72, 0.72, "WHY THIS ONE IS CAUSAL", family=dcc.MONO, size=7.5, color=dcc.LAVA, va="top")
fig.text(0.72, 0.685, ("Correlation alone proves nothing.\nThis link has a mechanism measured\nin the lab long before the warming:\n\n"
                       "1859 · John Tyndall shows CO₂\nabsorbs heat radiation.\n"
                       "1896 · Svante Arrhenius works out\nhow much doubling it would warm\nthe Earth.\n\n"
                       "Because the effect grows with the\nlogarithm of CO₂, the points fall\non a straight line here."),
         family=dcc.TEXT, size=8.3, color=dcc.INK, va="top", linespacing=1.4)
dcc.frame(fig, 16, subtitle=(f"Every year since 1959: the world's average temperature against the CO₂ in the air that year (r = {r:.2f}). "
                             f"The dashed fit works out to about {b:.1f} °C per doubling."),
          source="NOAA Global Monitoring Lab (Mauna Loa annual mean CO2); NASA GISTEMP v4 (global land and ocean)",
          note="That slope isn't the climate sensitivity: methane and other gases rose alongside CO2, aerosols offset some warming, and the oceans are still catching up.")
dcc.save(fig, 16, alt=(f"A scatter of global temperature anomaly against Mauna Loa CO₂ from 1959 to 2025, colored by year, on a log CO₂ axis, "
                       f"falling along a straight line (r {r:.2f}) from {d['mean'].min():.0f} ppm to {d['mean'].max():.0f} ppm."))
print(round(b, 2), round(r, 3))
