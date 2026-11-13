"""Day 28 · Modeling: a toy model of Whitman's 2025 NCAA Division III tournament, using only what was known before the
first game (Nov 15, 2025), set against what happened.

Model, all assumptions stated:
  * Whitman's scoring and conceding rates come from its 20 regular-season games: Gamma posteriors.
  * Tournament opponents are tougher than Whitman's usual schedule by a factor m per round; log m is drawn from a
    normal distribution whose center rises each round (survivors get better). Whitman scores rate/m, concedes rate*m.
  * Goals in a game are Poisson; a draw goes to penalties, a coin flip.
  * 400 draws of the uncertain inputs × 2,500 simulated tournaments each. The bars show the average chance of reaching
    each round; the whiskers show the middle 80% of those chances across input draws.
This is a toy for showing uncertainty, not a forecast."""
import sys
from pathlib import Path
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "toolkit"))
import dcc
import numpy as np
import pandas as pd

g = pd.read_csv(HERE.parent / "day-27-animation" / "data" / "whitman_2025.csv")
ncaa = g[g["date"] >= "2025-11-15"]
g = g[g["date"] < "2025-11-15"]                       # only what was known before the tournament
GF, GA, N = g["gf"].sum(), g["ga"].sum(), len(g)
ROUNDS = ["Round of 64\nNov 15", "Round of 32\n", "Sweet 16\n", "Elite 8\n", "Final four\n", "Final\n"]
REACH = ["Win round 1", "Win round 2", "Win round 3", "Reach final four", "Reach final", "Win it all"]
MU = np.array([0.20, 0.35, 0.50, 0.55, 0.62, 0.68])          # log toughness of a typical opponent, by round
SD = 0.30
rng = np.random.default_rng(2025)
P = []
for _ in range(400):
    att, dfn = rng.gamma(GF + 1, 1 / N), rng.gamma(GA + 1, 1 / N)
    mu = MU + rng.normal(0, 0.12)                           # how tough the field is, itself uncertain
    alive = np.ones(2500, bool)
    reach = []
    for r in range(6):
        m = np.exp(rng.normal(mu[r], SD, alive.size))
        us, them = rng.poisson(att / m), rng.poisson(dfn * m)
        win = (us > them) | ((us == them) & (rng.random(alive.size) < 0.5))
        alive &= win
        reach.append(alive.mean())
    P.append(reach)
P = np.array(P)
mean, lo, hi = P.mean(0), np.quantile(P, 0.1, 0), np.quantile(P, 0.9, 0)

fig, ax = dcc.figure("wide", box=(0.08, 0.17, 0.62, 0.56))
dcc.style_axes(ax, grid="y")
x = np.arange(6)
ax.bar(x, mean * 100, color=[dcc.LAKE] * 5 + [dcc.GOLD], width=0.62, lw=0)
ax.errorbar(x, mean * 100, yerr=[(mean - lo) * 100, (hi - mean) * 100], fmt="none", ecolor=dcc.INK, elinewidth=1.1, capsize=5)
for i in x:
    ax.text(i, hi[i] * 100 + 2.5, f"{mean[i]:.0%}", ha="center", family=dcc.MONO, size=9, color=dcc.INK)
    ax.text(i, -6, REACH[i], ha="center", va="top", family=dcc.TEXT, size=8.4, color=dcc.INK)
    ax.text(i, -12.5, ROUNDS[i].split("\n")[1], ha="center", va="top", family=dcc.MONO, size=7, color=dcc.STONE)
r0 = ncaa.iloc[0]
ax.annotate(f"What happened: lost {r0['ga']}–{r0['gf']}\nto {r0['opponent']} in round 1", (0, mean[0] * 100 / 2),
            xytext=(0.75, 78), family=dcc.TEXT, size=8.6, color=dcc.LAVA, fontweight="bold",
            arrowprops=dict(arrowstyle="-|>", color=dcc.LAVA, lw=0.9))
ax.set_xticks([]); ax.set_ylim(0, 100); ax.set_xlim(-0.6, 5.6)
ax.yaxis.set_major_formatter(__import__("matplotlib.ticker", fromlist=["FuncFormatter"]).FuncFormatter(lambda v, _: f"{v:.0f}%"))
ax.set_ylabel("Chance in the model", family=dcc.TEXT, size=9)
fig.text(0.73, 0.70, "THE MODEL, PLAINLY", family=dcc.MONO, size=7.5, color=dcc.LAVA, va="top")
fig.text(0.73, 0.665, (f"Whitman scored {GF} and allowed {GA}\nin {N} games. Tournament opponents\nare assumed tougher each round,\n"
                       "by an amount that is itself a guess.\nGoals are random (Poisson); a draw\nis a coin-flip shootout.\n\n"
                       "Bars: the average chance.\nWhiskers: the middle 80% when the\nguesses are varied.\n\n"
                       "A toy for showing uncertainty,\nnot a betting line."),
         family=dcc.TEXT, size=8.3, color=dcc.INK, va="top", linespacing=1.45)
dcc.frame(fig, 28, subtitle=(f"Whitman's chances in the 2025 NCAA Division III tournament, from a small model built only on its regular season "
                             f"({GF} goals for, {GA} against), one million simulated tournaments, and then what happened."),
          source="Model by me; inputs from Whitman Athletics stats (2025 season)",
          note=f"The full list of assumptions is at the top of make.py. The model gave about a {1 - mean[0]:.0%} chance of going out in that first game.")
dcc.save(fig, 28, alt=("A bar chart of modeled chances for Whitman in the 2025 NCAA Division III tournament: "
                       + "; ".join(f"{r.lower()} {m:.0%}" for r, m in zip(REACH, mean)) + ", with 80% whiskers."))
print([f"{m:.3f} ({l:.3f}-{h:.3f})" for m, l, h in zip(mean, lo, hi)])
