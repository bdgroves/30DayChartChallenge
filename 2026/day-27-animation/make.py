"""Day 27 · Animation: Whitman men's soccer, 2025, game by game. Each bar is a game's goal difference; the line is the
running total. Makes out/animation.gif (and the final frame as the usual out/chart.png)."""
import sys
from pathlib import Path
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "toolkit"))
import dcc
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation, PillowWriter

g = pd.read_csv(HERE / "data" / "whitman_2025.csv", parse_dates=["date"])
g["ncaa"] = g["date"] >= "2025-11-15"                 # the NCAA first round
g.loc[g["loc"] == "neutral", "loc"] = "home"          # Sidearm lists home games as "vs"
g["gd"] = g["gf"] - g["ga"]; g["cum"] = g["gd"].cumsum()
COL = {"W": dcc.LAKE, "T": dcc.GOLD, "L": dcc.LAVA}
NWC = {"Linfield", "Lewis & Clark", "Whitworth", "Pacific (OR)", "Pacific", "Willamette", "George Fox", "Pacific Lutheran", "Puget Sound"}

def draw(fig, ax, k, final=False):
    ax.clear(); dcc.style_axes(ax, grid="y")
    s = g.iloc[:k]
    ax.bar(range(len(s)), s["gd"], color=[COL[w] for w in s["wl"]], width=0.7, lw=0)
    ax.plot(range(len(s)), s["cum"], color=dcc.INK, lw=1.8, marker="o", ms=3.5)
    for i, r in s.iterrows():
        ax.text(i, (r["gd"] if r["gd"] >= 0 else r["gd"]) + (0.35 if r["gd"] >= 0 else -0.35), f"{r['gf']}–{r['ga']}",
                ha="center", va="bottom" if r["gd"] >= 0 else "top", family=dcc.MONO, size=6.5, color=dcc.INK)
    ax.set_xticks(range(len(g)))
    ax.set_xticklabels([("NCAA: " if r["ncaa"] else "at " if r["loc"] == "away" else "") + r["opponent"].replace(" (VA)", "").replace(" (OR)", "")
                        .replace("Mary Hardin-Baylor", "Mary Hardin-B.").replace("Colorado Col.", "Colorado College").replace("Pacific Lutheran", "PLU")
                        for _, r in g.iterrows()], rotation=55, ha="right", family=dcc.TEXT, size=6.8)
    for lab in ax.get_xticklabels()[k:]:
        lab.set_alpha(0.25)
    ax.set_xlim(-0.7, len(g) - 0.3); ax.set_ylim(-4, 32)
    ax.axhline(0, color=dcc.STONE, lw=0.6)
    w, t, l = (s["wl"] == "W").sum(), (s["wl"] == "T").sum(), (s["wl"] == "L").sum()
    ax.text(0.01, 0.96, f"{w}–{l}–{t}", transform=ax.transAxes, family=dcc.TITLE, size=26, fontweight="bold", color=dcc.INK, va="top")
    ax.text(0.01, 0.80, f"goals {s['gf'].sum()}–{s['ga'].sum()}  ·  through {s['date'].iloc[-1]:%b} {s['date'].iloc[-1].day}",
            transform=ax.transAxes, family=dcc.MONO, size=8, color=dcc.STONE, va="top")
    ax.set_ylabel("Goal difference (bars) and running total (line)", family=dcc.TEXT, size=8.5)
    if final:
        ax.annotate("NWC champions, then out\nin the NCAA first round,\n3–2 to Texas Lutheran", (len(g) - 1, s["cum"].iloc[-1]), xytext=(-70, 26),
                    textcoords="offset points", family=dcc.TEXT, size=8, color=dcc.INK, ha="right",
                    arrowprops=dict(arrowstyle="-", color=dcc.STONE, lw=0.6))

def new_fig():
    fig, ax = dcc.figure("wide", box=(0.08, 0.27, 0.86, 0.50))
    dcc.frame(fig, 27, subtitle=("Whitman's 2025 season, game by game: each bar is a game's goal difference (blue a win, gold a draw, red a loss) "
                                 "and the line is the running total. One loss all year, in the NCAA tournament."),
              source="Whitman Athletics stats (whitmanblues.com), via brooksgroves.com/outside")
    return fig, ax

fig, ax = new_fig(); draw(fig, ax, len(g), final=True)
dcc.save(fig, 27, alt=(f"Bars of goal difference for Whitman's {len(g)} games in 2025 with a rising running total to +{g['cum'].iloc[-1]}; "
                       f"record {(g['wl'] == 'W').sum()}-{(g['wl'] == 'L').sum()}-{(g['wl'] == 'T').sum()}, goals {g['gf'].sum()}-{g['ga'].sum()}."))
fig, ax = new_fig()
frames = list(range(1, len(g) + 1)) + [len(g)] * 8
anim = FuncAnimation(fig, lambda k: draw(fig, ax, k, final=(k == len(g))), frames=frames, interval=450)
out = dcc.day_dir(27) / "out" / "animation.gif"
anim.save(out, writer=PillowWriter(fps=2.2), dpi=110, savefig_kwargs={"facecolor": dcc.PARCHMENT})
print("saved", out.name, out.stat().st_size // 1024, "KB")
