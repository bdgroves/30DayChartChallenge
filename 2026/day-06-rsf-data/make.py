"""Day 6 · RSF Data: Reporters Without Borders' 2026 World Press Freedom Index for the countries I've
visited, with each one's change since 2025, over the five RSF bands."""
import sys
from pathlib import Path
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "toolkit"))
import dcc
import numpy as np
import pandas as pd
from places import VISITED

r = pd.read_csv(HERE / "data" / "rsf_2026.csv", sep=";", encoding="latin-1")
num = lambda s: pd.to_numeric(s.astype(str).str.replace(",", "."), errors="coerce")
r["score"], r["prev"] = num(r["Score 2026"]), num(r["Score N-1"])
v = r[r["ISO"].isin(VISITED)].sort_values("score").reset_index(drop=True)
v["name"] = v["ISO"].map(VISITED).replace({"Dominican Republic": "Dominican Rep."})
missing = sorted(VISITED[i] for i in set(VISITED) - set(r["ISO"]))

BANDS = [(0, 40, "Very serious", "#8f2d22"), (40, 55, "Difficult", "#c86a4f"), (55, 70, "Problematic", "#ddb07a"),
         (70, 85, "Satisfactory", "#b9c6a2"), (85, 100, "Good", "#7f9b78")]
fig, ax = dcc.figure("portrait", box=(0.24, 0.13, 0.62, 0.64))
dcc.style_axes(ax, grid="")
n = len(v)
for lo, hi, lab, c in BANDS:
    ax.axvspan(lo, hi, color=c, alpha=0.22, lw=0)
    ax.text((lo + hi) / 2, n + 1.4, lab.upper(), ha="center", va="bottom", family=dcc.MONO, size=6.6, color=dcc.ASH)
# every country, as a faint strip on top
ax.scatter(r["score"], np.full(len(r), n + 0.6), marker="|", s=60, color=dcc.STONE, alpha=0.45, lw=0.8)
ax.text(-1, n + 0.6, "all 180", ha="right", va="center", family=dcc.MONO, size=6.5, color=dcc.STONE)
for i, row in v.iterrows():
    up = row["score"] >= row["prev"]
    ax.annotate("", (row["score"], i), (row["prev"], i),
                arrowprops=dict(arrowstyle="-|>", color=dcc.SAGE if up else dcc.LAVA, lw=1.1, mutation_scale=7,
                                shrinkA=0, shrinkB=3))
    ax.scatter(row["score"], i, s=34, color=dcc.INK, zorder=3, lw=0)
    bold = row["ISO"] in ("USA", "ZAF")
    ax.text(-1, i, row["name"], ha="right", va="center", family=dcc.TEXT, size=8.6, color=dcc.INK,
            fontweight="bold" if bold else "normal")
    ax.text(101, i, f"{row['score']:.1f}  #{row['Rank']}", ha="left", va="center", family=dcc.MONO, size=7, color=dcc.ASH)
ax.set_xlim(0, 100); ax.set_ylim(-0.8, n + 2.4)
ax.set_yticks([]); ax.spines["left"].set_visible(False)
ax.set_xlabel("RSF score, 2026 (100 = complete freedom)", family=dcc.TEXT, size=8.5)
falls = (v["score"] < v["prev"]).sum()
us = v.set_index("ISO").loc["USA"]
dcc.frame(fig, 6, subtitle=(f"Press freedom in {n} of the countries I've visited, from Reporters Without Borders' 2026 index.\n"
                            f"Arrows run from the 2025 score; {falls} countries slipped. The United States is #{us['Rank']} of 180, "
                            f"\"problematic\"."),
          source="RSF World Press Freedom Index 2026 (rsf.org/en/index)",
          note=f"Not in the index: {', '.join(missing)}.")
dcc.save(fig, 6, alt=(f"Dot plot of RSF 2026 press freedom scores for {n} countries Brooks has visited, lowest {v.iloc[0]['name']} "
                      f"{v.iloc[0]['score']:.1f}, highest {v.iloc[-1]['name']} {v.iloc[-1]['score']:.1f}; the United States "
                      f"{us['score']:.1f}, rank {us['Rank']}."))
