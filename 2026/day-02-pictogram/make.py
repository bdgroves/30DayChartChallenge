"""Day 2 · Pictogram: twenty concerts I've logged, one ticket each, a row per artist."""
import sys
from pathlib import Path
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "toolkit"))
import dcc
import numpy as np
import pandas as pd
from matplotlib.path import Path as MPath
from matplotlib.patches import PathPatch

df = pd.read_csv(HERE / "data" / "concerts.csv", parse_dates=["date"])
df["artist"] = df["artist"].str.replace("‐", "-")
df["city"] = df["location"].str.split(",").str[0].replace({"Morrison": "Red Rocks"})
first = df.groupby("artist")["date"].min()
n = df.groupby("artist").size()
artists = sorted(n.index, key=lambda a: (-n[a], first[a]))


def ticket(x, y, w=1.0, h=0.62, notch=0.09):
    """A ticket outline with a half-circle bite out of each long side."""
    t = np.linspace(0, np.pi, 12)
    cx = x + w * 0.70
    top = [(x, y + h), (cx - notch, y + h)] + [(cx - notch * np.cos(a), y + h - notch * np.sin(a)) for a in t] + [(x + w, y + h)]
    bot = [(x + w, y), (cx + notch, y)] + [(cx + notch * np.cos(a), y + notch * np.sin(a)) for a in t] + [(x, y)]
    verts = top + bot + [(x, y + h)]
    return MPath(verts, [MPath.MOVETO] + [MPath.LINETO] * (len(verts) - 2) + [MPath.CLOSEPOLY])


COLORS = {"Coldplay": dcc.LAKE, "Midnight Oil": dcc.LAVA, "U2": dcc.GOLD}
fig, ax = dcc.figure("portrait", axes=False, box=(0.07, 0.07, 0.86, 0.73))
ROW = 1.0
for r, a in enumerate(artists):
    y = -r * ROW
    shows = df[df["artist"] == a].sort_values("date")
    ax.text(-0.15, y + 0.31, a, ha="right", va="center", family=dcc.TEXT, size=10, color=dcc.INK)
    for k, (_, s) in enumerate(shows.iterrows()):
        x = k * 1.18
        c = COLORS.get(a, dcc.SAGE)
        ax.add_patch(PathPatch(ticket(x, y), facecolor=c, edgecolor="none"))
        ax.plot([x + 0.70, x + 0.70], [y + 0.1, y + 0.52], color=dcc.PARCHMENT, lw=0.8, ls=(0, (1.5, 1.5)))
        ax.text(x + 0.35, y + 0.31, s["date"].strftime("%Y"), ha="center", va="center", family=dcc.MONO,
                size=7.5, color=dcc.WHITE)
        ax.text(x + 0.35, y - 0.06, s["city"], ha="center", va="top", family=dcc.MONO, size=5.6, color=dcc.STONE)
ax.set_xlim(-4.3, 3.6); ax.set_ylim(-len(artists) * ROW + 0.3, 0.9); ax.set_aspect("equal")

seattle = (df["city"] == "Seattle").sum()
dcc.frame(fig, 2, subtitle=(f"The {len(df)} concerts in my setlist.fm log, {df['date'].dt.year.min()}–{df['date'].dt.year.max()}: "
                            f"one ticket a show, a row per artist.\n{seattle} were in Seattle. "
                            "Midnight Oil and Coldplay have had me three times each."),
          source="setlist.fm, via brooksgroves.com/concerts")
dcc.save(fig, 2, alt=(f"A pictogram of {len(df)} concert tickets, one row per artist: "
                      + "; ".join(f"{a} {n[a]}" for a in artists) + "."))
