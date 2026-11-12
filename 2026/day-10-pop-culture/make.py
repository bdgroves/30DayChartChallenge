"""Day 10 · Pop Culture: how many songs each of my concerts ran, as a dot histogram (one dot per show
with a setlist on record), labeled by artist."""
import sys
from pathlib import Path
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "toolkit"))
import dcc
import pandas as pd

df = pd.read_csv(HERE.parent / "day-02-pictogram" / "data" / "concerts.csv", parse_dates=["date"])
df["artist"] = df["artist"].str.replace("‐", "-").replace({"The Presidents of the United States of America": "Presidents of the USA"})
known = df[df["songs"] > 0].copy()
none = df[df["songs"] == 0]
COL = {"Coldplay": dcc.LAKE, "Midnight Oil": dcc.LAVA, "U2": dcc.GOLD}
fig, ax = dcc.figure("portrait", box=(0.34, 0.115, 0.58, 0.63))
dcc.style_axes(ax, grid="x")
known = known.sort_values(["songs", "date"]).reset_index(drop=True)
for i, r in known.iterrows():
    c = COL.get(r["artist"], dcc.SAGE)
    ax.plot([0, r["songs"]], [i, i], color=c, lw=2.2, alpha=0.35, solid_capstyle="butt")
    ax.scatter(r["songs"], i, s=70, color=c, lw=0, zorder=3)
    ax.text(r["songs"] + 0.7, i, f"{r['songs']:.0f}", va="center", family=dcc.MONO, size=7.5, color=dcc.INK)
    city = r["location"].split(",")[0].replace("Morrison", "Red Rocks")
    ax.text(-0.8, i + 0.13, r["artist"], ha="right", va="center", family=dcc.TEXT, size=8.4, color=dcc.INK)
    ax.text(-0.8, i - 0.27, f"{city}, {r['date']:%Y}", ha="right", va="center", family=dcc.MONO, size=6, color=dcc.STONE)
ax.axvline(known["songs"].median(), color=dcc.INK, lw=0.8, ls=(0, (3, 2)))
ax.text(known["songs"].median() + 0.3, len(known) - 0.2, "median", family=dcc.MONO, size=7, color=dcc.INK, va="bottom")
ax.set_xlim(0, 31); ax.set_ylim(-0.8, len(known) + 0.4)
ax.set_yticks([]); ax.spines["left"].set_visible(False)
ax.set_xlabel("Songs played", family=dcc.TEXT, size=9)
med = known["songs"].median()
dcc.frame(fig, 10, subtitle=(f"Every show in my setlist.fm log with a setlist on record ({len(known)} of {len(df)}), by how many songs got played; "
                            f"The median show ran {med:.0f} songs. The long nights are the stadium shows: "
                            "Coldplay and U2 at 25 to 28."),
          source="setlist.fm, via brooksgroves.com/concerts",
          note=f"Short counts can be partial setlists, or an opening slot. No setlist on record for {', '.join(f'{a} ({d:%Y})' for a, d in zip(none['artist'], none['date']))}.")
dcc.save(fig, 10, alt=(f"A dot histogram of {len(known)} concerts by number of songs, from {known['songs'].min()} "
                       f"({known.iloc[0]['artist']}) to {known['songs'].max()} ({known.iloc[-1]['artist']}); median {med:.0f}."))
