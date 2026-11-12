"""
dcc — the house style for #30DayChartChallenge 2026.

Same look as brooksgroves.com: parchment and ink, lava and gold, Playfair Display for
titles, Libre Baskerville for text, DM Mono for labels. Every chart gets the same frame:
the day and theme up top, the title, and a credit line with the data source.

    import dcc
    fig, ax = dcc.figure("square")              # square, portrait (4:5), wide (16:9)
    ...plot on ax...
    dcc.frame(fig, 28, subtitle="...", source="USGS Did You Feel It?")
    dcc.save(fig, 28, alt="Alt text describing the chart for screen readers.")

Theme and title come from days.yml, so they're spelled the same everywhere.
"""
from __future__ import annotations

from pathlib import Path

import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib import font_manager
from matplotlib.colors import LinearSegmentedColormap

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent                          # the 2026/ folder

# ── palette (brooksgroves.com) ────────────────────────────────────────────────
INK = "#1c1a16"
PARCHMENT = "#f5f0e8"
CREAM = "#ede8dc"
WHITE = "#fdfaf4"
STONE = "#7a7268"
ASH = "#6b6560"
MIST = "#d6d0c4"
GOLD = "#c8922a"
LAVA = "#c0392b"
SAGE = "#6f8a6a"
LAKE = "#3f6f8a"
NIGHT = "#14171c"                           # dark-mode paper

PALETTE = dict(ink=INK, parchment=PARCHMENT, cream=CREAM, white=WHITE, stone=STONE, ash=ASH,
               mist=MIST, gold=GOLD, lava=LAVA, sage=SAGE, lake=LAKE, night=NIGHT)
CATEGORICAL = [LAVA, LAKE, GOLD, SAGE, "#8a5a83", ASH]

# sequential ramps that start at the paper color
SEQ_HEAT = LinearSegmentedColormap.from_list("dmc_heat", [CREAM, "#e8c48a", GOLD, LAVA, "#6e1f17"])
SEQ_WATER = LinearSegmentedColormap.from_list("dmc_water", [CREAM, "#a9c4cf", LAKE, "#1f3a4a"])
SEQ_LAND = LinearSegmentedColormap.from_list("dmc_land", [CREAM, "#c9d1b0", SAGE, "#3e5238"])
DIVERGING = LinearSegmentedColormap.from_list("dmc_div", [LAKE, "#a9c4cf", CREAM, "#e8b0a8", LAVA])
for _c in (SEQ_HEAT, SEQ_WATER, SEQ_LAND, DIVERGING):
    mpl.colormaps.register(_c, force=True)

SIZES = {                                    # inches at 300 dpi
    "square": (8, 8),                        # 2400 × 2400
    "portrait": (8, 10),                     # 2400 × 3000, 4:5 for Instagram and Bluesky
    "wide": (10.667, 6),                     # 3200 × 1800, 16:9
}
DPI = 300

TITLE = "Playfair Display"
TEXT = "Libre Baskerville"
MONO = "DM Mono"


def setup() -> None:
    """Register the bundled fonts and set matplotlib defaults. Called on import."""
    for f in (HERE / "fonts").glob("*.ttf"):
        font_manager.fontManager.addfont(str(f))
    mpl.rcParams.update({
        "font.family": TEXT,
        "xtick.labelsize": 9, "ytick.labelsize": 9,
        "font.size": 10,
        "axes.edgecolor": MIST,
        "axes.labelcolor": INK,
        "text.color": INK,
        "xtick.color": STONE,
        "ytick.color": STONE,
        "figure.facecolor": PARCHMENT,
        "axes.facecolor": PARCHMENT,
        "savefig.facecolor": PARCHMENT,
        "legend.frameon": False,
        "legend.fontsize": 8,
    })


setup()


def day_info(day: int) -> dict:
    """The day's entry in days.yml (theme, title, pitch, ...)."""
    import yaml
    plan = yaml.safe_load((ROOT / "days.yml").read_text(encoding="utf-8"))
    for d in plan["days"]:
        if d["day"] == day:
            return d
    raise KeyError(f"day {day} is not in days.yml")


def day_dir(day: int) -> Path:
    """The day's folder, e.g. 2026/day-28-feeling."""
    found = sorted(ROOT.glob(f"day-{day:02d}-*"))
    if not found:
        raise FileNotFoundError(f"No folder for day {day}; run toolkit/build.py first")
    return found[0]


def figure(size: str = "square", dark: bool = False, box=(0.11, 0.14, 0.83, 0.64), axes: bool = True):
    """A figure in one of SIZES with one chart axes inside the frame. Returns (fig, ax).

    The default box leaves room for a two-line subtitle and the tick labels; pass box to change
    it. axes=False hides the axes entirely (for maps, diagrams and image-like charts)."""
    paper = NIGHT if dark else PARCHMENT
    fig = plt.figure(figsize=SIZES[size], facecolor=paper)
    fig._dmc_dark = dark
    ax = fig.add_axes(box)
    ax.set_facecolor(paper)
    if axes:
        style_axes(ax, dark)
    else:
        ax.set_axis_off()
    return fig, ax


def style_axes(ax, dark: bool = False, grid: str = "y") -> None:
    """House axes: no top or right spine, quiet gridlines on `grid` ("x", "y", "both" or ""),
    DM Mono tick labels in stone, axis labels in Libre Baskerville."""
    ink, line = (MIST, "#3a3f47") if dark else (STONE, MIST)
    for side in ("top", "right"):
        ax.spines[side].set_visible(False)
    for side in ("left", "bottom"):
        ax.spines[side].set_color(line)
    ax.tick_params(colors=ink, length=0, pad=6, labelsize=9)
    for lab in ax.get_xticklabels() + ax.get_yticklabels():
        lab.set_fontfamily(MONO)
    if grid:
        ax.grid(True, axis=grid, color=line, linewidth=0.6)
        ax.set_axisbelow(True)
    ax.xaxis.label.set_color(WHITE if dark else INK)
    ax.yaxis.label.set_color(WHITE if dark else INK)


def frame(fig, day: int, subtitle: str = "", source: str = "", title: str | None = None,
          note: str = "") -> None:
    """Header (day · theme, title, subtitle) and footer (data source, credit)."""
    d = day_info(day)
    dark = getattr(fig, "_dmc_dark", False)
    ink = WHITE if dark else INK
    soft = MIST if dark else STONE
    w, h = fig.get_size_inches()
    scale = min(w, h) / 8          # text sized to the short side; positions are fractions of the figure
    left = 0.05
    fig.text(left, 0.965, f"#30DAYCHARTCHALLENGE  ·  DAY {day:02d}  ·  {d['theme'].upper()}",
             family=MONO, size=8.5 * scale, color=LAVA, va="top")
    fig.text(left, 0.94, title or d["title"], family=TITLE, weight=900, size=26 * scale,
             color=ink, va="top")
    if subtitle:
        fig.text(left, 0.875, subtitle, family=TEXT, size=10.5 * scale, color=soft, va="top",
                 wrap=True, linespacing=1.5)
    line_y = 0.06
    fig.add_artist(plt.Line2D([left, 1 - left], [line_y + 0.02] * 2, color=MIST if not dark else ASH,
                              lw=0.8 * scale, transform=fig.transFigure))
    if source:
        fig.text(left, line_y, f"DATA  {source}", family=MONO, size=7 * scale, color=soft, va="top")
    if note:
        fig.text(left, line_y - 0.022, note, family=TEXT, style="italic", size=7.5 * scale,
                 color=soft, va="top")
    fig.text(1 - left, line_y, "BROOKS GROVES  ·  BROOKSGROVES.COM", family=MONO, size=7 * scale,
             color=ink, va="top", ha="right")


def save(fig, day: int, name: str = "chart", alt: str = "") -> Path:
    """Save out/<name>.png at full resolution, plus out/alt.txt for the post's alt text."""
    out = day_dir(day) / "out"
    out.mkdir(exist_ok=True)
    path = out / f"{name}.png"
    for a in fig.axes:                                  # tick labels in DM Mono, after plotting made them
        if a.axison:
            for lab in a.get_xticklabels() + a.get_yticklabels():
                lab.set_fontfamily(MONO)
    fig.savefig(path, dpi=DPI, facecolor=fig.get_facecolor())
    if name == "chart" and fig.axes:
        crop(fig, path, out / "crop.jpg")
    if alt:
        (out / "alt.txt").write_text(alt.strip() + "\n", encoding="utf-8")
    plt.close(fig)
    print(f"saved {path.relative_to(ROOT.parent)}")
    return path


def crop(fig, png: Path, dest: Path, ax=None) -> None:
    """Save just the chart (the first axes, or ax) from the finished poster, for gallery thumbnails."""
    from PIL import Image
    ax = ax or fig.axes[0]
    x0, y0, x1, y1 = ax.get_position().extents          # figure fractions, origin bottom-left
    im = Image.open(png)
    W, H = im.size
    pad = 0.07 if ax.axison else 0.005                 # keep the tick labels in a chart's thumbnail
    box = (max(0, int((x0 - pad) * W)), max(0, int((1 - y1 - pad) * H)),
           min(W, int((x1 + pad) * W)), min(H, int((1 - y0 + pad) * H)))
    c = im.crop(box).convert("RGB")
    c.thumbnail((1200, 1200))
    c.save(dest, quality=86, optimize=True)


def scalebar(ax, km: float, loc=(0.05, 0.05), color=INK, crs_units_per_km: float = 1000) -> None:
    """A plain scale bar for projected axes in meters."""
    x0, x1 = ax.get_xlim()
    y0, y1 = ax.get_ylim()
    x = x0 + (x1 - x0) * loc[0]
    y = y0 + (y1 - y0) * loc[1]
    length = km * crs_units_per_km
    ax.plot([x, x + length], [y, y], color=color, lw=1.2, solid_capstyle="butt")
    ax.text(x + length / 2, y + (y1 - y0) * 0.012, f"{km:g} km", ha="center", va="bottom",
            family=MONO, size=7, color=color)


def place_labels(ax, items, gap: float = 4, taken=None, **kw):
    """Label points without overlaps. items: (x, y, text) in data coordinates, most important first.

    Tries right, left, above and below each point, keeps the first spot that collides with no
    label already placed (or with the axes edge), and skips the label if none is free.
    Pass the same list as taken to several calls to keep them clear of each other.
    Returns the texts that were placed."""
    fig = ax.figure
    r = fig.canvas.get_renderer()
    taken = [] if taken is None else taken
    placed = []
    for x, y, text in items:
        for ha, va, dx, dy in (("left", "center", 1, 0), ("right", "center", -1, 0),
                               ("center", "bottom", 0, 1), ("center", "top", 0, -1)):
            t = label(ax, x, y, text, ha=ha, va=va, **kw)
            from matplotlib.transforms import offset_copy
            t.set_transform(offset_copy(ax.transData, fig=fig, x=dx * gap, y=dy * gap, units="points"))
            bb = t.get_window_extent(r).expanded(1.04, 1.1)
            inside = ax.get_window_extent(r).contains(bb.x0, bb.y0) and ax.get_window_extent(r).contains(bb.x1, bb.y1)
            if inside and not any(bb.overlaps(o) for o in taken):
                taken.append(bb)
                placed.append(t)
                break
            t.remove()
    return placed


def label(ax, x, y, text, size=8, color=INK, halo=PARCHMENT, **kw):
    """A place label with a paper-colored halo so it reads over anything."""
    import matplotlib.patheffects as pe
    kw.setdefault("clip_on", True)                # labels outside the chart aren't drawn
    kw.setdefault("family", TEXT)
    return ax.text(x, y, text, size=size, color=color,
                   path_effects=[pe.withStroke(linewidth=2.5, foreground=halo)], **kw)
