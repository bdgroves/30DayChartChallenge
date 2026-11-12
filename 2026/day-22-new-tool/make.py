"""Day 22 · New Tool: Lakewood's rain as a calendar, made with Altair (Vega-Lite), which I hadn't used before.
Altair draws the chart; the PNG it makes is set into the usual frame with matplotlib.
Needs: pip install altair vl-convert-python"""
import sys
from pathlib import Path
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "toolkit"))
import dcc
import io
import altair as alt
import pandas as pd
import vl_convert as vlc
from PIL import Image

vlc.register_font_directory(str(HERE.parent / "toolkit" / "fonts"))
df = pd.read_csv(HERE.parent / "day-08-circular" / "data" / "lakewood_daily.csv", parse_dates=["time"])
df = df[df["time"].dt.year.between(2016, 2025)].copy()
df["year"] = df["time"].dt.year.astype(str)
df["week"] = df["time"].dt.dayofyear.sub(1) // 7 + 1
df["rain"] = df["precipitation_sum"]
wk = df.groupby(["year", "week"], as_index=False).agg(rain=("rain", "sum"), start=("time", "min"))
chart = (alt.Chart(wk).mark_rect(cornerRadius=1.5, stroke=dcc.PARCHMENT, strokeWidth=1.2).encode(
    x=alt.X("week:O", title=None, axis=alt.Axis(values=[1, 5, 9, 13, 18, 22, 26, 31, 35, 40, 44, 48],
                                                 labelExpr="{1:'JAN',5:'FEB',9:'MAR',13:'APR',18:'MAY',22:'JUN',26:'JUL',31:'AUG',35:'SEP',40:'OCT',44:'NOV',48:'DEC'}[datum.value]",
                                                 labelAngle=0, ticks=False, domain=False)),
    y=alt.Y("year:O", title=None, axis=alt.Axis(ticks=False, domain=False)),
    color=alt.Color("rain:Q", title="Rain in the week (inches)",
                    scale=alt.Scale(range=[dcc.CREAM, "#a9c4cf", dcc.LAKE, "#1f3a4a"], domain=[0, 4], clamp=True),
                    legend=alt.Legend(orient="bottom", gradientLength=320, titleFont="Libre Baskerville", titleFontWeight="normal",
                                      labelFont="DM Mono", titleColor=dcc.INK, labelColor=dcc.STONE, direction="horizontal")),
    tooltip=["year", "week", alt.Tooltip("rain:Q", format=".2f")])
    .properties(width=1180, height=420, background=dcc.PARCHMENT)
    .configure_view(strokeWidth=0)
    .configure_axis(labelFont="DM Mono", labelColor=dcc.STONE, labelFontSize=13, grid=False))
png = vlc.vegalite_to_png(chart.to_json(), scale=2)
img = Image.open(io.BytesIO(png))
(HERE / "data").mkdir(exist_ok=True)

fig, ax = dcc.figure("wide", axes=False, box=(0.05, 0.10, 0.90, 0.66))
ax.imshow(img); ax.set_aspect("equal"); ax.axis("off")
yr = df.groupby("year")["rain"].sum()
wet = df.groupby(df["time"].dt.month)["rain"].sum() / 10
dcc.frame(fig, 22, subtitle=(f"Ten years of rain in Lakewood, week by week, 2016–2025: a year on each row. The dry summer stripe is the "
                             f"Northwest's signature; November and December bring the most, about {wet.loc[11]:.0f} and "
                             f"{wet.loc[12]:.0f} inches a month. Wettest year {yr.idxmax()} ({yr.max():.0f} in), driest {yr.idxmin()} ({yr.min():.0f} in)."),
          source="Open-Meteo historical weather API (ERA5 reanalysis), Lakewood city center",
          note="Made with Altair 6 and vl-convert (Vega-Lite), new to me this year, then framed in matplotlib.")
dcc.save(fig, 22, alt=(f"A calendar heatmap of weekly rain in Lakewood, WA, from 2016 to 2025, ten rows of 53 weeks; summers are pale "
                       f"and the late-autumn weeks dark blue. Wettest year {yr.idxmax()}, driest {yr.idxmin()}."))
print(yr.round(1).to_dict())
