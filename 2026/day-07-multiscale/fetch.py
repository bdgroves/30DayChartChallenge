"""USGS ComCat: every earthquake in a box around Washington since 1970, any magnitude."""
import sys; from pathlib import Path; sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
from fetching import *
import pandas as pd
out = data_dir(__file__)
frames = []
def chunk(a, b):
    url = ("https://earthquake.usgs.gov/fdsnws/event/1/query?format=csv&orderby=time-asc"
           f"&starttime={a}&endtime={b}&minlatitude=45.5&maxlatitude=49.05&minlongitude=-124.9&maxlongitude=-116.9")
    n = int(get(url.replace("/query?format=csv", "/count?format=text")).text.strip())
    if n >= 19000:
        mid = (pd.Timestamp(a) + (pd.Timestamp(b) - pd.Timestamp(a)) / 2).strftime("%Y-%m-%d")
        chunk(a, mid); chunk(mid, b); return
    if n:
        df = pd.read_csv(io.StringIO(get(url).text), usecols=["time", "latitude", "longitude", "depth", "mag", "magType", "place", "type"])
        frames.append(df)
    print(a, b, n)
for y in range(1970, 2027):
    chunk(f"{y}-01-01", f"{y+1}-01-01")
df = pd.concat(frames).drop_duplicates()
df = df[df["type"] == "earthquake"].drop(columns="type")
df.to_csv(out / "quakes.csv.gz", index=False, compression="gzip")
print(len(df), "quakes")
