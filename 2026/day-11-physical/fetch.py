"""Copernicus GLO-30 elevation: histograms for boxes around the Ruby Mountains, Yosemite's Sierra and Mount Rainier."""
import sys; from pathlib import Path; sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
from fetching import *
import numpy as np, pandas as pd, rasterio
from rasterio.windows import from_bounds
out = data_dir(__file__)
BOXES = {  # name: (west, south, east, north), tile(s)
    "Ruby Mountains": (-115.62, 40.30, -115.22, 40.82),
    "Yosemite high country": (-119.75, 37.65, -119.25, 37.98),
    "Mount Rainier": (-121.95, 46.72, -121.55, 47.00),
}
def tile(lat, lon):
    ns, ew = ("N", "W")
    n = f"Copernicus_DSM_COG_10_{ns}{int(np.floor(lat)):02d}_00_{ew}{int(-np.floor(lon)):03d}_00_DEM"
    return f"https://copernicus-dem-30m.s3.amazonaws.com/{n}/{n}.tif"
rows = []
for name, (w_, s, e, n) in BOXES.items():
    url = tile(s, w_)
    with rasterio.open(url) as src:
        z = src.read(1, window=from_bounds(w_, s, e, n, src.transform)).astype(float)
    z = z[z > -100]
    hist, edges = np.histogram(z, bins=np.arange(0, 4500, 50))
    for c, lo in zip(hist, edges[:-1]):
        rows.append({"range": name, "elev_m": lo, "cells": int(c)})
    print(name, z.size, z.min(), z.max(), url.split("/")[-1])
pd.DataFrame(rows).to_csv(out / "hypsometry.csv", index=False)
