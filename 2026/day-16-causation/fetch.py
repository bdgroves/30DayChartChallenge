"""NOAA Mauna Loa annual CO2 and NASA GISTEMP global annual temperature anomaly."""
import sys; from pathlib import Path; sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
from fetching import *
out = data_dir(__file__)
save_text(out / "co2_annmean_mlo.csv", get("https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_annmean_mlo.csv").text)
save_text(out / "gistemp_global.csv", get("https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv").text)
