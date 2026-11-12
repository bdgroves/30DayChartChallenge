"""NSIDC Sea Ice Index: September Arctic extent, 1979 on."""
import sys; from pathlib import Path; sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
from fetching import *
out = data_dir(__file__)
for v in ("v4.0", "v3.0"):
    try:
        r = get(f"https://noaadata.apps.nsidc.org/NOAA/G02135/north/monthly/data/N_09_extent_{v}.csv", tries=2)
        save_text(out / "N_09_extent.csv", r.text); print("version", v); break
    except SystemExit as e:
        print(e)
