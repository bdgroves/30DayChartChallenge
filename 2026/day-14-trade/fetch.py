"""US Census Bureau: Washington State exports by country (2025) and by product (HS2)."""
import sys; from pathlib import Path; sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
from fetching import *
import json
out = data_dir(__file__)
base = "https://api.census.gov/data/timeseries/intltrade/exports/statehs"
for name, q in {
    "by_country": "get=CTY_CODE,CTY_NAME,ALL_VAL_YR&STATE=WA&time=2025-12",
    "by_product": "get=E_COMMODITY,E_COMMODITY_SDESC,ALL_VAL_YR&STATE=WA&COMM_LVL=HS2&time=2025-12",
    "country_product": "get=CTY_CODE,CTY_NAME,E_COMMODITY,E_COMMODITY_SDESC,ALL_VAL_YR&STATE=WA&COMM_LVL=HS2&time=2025-12",
}.items():
    try:
        r = get(f"{base}?{q}", tries=2)
        save_text(out / f"{name}.json", r.text)
    except SystemExit as e:
        print("skip", name, e)
