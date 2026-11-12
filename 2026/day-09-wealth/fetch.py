"""Zillow Home Value Index by county: Washington's 39 counties, every month."""
import sys; from pathlib import Path; sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
from fetching import *
import pandas as pd
out = data_dir(__file__)
url = "https://files.zillowstatic.com/research/public_csvs/zhvi/County_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"
df = pd.read_csv(io.StringIO(get(url).text))
wa = df[df["State"] == "WA"]
wa.to_csv(out / "zhvi_wa_counties.csv", index=False)
print(len(wa), "counties; months", wa.columns[9], "to", wa.columns[-1])
