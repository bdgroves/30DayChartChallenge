"""NRCS SNOTEL Paradise (679), Mount Rainier: daily snow depth and snow water since 1980."""
import sys; from pathlib import Path; sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
from fetching import *
out = data_dir(__file__)
url = ("https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customSingleStationReport/daily/start_of_period/"
       "679:WA:SNTL%7Cid=%22%22%7Cname/1980-10-01,2026-09-30/SNWD::value,WTEQ::value")
save_text(out / "paradise_daily.csv", get(url).text)
