"""NRCS report generator: every Washington SNOTEL station, elevation and April 1 snow water (2026 and the 1991-2020 median)."""
import sys; from pathlib import Path; sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
from fetching import *
out = data_dir(__file__)
url = ("https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultipleStationReport/daily/start_of_period/"
       "state=%22WA%22%20AND%20network=%22SNTL%22%20AND%20element=%22WTEQ%22%7Cname/2026-04-01,2026-04-01/"
       "stationId,name,elevation,latitude,longitude,WTEQ::value,WTEQ::median_1991")
save_text(out / "snotel_wa_apr1.csv", get(url).text)
