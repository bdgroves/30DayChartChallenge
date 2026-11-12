"""UNICEF under-five mortality (all countries, 1990 on) and World Bank GDP per person (PPP)."""
import sys; from pathlib import Path; sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
from fetching import *
import json
out = data_dir(__file__)
u = "https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,CME,1.0/.CME_MRY0T4._T?format=csv&startPeriod=1990"
save_text(out / "unicef_u5mr.csv", get(u).text)
wb = get("https://api.worldbank.org/v2/country/all/indicator/NY.GDP.PCAP.PP.KD?format=json&per_page=20000&date=1990:2024").json()
rows = [f'{r["countryiso3code"]},{r["date"]},{r["value"]}' for r in wb[1] if r["value"] is not None and r["countryiso3code"]]
save_text(out / "wb_gdppc_ppp.csv", "iso3,year,gdppc\n" + "\n".join(rows))
meta = get("https://api.worldbank.org/v2/country?format=json&per_page=400").json()
rows = [f'{c["id"]},"{c["name"]}",{c["region"]["value"]},{c["incomeLevel"]["value"]}' for c in meta[1]]
save_text(out / "wb_countries.csv", "iso3,name,region,income\n" + "\n".join(rows))
