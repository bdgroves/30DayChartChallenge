"""UN Comtrade (public preview API, no key): United States goods exports and imports with every partner, 2024."""
import sys; from pathlib import Path; sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
from fetching import *

import json
out = data_dir(__file__)
for flow in ("X", "M"):
    url = ("https://comtradeapi.un.org/public/v1/preview/C/A/HS?reporterCode=842&period=2024"
           f"&flowCode={flow}&cmdCode=TOTAL&customsCode=C00&motCode=0&partner2Code=0")
    save_text(out / f"us_2024_{flow}.json", get(url).text)
save_text(out / "partners.json", get("https://comtradeapi.un.org/files/v1/app/reference/partnerAreas.json").text)
