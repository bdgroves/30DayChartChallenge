"""NASA Exoplanet Archive: planets with a measured mass and radius, with uncertainties."""
import sys; from pathlib import Path; sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
from fetching import *
from urllib.parse import quote
out = data_dir(__file__)
q = ("select pl_name,hostname,discoverymethod,disc_year,pl_bmasse,pl_bmasseerr1,pl_bmasseerr2,pl_bmassprov,"
     "pl_rade,pl_radeerr1,pl_radeerr2,pl_orbper,pl_eqt from pscomppars where pl_bmasse is not null and pl_rade is not null")
save_text(out / "exoplanets.csv", get("https://exoplanetarchive.ipac.caltech.edu/TAP/sync?format=csv&query=" + quote(q)).text)
