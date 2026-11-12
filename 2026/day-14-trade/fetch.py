"""US Census Bureau state export pages for Washington: top countries and top products."""
import sys; from pathlib import Path; sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
from fetching import *

import re
out = data_dir(__file__)
for u in ("https://www.census.gov/foreign-trade/statistics/state/data/wa.html",
          "https://www.census.gov/foreign-trade/statistics/state/data/imports/wa.html"):
    try:
        save_text(out / ("imports_wa.html" if "imports" in u else "wa.html"), get(u, tries=2).text)
    except SystemExit as e:
        print(e)
html = (out / "wa.html").read_text()
links = sorted(set(re.findall(r'href="([^"]+)"', html)))
save_text(out / "wa_links.txt", "\n".join(links))
for l in links:
    if re.search(r"(wa|state).*\.(html|xls|xlsx|csv|txt)$", l, re.I) and "data" in l:
        url = l if l.startswith("http") else "https://www.census.gov" + (l if l.startswith("/") else "/foreign-trade/statistics/state/data/" + l)
        try:
            save_text(out / ("x_" + url.split("/")[-1].replace(".html", ".htm")), get(url, tries=1).text)
        except SystemExit as e:
            print(e)
