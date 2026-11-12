"""IHME GHDx: find the public Global Burden of Disease life-expectancy files and fetch them."""
import sys; from pathlib import Path; sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
from fetching import *
import re
out = data_dir(__file__)
pages = ["https://ghdx.healthdata.org/record/ihme-data/gbd-2021-life-tables-1950-2021",
         "https://ghdx.healthdata.org/record/ihme-data/global-life-expectancy-all-cause-mortality-and-cause-specific-mortality-forecasts-2022-2050",
         "https://ghdx.healthdata.org/record/ihme-data/gbd-2023-life-expectancy-hale-1950-2023",
         "https://ghdx.healthdata.org/gbd-2023"]
found = []
for p in pages:
    try:
        html = get(p, tries=2).text
    except SystemExit as e:
        print(e); continue
    links = sorted(set(re.findall(r'href="([^"]+\.(?:zip|csv|xlsx|CSV|ZIP|XLSX))"', html)))
    print(p, len(links)); found += [p + " -> " + l for l in links]
save_text(out / "ghdx_links.txt", "\n".join(found) or "none")
