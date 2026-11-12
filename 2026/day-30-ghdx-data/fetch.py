"""IHME GHDx: GBD 2021 demographics record. Finds the life-expectancy files, downloads them and keeps
life expectancy at birth, both sexes, every location, with the 95% uncertainty interval."""
import sys; from pathlib import Path; sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
from fetching import *

import re, zipfile
import pandas as pd
out = data_dir(__file__)
rec = "https://ghdx.healthdata.org/record/ihme-data/global-burden-disease-study-2021-gbd-2021-demographics-1950-2021"
html = get(rec).text
links = sorted(set(re.findall(r'href="([^"]+)"', html)))
files = [l for l in links if re.search(r"\.(zip|csv|xlsx)$", l, re.I)]
save_text(out / "ghdx_links.txt", "\n".join(links))
print(len(files), "data files:"); print("\n".join(files))
le = [f for f in files if re.search(r"LIFE|_LE_|EXPECT", f, re.I)]
keep = []
for f in le:
    url = f if f.startswith("http") else "https://ghdx.healthdata.org" + f
    r = get(url, tries=2)
    p = out / url.split("/")[-1]
    p.write_bytes(r.content); print("got", p.name, len(r.content))
    if p.suffix.lower() == ".zip":
        with zipfile.ZipFile(p) as z:
            for n in z.namelist():
                print("  ", n)
                if n.lower().endswith(".csv"):
                    df = pd.read_csv(z.open(n), low_memory=False)
                    print("   cols", list(df.columns)[:20])
                    keep.append(df)
        p.unlink()
if keep:
    df = pd.concat(keep)
    df.to_csv(out / "life_expectancy_raw_head.csv", index=False) if len(df) < 200000 else None
    m = df
    for col, val in (("sex_name", "both"), ("age_name", "<1 year"), ("age_group_name", "<1 year")):
        if col in m.columns and val in set(m[col].astype(str)):
            m = m[m[col].astype(str) == val]
    m.to_csv(out / "life_expectancy.csv.gz", index=False, compression="gzip")
    print(len(m), "rows kept")
