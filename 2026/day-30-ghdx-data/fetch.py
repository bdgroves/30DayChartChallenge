"""UN World Population Prospects 2024 (catalogued in IHME's GHDx): probabilistic projections of life
expectancy at birth, both sexes, with the 80% and 95% prediction intervals. IHME's own GBD files need a login."""
import sys; from pathlib import Path; sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
from fetching import *

import pandas as pd
out = data_dir(__file__)
base = "https://population.un.org/wpp/assets/Excel%20Files/2_Probabilistic%20Projections/"
for name in ("UN_PPP2024_Output_e0B.xlsx",):
    r = get(base + name)
    p = out / name; p.write_bytes(r.content); print(name, len(r.content))
    x = pd.ExcelFile(p); print(x.sheet_names)
    frames = []
    for sh in x.sheet_names:
        df = pd.read_excel(p, sheet_name=sh, header=None)
        hdr_row = df.index[df.apply(lambda r: r.astype(str).str.contains("ISO3 Alpha-code").any(), axis=1)]
        if len(hdr_row) == 0:
            continue
        h = hdr_row[0]
        df = pd.read_excel(p, sheet_name=sh, header=h)
        df["sheet"] = sh
        frames.append(df)
        print(sh, df.shape, list(df.columns)[:14])
    big = pd.concat(frames)
    big.to_csv(out / "wpp2024_e0_both_probabilistic.csv.gz", index=False, compression="gzip")
    p.unlink()
