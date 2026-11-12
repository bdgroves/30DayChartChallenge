"""Mono Basin Clearinghouse: Mono Lake's yearly surface elevation."""
import sys; from pathlib import Path; sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
from fetching import *
out = data_dir(__file__)
for u in ("https://www.monobasinresearch.org/data/levelyearly.php", "https://www.monobasinresearch.org/data/levelmonthly.php"):
    try:
        save_text(out / u.split("/")[-1].replace(".php", ".html"), get(u, tries=2).text)
    except SystemExit as e:
        print(e)
