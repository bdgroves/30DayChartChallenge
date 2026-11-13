"""RSF World Press Freedom Index: the 2026 scores (and 2025, for the change)."""
import sys; from pathlib import Path; sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
from fetching import *
out = data_dir(__file__)
for year in (2025, 2024):
    r = get(f"https://rsf.org/sites/default/files/import_classement/{year}.csv")
    save_text(out / f"rsf_{year}.csv", r.content.decode("utf-8-sig", errors="replace"))
