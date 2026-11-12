"""Open-Meteo (ERA5) daily weather for Lakewood, WA, 1950-2025. Days 22, 26 and 29 use it too."""
import sys; from pathlib import Path; sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
from fetching import *
out = data_dir(__file__)
df = lakewood_daily()
df.to_csv(out / "lakewood_daily.csv", index=False)
print(len(df), "days", df["time"].min(), df["time"].max())
