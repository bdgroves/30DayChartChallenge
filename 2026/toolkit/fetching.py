"""Small helpers for the day fetch.py scripts (run on GitHub Actions, which can reach the web)."""
from __future__ import annotations

import io
import time
from pathlib import Path

import requests

UA = {"User-Agent": "30DayChartChallenge/2026 (github.com/bdgroves/30DayChartChallenge)"}
LAKEWOOD = (47.1718, -122.5185)          # Lakewood, Washington: the city center


def get(url: str, tries: int = 4, **kw) -> requests.Response:
    for i in range(tries):
        try:
            r = requests.get(url, headers=UA, timeout=120, **kw)
            if r.status_code == 200:
                return r
            print(f"  {r.status_code} {url[:120]}")
        except requests.RequestException as e:
            print(f"  {type(e).__name__} {url[:120]}")
        time.sleep(5 * (i + 1))
    raise SystemExit(f"could not fetch {url}")


def data_dir(file: str) -> Path:
    d = Path(file).resolve().parent / "data"
    d.mkdir(exist_ok=True)
    return d


def save_text(path: Path, text: str) -> None:
    path.write_text(text, encoding="utf-8")
    print(f"wrote {path.name}: {len(text):,} chars, {text.count(chr(10)):,} lines")


def lakewood_daily(start="1950-01-01", end="2025-12-31") -> "pandas.DataFrame":
    import pandas as pd
    lat, lon = LAKEWOOD
    url = ("https://archive-api.open-meteo.com/v1/archive"
           f"?latitude={lat}&longitude={lon}&start_date={start}&end_date={end}"
           "&daily=temperature_2m_max,temperature_2m_min,temperature_2m_mean,precipitation_sum"
           "&timezone=America%2FLos_Angeles&temperature_unit=fahrenheit&precipitation_unit=inch")
    j = get(url).json()
    return pd.DataFrame(j["daily"])
