"""Whitman men's soccer, 2025 season (including the NCAA tournament), from the school's cumulative stats page.
Same parsing as brooksgroves.com's fetch-whitman workflow; one request."""
import sys; from pathlib import Path; sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
from fetching import *
import csv, re
from datetime import datetime
from bs4 import BeautifulSoup

out = data_dir(__file__)
html = get("https://whitmanblues.com/sports/mens-soccer/stats/2025").text
soup = BeautifulSoup(html, "html.parser")
cells = lambda tr: [c.get_text(" ", strip=True) for c in tr.find_all(["td", "th"])]
rows = []
for table in soup.find_all("table"):
    head = " ".join(cells(table.find("tr"))) if table.find("tr") else ""
    if not ("W/L" in head and "Score" in head):
        continue
    for tr in table.find_all("tr")[1:]:
        c = cells(tr)
        if len(c) < 4 or not re.match(r"\d{1,2}/\d{1,2}/\d{4}", c[0]):
            continue
        opp = c[1]
        loc = "away" if opp.lower().startswith("at ") else ("neutral" if opp.lower().startswith("vs") else "home")
        opp = re.sub(r"^(at|vs\.?)\s+", "", opp, flags=re.I).strip()
        wl = c[2].strip().upper()[:1]
        sc = re.findall(r"\d+", c[3])
        rows.append([datetime.strptime(c[0], "%m/%d/%Y").date().isoformat(), opp, loc, wl, sc[0], sc[1], c[3]])
    break
with open(out / "whitman_2025.csv", "w", newline="") as f:
    w = csv.writer(f); w.writerow(["date", "opponent", "loc", "wl", "gf", "ga", "score_text"]); w.writerows(rows)
print(len(rows), "games"); [print(r) for r in rows]
