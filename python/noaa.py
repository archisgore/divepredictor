from datetime import date, datetime
import httpx

from models import Tide, Current

BASE = "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter"
APP = "divepredictor"
TIMEOUT = 15.0


def _begin_date(d: date) -> str:
    return f"{d.year:04d}{d.month:02d}{d.day:02d}"


def _parse_dt(s: str) -> datetime:
    # NOAA "t" / "Time" format: "YYYY-MM-DD HH:MM"
    return datetime.strptime(s, "%Y-%m-%d %H:%M")


def fetch_tides(station_id: str, d: date) -> list[Tide]:
    params = {
        "format": "json",
        "application": APP,
        "time_zone": "lst_ldt",
        "range": "24",
        "product": "predictions",
        "interval": "hilo",
        "units": "english",
        "datum": "MLLW",
        "begin_date": _begin_date(d),
        "station": station_id,
    }
    r = httpx.get(BASE, params=params, timeout=TIMEOUT)
    r.raise_for_status()
    data = r.json()
    items = data.get("predictions", [])
    tides: list[Tide] = []
    for it in items:
        kind = "HighTide" if it["type"] == "H" else "LowTide"
        tides.append(Tide(
            station_id=station_id,
            dt=_parse_dt(it["t"]),
            type=kind,
            magnitude=float(it["v"]),
        ))
    return tides


def fetch_currents(station_id: str, d: date) -> list[Current]:
    params = {
        "format": "json",
        "application": APP,
        "time_zone": "lst_ldt",
        "range": "24",
        "product": "currents_predictions",
        "interval": "MAX_SLACK",
        "units": "english",
        "begin_date": _begin_date(d),
        "station": station_id,
    }
    r = httpx.get(BASE, params=params, timeout=TIMEOUT)
    r.raise_for_status()
    data = r.json()
    cp = data.get("current_predictions", {}) or {}
    items = cp.get("cp", []) or []
    currents: list[Current] = []
    for it in items:
        currents.append(Current(
            station_id=station_id,
            dt=_parse_dt(it["Time"]),
            type=it["Type"],
            magnitude=float(it["Velocity_Major"]),
        ))
    return currents
