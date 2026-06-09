from __future__ import annotations

import logging
from datetime import date, datetime

import httpx

from models import Current, Tide

log = logging.getLogger(__name__)

BASE = "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter"
APP = "divepredictor"
TIMEOUT = 15.0


def _begin_date(d: date) -> str:
    return f"{d.year:04d}{d.month:02d}{d.day:02d}"


def _parse_dt(s: str) -> datetime | None:
    # NOAA "t" / "Time" format: "YYYY-MM-DD HH:MM" (sometimes with seconds in other products)
    for fmt in ("%Y-%m-%d %H:%M", "%Y-%m-%d %H:%M:%S"):
        try:
            return datetime.strptime(s, fmt)
        except ValueError:
            continue
    return None


def _as_float(v) -> float | None:
    if v is None or v == "":
        return None
    try:
        return float(v)
    except (TypeError, ValueError):
        return None


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
    if not isinstance(data, dict):
        log.warning("NOAA tides: unexpected payload shape for station=%s date=%s", station_id, d)
        return []
    items = data.get("predictions") or []
    tides: list[Tide] = []
    for it in items:
        if not isinstance(it, dict):
            continue
        dt = _parse_dt(it.get("t", ""))
        mag = _as_float(it.get("v"))
        kind_raw = it.get("type")
        if dt is None or mag is None or kind_raw not in ("H", "L"):
            log.warning("NOAA tides: skipping malformed item %r", it)
            continue
        tides.append(Tide(
            station_id=station_id,
            dt=dt,
            type="HighTide" if kind_raw == "H" else "LowTide",
            magnitude=mag,
        ))
    return tides


def current_station_bin(station_id: str) -> str | None:
    """Return the primary bin number for a current station (used for direct-link URLs).

    NOAA's stations now publish multiple depth bins; the unsuffixed
    `predictions.html?id=<station>` URL no longer auto-resolves, so callers
    that want a deep link need `<station>_<bin>`. We learn the bin by asking
    NOAA for predictions today and looking at the first item's `Bin` field.
    Returns None if the lookup fails.
    """
    try:
        r = httpx.get(BASE, params={
            "format": "json", "application": APP, "time_zone": "lst_ldt",
            "range": "24", "product": "currents_predictions",
            "interval": "MAX_SLACK", "units": "english",
            "begin_date": _begin_date(date.today()), "station": station_id,
        }, timeout=TIMEOUT)
        r.raise_for_status()
        cp = (r.json() or {}).get("current_predictions") or {}
        items = (cp.get("cp") if isinstance(cp, dict) else None) or []
        for it in items:
            b = it.get("Bin") if isinstance(it, dict) else None
            if b not in (None, ""):
                return str(b)
    except Exception:
        log.warning("Could not determine bin for station %s", station_id)
    return None


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
    if not isinstance(data, dict):
        log.warning("NOAA currents: unexpected payload shape for station=%s date=%s", station_id, d)
        return []
    cp = data.get("current_predictions") or {}
    items = cp.get("cp") if isinstance(cp, dict) else None
    items = items or []
    currents: list[Current] = []
    for it in items:
        if not isinstance(it, dict):
            continue
        dt = _parse_dt(it.get("Time", ""))
        kind = it.get("Type")
        # For slack entries NOAA sometimes sends Velocity_Major="" — treat as 0.
        mag = _as_float(it.get("Velocity_Major"))
        if mag is None and kind == "slack":
            mag = 0.0
        if dt is None or kind not in ("flood", "ebb", "slack") or mag is None:
            log.warning("NOAA currents: skipping malformed item %r", it)
            continue
        currents.append(Current(
            station_id=station_id,
            dt=dt,
            type=kind,
            magnitude=mag,
        ))
    return currents
