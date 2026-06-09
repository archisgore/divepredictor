from __future__ import annotations

import os
import sqlite3
from datetime import date, datetime
from pathlib import Path

from models import Current, Tide
import noaa

DB_PATH = os.environ.get("DIVEPREDICTOR_DB", "divepredictor.sqlite3")
SCHEMA = Path(__file__).parent / "schema.sql"


def _connect() -> sqlite3.Connection:
    conn = sqlite3.connect(DB_PATH)
    conn.row_factory = sqlite3.Row
    return conn


def init_db() -> None:
    with _connect() as conn:
        conn.executescript(SCHEMA.read_text())


def _day_key(d: date) -> str:
    return f"{d.year:04d}-{d.month:02d}-{d.day:02d}"


def _time_key(dt: datetime) -> str:
    return f"{dt.hour:02d}:{dt.minute:02d}:{dt.second:02d}"


def get_tides(station_id: str, d: date) -> list[Tide]:
    day = _day_key(d)
    with _connect() as conn:
        rows = conn.execute(
            "SELECT day, time, type, magnitude FROM tides WHERE id = ? AND day = ? ORDER BY time",
            (station_id, day),
        ).fetchall()
    if rows:
        return [_row_to_tide(station_id, r) for r in rows]

    tides = noaa.fetch_tides(station_id, d)
    if not tides:
        return []
    with _connect() as conn:
        conn.executemany(
            "INSERT OR IGNORE INTO tides (id, day, time, type, magnitude) VALUES (?, ?, ?, ?, ?)",
            [(t.station_id, _day_key(t.dt.date()), _time_key(t.dt), t.type, t.magnitude) for t in tides],
        )
        conn.commit()
    return tides


def get_currents(station_id: str, d: date) -> list[Current]:
    day = _day_key(d)
    with _connect() as conn:
        rows = conn.execute(
            "SELECT day, time, type, magnitude FROM currents WHERE id = ? AND day = ? ORDER BY time",
            (station_id, day),
        ).fetchall()
    if rows:
        return [_row_to_current(station_id, r) for r in rows]

    currents = noaa.fetch_currents(station_id, d)
    if not currents:
        return []
    with _connect() as conn:
        conn.executemany(
            "INSERT OR IGNORE INTO currents (id, day, time, type, magnitude) VALUES (?, ?, ?, ?, ?)",
            [(c.station_id, _day_key(c.dt.date()), _time_key(c.dt), c.type, c.magnitude) for c in currents],
        )
        conn.commit()
    return currents


def _row_to_tide(station_id: str, r: sqlite3.Row) -> Tide:
    dt = datetime.strptime(f"{r['day']} {r['time']}", "%Y-%m-%d %H:%M:%S")
    return Tide(station_id=station_id, dt=dt, type=r["type"], magnitude=r["magnitude"])


def _row_to_current(station_id: str, r: sqlite3.Row) -> Current:
    dt = datetime.strptime(f"{r['day']} {r['time']}", "%Y-%m-%d %H:%M:%S")
    return Current(station_id=station_id, dt=dt, type=r["type"], magnitude=r["magnitude"])
