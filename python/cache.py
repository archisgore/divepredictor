from __future__ import annotations

import os
import sqlite3
import threading
from datetime import date, datetime
from pathlib import Path

from models import Current, Tide
import noaa

DB_PATH = os.environ.get("DIVEPREDICTOR_DB", "divepredictor.sqlite3")
SCHEMA = Path(__file__).parent / "schema.sql"

# Per-key locks so concurrent requests for the same (table, station, date)
# don't both fire a NOAA fetch — the second waiter sees the cache filled by
# the first. Bounded by the universe of (station, date) pairs we ever serve.
_lock_table_lock = threading.Lock()
_locks: dict[tuple[str, str, str], threading.Lock] = {}


def _connect() -> sqlite3.Connection:
    conn = sqlite3.connect(DB_PATH, timeout=10.0)
    conn.row_factory = sqlite3.Row
    conn.execute("PRAGMA journal_mode=WAL")
    conn.execute("PRAGMA synchronous=NORMAL")
    conn.execute("PRAGMA busy_timeout=5000")
    return conn


def init_db() -> None:
    parent = Path(DB_PATH).parent
    if str(parent) not in ("", ".") and not parent.exists():
        raise RuntimeError(
            f"DIVEPREDICTOR_DB parent dir {parent} does not exist. "
            "On Fly, this usually means the /data volume is not mounted."
        )
    with _connect() as conn:
        conn.executescript(SCHEMA.read_text())


def _day_key(d: date) -> str:
    return f"{d.year:04d}-{d.month:02d}-{d.day:02d}"


def _time_key(dt: datetime) -> str:
    return f"{dt.hour:02d}:{dt.minute:02d}:{dt.second:02d}"


def _key_lock(table: str, station_id: str, day: str) -> threading.Lock:
    key = (table, station_id, day)
    with _lock_table_lock:
        lock = _locks.get(key)
        if lock is None:
            lock = threading.Lock()
            _locks[key] = lock
        return lock


def get_tides(station_id: str, d: date) -> list[Tide]:
    day = _day_key(d)
    cached = _select_tides(station_id, day)
    if cached:
        return cached

    with _key_lock("tides", station_id, day):
        # double-check after acquiring the lock — someone else may have filled it
        cached = _select_tides(station_id, day)
        if cached:
            return cached

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
    cached = _select_currents(station_id, day)
    if cached:
        return cached

    with _key_lock("currents", station_id, day):
        cached = _select_currents(station_id, day)
        if cached:
            return cached

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


def _select_tides(station_id: str, day: str) -> list[Tide]:
    with _connect() as conn:
        rows = conn.execute(
            "SELECT day, time, type, magnitude FROM tides WHERE id = ? AND day = ? ORDER BY time",
            (station_id, day),
        ).fetchall()
    return [_row_to_tide(station_id, r) for r in rows]


def _select_currents(station_id: str, day: str) -> list[Current]:
    with _connect() as conn:
        rows = conn.execute(
            "SELECT day, time, type, magnitude FROM currents WHERE id = ? AND day = ? ORDER BY time",
            (station_id, day),
        ).fetchall()
    return [_row_to_current(station_id, r) for r in rows]


def _row_to_tide(station_id: str, r: sqlite3.Row) -> Tide:
    dt = datetime.strptime(f"{r['day']} {r['time']}", "%Y-%m-%d %H:%M:%S")
    return Tide(station_id=station_id, dt=dt, type=r["type"], magnitude=r["magnitude"])


def _row_to_current(station_id: str, r: sqlite3.Row) -> Current:
    dt = datetime.strptime(f"{r['day']} {r['time']}", "%Y-%m-%d %H:%M:%S")
    return Current(station_id=station_id, dt=dt, type=r["type"], magnitude=r["magnitude"])
