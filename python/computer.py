from __future__ import annotations

import logging
from datetime import date, datetime, timedelta, timezone

import cache
import sites
from models import DiveSolution

log = logging.getLogger(__name__)

MAX_DAYS = 60


def solve(
    site_id: str,
    start_date: date,
    desired_count: int,
    end_date: date | None = None,
) -> list[DiveSolution]:
    site = sites.site_by_id(site_id)
    finder = sites.finder_for(site_id)
    if site is None or finder is None:
        return []

    hard_end = start_date + timedelta(days=MAX_DAYS - 1)
    effective_end = min(end_date, hard_end) if end_date else hard_end

    solutions: list[DiveSolution] = []
    current_date = start_date
    while current_date <= effective_end:
        try:
            tides = cache.get_tides(site.tide_station_id, current_date)
            currents = cache.get_currents(site.current_station_id, current_date)
        except Exception:
            log.exception("Fetch failed for site=%s date=%s", site_id, current_date)
            return solutions + [DiveSolution(
                site_id=site.id,
                time=datetime.now(timezone.utc),
                description=f"Could not fetch NOAA data for {current_date}.",
            )]

        if not tides and not currents:
            solutions.append(DiveSolution(
                site_id=site.id,
                time=datetime.now(timezone.utc),
                description=(
                    f"INTERNAL: No tides or currents from stations "
                    f"{site.tide_station_id}/{site.current_station_id} on {current_date}."
                ),
            ))
            return solutions

        solutions.extend(finder(tides, currents))
        if len(solutions) >= desired_count:
            return solutions
        current_date = current_date + timedelta(days=1)

    return solutions
