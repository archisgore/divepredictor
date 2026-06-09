from __future__ import annotations

import logging
from dataclasses import dataclass
from datetime import date, timedelta

import cache
import sites
from models import DiveSolution

log = logging.getLogger(__name__)

MAX_DAYS = 60


@dataclass(frozen=True)
class SolveResult:
    solutions: list[DiveSolution]
    error: str | None = None  # populated if NOAA fetch failed; UI renders separately


def solve(
    site_id: str,
    start_date: date,
    desired_count: int,
    end_date: date | None = None,
) -> SolveResult:
    site = sites.site_by_id(site_id)
    finder = sites.finder_for(site_id)
    if site is None or finder is None:
        return SolveResult(solutions=[], error=f"Unknown dive site '{site_id}'.")

    hard_end = start_date + timedelta(days=MAX_DAYS - 1)
    effective_end = min(end_date, hard_end) if end_date else hard_end

    solutions: list[DiveSolution] = []
    current_date = start_date
    while current_date <= effective_end:
        try:
            tides = cache.get_tides(site.tide_station_id, current_date)
            currents = cache.get_currents(site.current_station_id, current_date)
        except Exception as exc:
            log.exception("Fetch failed for site=%s date=%s", site_id, current_date)
            return SolveResult(
                solutions=solutions,
                error=(
                    f"Could not fetch NOAA data for {current_date.isoformat()} "
                    f"(station {site.tide_station_id}/{site.current_station_id}): {exc}"
                ),
            )

        if not tides and not currents:
            return SolveResult(
                solutions=solutions,
                error=(
                    f"NOAA returned no tides or currents on {current_date.isoformat()} "
                    f"for stations {site.tide_station_id}/{site.current_station_id}."
                ),
            )

        solutions.extend(finder(tides, currents))
        if len(solutions) >= desired_count:
            return SolveResult(solutions=solutions)
        current_date = current_date + timedelta(days=1)

    return SolveResult(solutions=solutions)
