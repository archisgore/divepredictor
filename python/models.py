from dataclasses import dataclass, field
from datetime import datetime, timedelta


@dataclass(frozen=True)
class DiveLocation:
    address: str = ""
    latitude: float | None = None
    longitude: float | None = None


@dataclass(frozen=True)
class Tide:
    station_id: str
    dt: datetime
    type: str  # "HighTide" | "LowTide"
    magnitude: float


@dataclass(frozen=True)
class Current:
    station_id: str
    dt: datetime
    type: str  # "flood" | "ebb" | "slack"
    magnitude: float


@dataclass(frozen=True)
class DiveSolution:
    site_id: str
    time: datetime
    description: str
    length: int = -1


@dataclass(frozen=True)
class DiveSite:
    id: str
    name: str
    tide_station_id: str
    current_station_id: str
    location: DiveLocation = field(default_factory=DiveLocation)
    # Human notes explaining the site's diving technique / why the rule is shaped
    # the way it is. Surfaced in the UI sidebar; empty string means no notes.
    notes: str = ""
    # Every condition find_solutions() tests, in the order it tests them, one
    # short line each. Must stay in lockstep with the site's _keep() predicate.
    rules: tuple[str, ...] = ()
    # How long before the computed slack you should be in the water. The UI
    # subtracts this from the slack to show a splash time. Zero means "the
    # listed slack is the time to be in the water".
    splash_offset: timedelta = timedelta(0)

