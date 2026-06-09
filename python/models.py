from dataclasses import dataclass, field
from datetime import datetime


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
