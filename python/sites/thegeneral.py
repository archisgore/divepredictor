from datetime import timedelta

from models import Current, DiveLocation, DiveSite, DiveSolution, Tide
from sites._common import fmt, safe_slack_triples

# The General is deeper and further offshore than the Fishing Reef, so the
# descent and the swim to the wreck both take longer — be in the water a full
# hour before slack.
SPLASH_LEAD = timedelta(hours=1)

DAYLIGHT_START_HOUR = 9
DAYLIGHT_END_HOUR = 16

site = DiveSite(
    id="thegeneral",
    name="The General",
    tide_station_id="9447130",
    current_station_id="PUG1520",
    location=DiveLocation(
        address="Boat dive — East Passage, ~1 mi NE of Dolphin Point, Vashon Island, WA",
        latitude=47.505211,
        longitude=-122.392421,
    ),
    notes=(
        "Boat-access wreck in roughly 130–170 ft of water in East Passage. "
        "Currents come from PUG1520 (\"Dolphin Point, 1.3 miles East of\"), "
        "about 1.5 miles away in the same channel."
    ),
    splash_offset=SPLASH_LEAD,
    rules=(
        "Slack before ebb — the peak before slack must be a flood, so the "
        "current is at your back both ways.",
        f"Slack between {DAYLIGHT_START_HOUR:02d}:00 and {DAYLIGHT_END_HOUR:02d}:00 local.",
        "Splash 60 min before slack — the listed time is the splash time.",
    ),
)


def _keep(before: Current, slack: Current, _after: Current) -> bool:
    if before.type != "flood" or slack.type != "slack":
        return False
    return DAYLIGHT_START_HOUR <= slack.dt.hour <= DAYLIGHT_END_HOUR


def find_solutions(_tides: list[Tide], currents: list[Current]) -> list[DiveSolution]:
    out: list[DiveSolution] = []
    for b, slack, a in safe_slack_triples(currents, _keep):
        exch = abs(b.magnitude) + abs(a.magnitude)
        out.append(DiveSolution(
            site_id=site.id,
            time=slack.dt,
            description=(
                f"Slack at {fmt(slack.dt)} across a {exch:.2f} kt exchange: "
                f"{b.type}({b.magnitude})@{fmt(b.dt)} → {a.type}({a.magnitude})@{fmt(a.dt)}."
            ),
        ))
    return out
