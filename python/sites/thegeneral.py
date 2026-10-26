from datetime import timedelta

from models import Current, DiveLocation, DiveSite, DiveSolution, Rule, Tide
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
        "Deep boat-access wreck sitting in roughly 130–170 ft of water in East "
        "Passage. The nearest NOAA current station is PUG1520 (\"Dolphin Point, "
        "1.3 miles East of\"), about 1.5 miles away in the same channel. Because "
        "the dive is deep and the wreck takes a few minutes to find, the splash "
        "lead is an hour rather than the Fishing Reef's half hour."
    ),
    splash_offset=SPLASH_LEAD,
    rules=(
        Rule(
            "Slack before ebb — the peak before slack must be a flood.",
            "Same reasoning as the Fishing Reef: the last of the flood carries you "
            "out and down to the wreck, and the building ebb carries you back.",
        ),
        Rule(
            f"Slack falls between {DAYLIGHT_START_HOUR:02d}:00 and "
            f"{DAYLIGHT_END_HOUR:02d}:00 local time.",
            "A 130–170 ft dive wants every bit of ambient light, and the boat ride "
            "out and back should be in daylight too.",
        ),
        Rule(
            "Splash 60 minutes before slack.",
            "The listed time is the splash time; slack itself is an hour later. "
            "The extra half hour over the Fishing Reef covers the longer descent "
            "and the swim to the wreck.",
        ),
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
                f"{b.type}({b.magnitude})@{fmt(b.dt)} → {a.type}({a.magnitude})@{fmt(a.dt)}. "
                f"Flood before, ebb after — current at your back both ways."
            ),
        ))
    return out
