from datetime import timedelta

from models import Current, DiveLocation, DiveSite, DiveSolution, Rule, Tide
from sites._common import fmt, safe_slack_triples

# Be in the water this long before the predicted slack.
SPLASH_LEAD = timedelta(minutes=30)

DAYLIGHT_START_HOUR = 9
DAYLIGHT_END_HOUR = 16

site = DiveSite(
    id="alkifishingreef",
    name="Alki Fishing Reef",
    tide_station_id="9447130",
    current_station_id="PUG1516",
    location=DiveLocation(address="4503 Beach Dr SW Seattle, WA 98116 United States"),
    splash_offset=SPLASH_LEAD,
    rules=(
        Rule(
            "Slack before ebb — the peak before slack must be a flood.",
            "You want the current at your back both ways: ride the tail of the "
            "flood out to the reef, turn at slack, and ride the building ebb "
            "back in. Slack before flood pushes you the wrong way on the swim home.",
        ),
        Rule(
            f"Slack falls between {DAYLIGHT_START_HOUR:02d}:00 and "
            f"{DAYLIGHT_END_HOUR:02d}:00 local time.",
            "Keeps the whole dive, including the surface swim, in daylight.",
        ),
        Rule(
            "Splash 30 minutes before slack.",
            "The listed time is the splash time; slack itself is 30 minutes later. "
            "Entering early lets the last of the flood carry you out to the reef.",
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
