from models import Current, DiveLocation, DiveSite, DiveSolution, Tide
from sites._common import exchange_description, safe_slack_triples

site = DiveSite(
    id="deceptionpass",
    name="Deception Pass",
    tide_station_id="9447110",
    current_station_id="PUG1701",
    location=DiveLocation(address="North Beach, Deception Pass State Park, Ault Field, WA"),
    rules=(
        "Slack before ebb — the peak before slack must be a flood.",
        "Flood peak < 6.0 kt.",
        "Ebb peak < 5.5 kt.",
        "Combined exchange ≤ 12.0 kt.",
        "Splash at slack.",
    ),
)


def _keep(before: Current, slack: Current, _after: Current) -> bool:
    if before.type != "flood" or slack.type != "slack":
        return False
    flood = abs(before.magnitude)
    ebb = abs(_after.magnitude)
    return (flood < 6) and (ebb < 5.5) and (flood + ebb) <= 12


def find_solutions(_tides: list[Tide], currents: list[Current]) -> list[DiveSolution]:
    return [
        DiveSolution(site_id=site.id, time=slack.dt, description=exchange_description(b, slack, a))
        for b, slack, a in safe_slack_triples(currents, _keep)
    ]
