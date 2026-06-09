from models import Current, DiveLocation, DiveSite, DiveSolution, Tide
from sites._common import exchange_description, safe_slack_triples

site = DiveSite(
    id="governor",
    name="SS Governor",
    tide_station_id="9447110",
    current_station_id="PCT1511",
    location=DiveLocation(address="Port Townsend, WA"),
)


def _keep(before: Current, slack: Current, after: Current) -> bool:
    if before.type != "flood" or slack.type != "slack":
        return False
    flood = abs(before.magnitude)
    ebb = abs(after.magnitude)
    return (flood < 1) and (ebb < 1) and (flood + ebb) <= 1.5


def find_solutions(_tides: list[Tide], currents: list[Current]) -> list[DiveSolution]:
    return [
        DiveSolution(site_id=site.id, time=slack.dt, description=exchange_description(b, slack, a))
        for b, slack, a in safe_slack_triples(currents, _keep)
    ]
