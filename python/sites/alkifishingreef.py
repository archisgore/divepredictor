from models import Current, DiveLocation, DiveSite, DiveSolution, Tide
from sites._common import safe_slack_triples

site = DiveSite(
    id="alkifishingreef",
    name="Alki Fishing Reef",
    tide_station_id="9447130",
    current_station_id="PUG1516",
    location=DiveLocation(address="4503 Beach Dr SW Seattle, WA 98116 United States"),
)


def _keep(before: Current, slack: Current, _after: Current) -> bool:
    if before.type != "flood" or slack.type != "slack":
        return False
    return 9 <= slack.dt.hour <= 16


def find_solutions(_tides: list[Tide], currents: list[Current]) -> list[DiveSolution]:
    out: list[DiveSolution] = []
    for b, slack, a in safe_slack_triples(currents, _keep):
        exch = abs(b.magnitude) + abs(a.magnitude)
        out.append(DiveSolution(
            site_id=site.id,
            time=slack.dt,
            description=f"Slack between a {exch:.2f} Exchange",
        ))
    return out
