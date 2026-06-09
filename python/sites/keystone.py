from models import Current, DiveLocation, DiveSite, DiveSolution, Tide
from sites._common import exchange_description, safe_slack_triples

site = DiveSite(
    id="keystone",
    name="Keystone Jetty",
    tide_station_id="9447905",
    current_station_id="PUG1624",
    location=DiveLocation(address="12981 WA-20, Coupeville, WA 98239"),
)


def _keep(before: Current, slack: Current, after: Current) -> bool:
    if slack.type != "slack":
        return False
    exchange = abs(before.magnitude) + abs(after.magnitude)
    # Erlang divided by 360.0; preserves original behavior exactly.
    time = (after.dt - before.dt).total_seconds() / 360.0
    if time == 0:
        return False
    return (exchange <= 6) and ((exchange / time) <= 0.55)


def find_solutions(_tides: list[Tide], currents: list[Current]) -> list[DiveSolution]:
    return [
        DiveSolution(site_id=site.id, time=slack.dt, description=exchange_description(b, slack, a))
        for b, slack, a in safe_slack_triples(currents, _keep)
    ]
