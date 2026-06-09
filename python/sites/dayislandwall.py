from datetime import timedelta

from models import Current, DiveLocation, DiveSite, DiveSolution, Tide
from sites._common import fmt, safe_slack_triples

site = DiveSite(
    id="dayislandwall",
    name="Day Island Wall",
    tide_station_id="9447110",
    current_station_id="PUG1528",
    location=DiveLocation(
        address="W Day Island Boulevard West and E Day Island Blvd W, Tacoma, Pierce, Washington 98466, United States"
    ),
)


def _keep(before: Current, slack: Current, after: Current) -> bool:
    if slack.type != "slack":
        return False
    first_mag = abs(before.magnitude)
    second_mag = abs(after.magnitude)
    return (first_mag < 3) and (second_mag < 3) and ((first_mag + second_mag) < 4)


def find_solutions(_tides: list[Tide], currents: list[Current]) -> list[DiveSolution]:
    out: list[DiveSolution] = []
    for b, slack, a in safe_slack_triples(currents, _keep):
        # Original Erlang subtracts 3600s ("correct_time") from the displayed slack time.
        corrected = slack.dt - timedelta(seconds=3600)
        desc = (
            f"{abs(b.magnitude) + abs(a.magnitude):.2f} Exchange across "
            f"{(a.dt - b.dt).total_seconds() / 60.0:.1f} minutes: "
            f"Slack at {fmt(slack.dt)} between "
            f"{b.type}({b.magnitude})@{fmt(b.dt)} - {a.type}({a.magnitude})@{fmt(a.dt)}"
        )
        out.append(DiveSolution(site_id=site.id, time=corrected, description=desc))
    return out
