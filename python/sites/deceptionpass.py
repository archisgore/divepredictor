from models import Current, DiveLocation, DiveSite, DiveSolution, Rule, Tide
from sites._common import exchange_description, safe_slack_triples

site = DiveSite(
    id="deceptionpass",
    name="Deception Pass",
    tide_station_id="9447110",
    current_station_id="PUG1701",
    location=DiveLocation(address="North Beach, Deception Pass State Park, Ault Field, WA"),
    rules=(
        Rule(
            "Slack before ebb — the peak before slack must be a flood.",
            "Deception Pass is a narrow, high-flow channel. Slack before ebb puts "
            "the building current on the side of the pass you want to be swept toward.",
        ),
        Rule(
            "Flood peak before slack < 6.0 kt.",
            "Caps how much water is still moving as the slack approaches.",
        ),
        Rule(
            "Ebb peak after slack < 5.5 kt.",
            "Slightly tighter than the flood limit — the ebb is what builds under "
            "you while you are still in the water.",
        ),
        Rule(
            "Combined exchange ≤ 12.0 kt.",
            "Both peaks can pass individually and still add up to a slack that is "
            "over almost as soon as it starts.",
        ),
        Rule(
            "Splash at slack.",
            "No lead time applied — the listed time is the predicted slack itself.",
        ),
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
