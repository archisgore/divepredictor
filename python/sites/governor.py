from models import Current, DiveLocation, DiveSite, DiveSolution, Rule, Tide
from sites._common import exchange_description, safe_slack_triples

site = DiveSite(
    id="governor",
    name="SS Governor",
    tide_station_id="9447110",
    current_station_id="PCT1511",
    location=DiveLocation(address="Port Townsend, WA"),
    rules=(
        Rule(
            "Slack before ebb — the peak before slack must be a flood.",
            "Drop onto the wreck on the tail of the flood and leave on the ebb.",
        ),
        Rule(
            "Flood peak before slack < 1.0 kt.",
            "Far and away the strictest limits of any site here. The Governor is a "
            "deep wreck off Point Wilson; a current you could swim against on a "
            "shore dive is not one you want at depth with a decompression obligation.",
        ),
        Rule(
            "Ebb peak after slack < 1.0 kt.",
            "Same reasoning, applied to the current building during your ascent.",
        ),
        Rule(
            "Combined exchange ≤ 1.5 kt.",
            "Rules out days where both peaks sneak in just under 1.0 kt.",
        ),
        Rule(
            "Splash at slack.",
            "No lead time applied — the listed time is the predicted slack itself.",
        ),
    ),
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
