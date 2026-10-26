from models import Current, DiveLocation, DiveSite, DiveSolution, Rule, Tide
from sites._common import exchange_description, safe_slack_triples

site = DiveSite(
    id="keystone",
    name="Keystone Jetty",
    tide_station_id="9447905",
    current_station_id="PUG1624",
    location=DiveLocation(address="12981 WA-20, Coupeville, WA 98239"),
    rules=(
        Rule(
            "Any slack qualifies — slack before ebb and slack before flood both count.",
            "The jetty is protected enough on both sides that direction of the "
            "following current is not the deciding factor; size of the exchange is.",
        ),
        Rule(
            "Total exchange ≤ 6.0 kt (flood peak + ebb peak, magnitudes added).",
            "Admiralty Inlet moves a lot of water. A big exchange means a short, "
            "sharp slack that closes before you are back at the jetty.",
        ),
        Rule(
            "Swing rate ≤ 0.55 kt per 6 minutes between the two surrounding peaks.",
            "Rules out a modest exchange that happens unusually fast, which gives "
            "the same narrow slack window as a large one.",
        ),
        Rule(
            "Splash at slack.",
            "No lead time applied — the listed time is the predicted slack itself.",
        ),
    ),
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
