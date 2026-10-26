from models import Current, DiveLocation, DiveSite, DiveSolution, Rule, Tide

site = DiveSite(
    id="cove2",
    name="Alki Seacrest Cove 2",
    tide_station_id="9447110",
    current_station_id="PUG1507",
    location=DiveLocation(address="1660 Harbor Ave SW Seattle, WA 98126"),
    rules=(
        Rule(
            "No current filter — every predicted current state is listed.",
            "Cove 2 sits inside Elliott Bay behind Duwamish Head, out of the "
            "main channel flow. There is no slack to wait for.",
        ),
        Rule(
            "Dive on whatever time suits you.",
            "Listed times are simply the NOAA current predictions for the range, "
            "shown so you know what the water is doing — not a constraint.",
        ),
    ),
)


def find_solutions(_tides: list[Tide], currents: list[Current]) -> list[DiveSolution]:
    return [
        DiveSolution(
            site_id=site.id,
            time=c.dt,
            description=f"{c.type} ({c.magnitude}) - Cove 2 is always divable",
        )
        for c in currents
    ]
