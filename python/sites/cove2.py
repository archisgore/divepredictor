from models import Current, DiveLocation, DiveSite, DiveSolution, Tide

site = DiveSite(
    id="cove2",
    name="Alki Seacrest Cove 2",
    tide_station_id="9447110",
    current_station_id="PUG1507",
    location=DiveLocation(address="1660 Harbor Ave SW Seattle, WA 98126"),
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
