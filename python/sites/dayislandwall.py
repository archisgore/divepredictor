from datetime import timedelta

from models import Current, DiveLocation, DiveSite, DiveSolution, Tide
from sites._common import fmt, safe_slack_triples

# Day Island Wall sits in the side passage west of the Tacoma Narrows.
# The closest NOAA current station, PUG1528, is "The Narrows, South end
# (midstream)" — in the main channel, around the corner from the wall.
# In Puget Sound's geometry, slack in a side passage typically leads or
# lags the main channel by 30-90 min; empirically, slack at the wall
# occurs about an hour earlier than PUG1528's prediction. We subtract
# that here so displayed times reflect the wall, not the midstream
# Narrows station.
LOCAL_SLACK_OFFSET = timedelta(hours=-1)

site = DiveSite(
    id="dayislandwall",
    name="Day Island Wall",
    tide_station_id="9447110",
    current_station_id="PUG1528",
    location=DiveLocation(
        address="W Day Island Boulevard West and E Day Island Blvd W, Tacoma, Pierce, Washington 98466, United States"
    ),
    notes=(
        "Spatial slack offset: the nearest NOAA current station, PUG1528 "
        "(\"The Narrows, South end, midstream\"), sits in the main channel "
        "around the corner from the wall. Empirically, slack at Day Island "
        "Wall happens ~1 h before the NOAA prediction at the Narrows. "
        "Listed times already apply that correction — dive these as the "
        "slack itself."
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
        local_slack = slack.dt + LOCAL_SLACK_OFFSET
        exch = abs(b.magnitude) + abs(a.magnitude)
        minutes = (a.dt - b.dt).total_seconds() / 60.0
        desc = (
            f"Estimated slack at the wall (NOAA Narrows prediction "
            f"{fmt(slack.dt)} − 1 h). "
            f"{exch:.2f} kt exchange across {minutes:.0f} min: "
            f"{b.type}({b.magnitude})@{fmt(b.dt)} → {a.type}({a.magnitude})@{fmt(a.dt)}."
        )
        out.append(DiveSolution(site_id=site.id, time=local_slack, description=desc))
    return out
