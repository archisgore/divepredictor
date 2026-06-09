from __future__ import annotations

from collections.abc import Callable, Iterable
from typing import TypeAlias

from models import Current

Triple: TypeAlias = tuple[Current, Current, Current]


def sliding_triples(currents: Iterable[Current]) -> Iterable[Triple]:
    items = list(currents)
    for i in range(len(items) - 2):
        yield items[i], items[i + 1], items[i + 2]


def safe_slack_triples(
    currents: Iterable[Current],
    keep: Callable[[Current, Current, Current], bool],
) -> list[Triple]:
    return [t for t in sliding_triples(currents) if keep(*t)]


def fmt(dt) -> str:
    return dt.strftime("%Y-%m-%d %H:%M:%S")


def exchange_description(before: Current, slack: Current, after: Current) -> str:
    exch = abs(before.magnitude) + abs(after.magnitude)
    minutes = (after.dt - before.dt).total_seconds() / 60.0
    return (
        f"{exch:.2f} Exchange across {minutes:.1f} minutes: "
        f"Slack between {before.type}({before.magnitude})@{fmt(before.dt)} - "
        f"{after.type}({after.magnitude})@{fmt(after.dt)}"
    )
