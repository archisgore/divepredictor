from __future__ import annotations

from collections.abc import Callable

from models import Current, DiveSite, DiveSolution, Tide

from sites import alkifishingreef, cove2, dayislandwall, deceptionpass, governor, keystone

SolutionFinder = Callable[[list[Tide], list[Current]], list[DiveSolution]]

_MODULES = [cove2, keystone, deceptionpass, dayislandwall, governor, alkifishingreef]

SITES: dict[str, DiveSite] = {m.site.id: m.site for m in _MODULES}
FINDERS: dict[str, SolutionFinder] = {m.site.id: m.find_solutions for m in _MODULES}


def list_sites() -> list[DiveSite]:
    return [m.site for m in _MODULES]


def site_by_id(site_id: str) -> DiveSite | None:
    return SITES.get(site_id)


def finder_for(site_id: str) -> SolutionFinder | None:
    return FINDERS.get(site_id)
