from __future__ import annotations

import os
from contextlib import asynccontextmanager
from datetime import date, datetime, timedelta
from pathlib import Path
from urllib.parse import quote_plus

from fastapi import FastAPI, Request
from fastapi.responses import HTMLResponse, RedirectResponse
from fastapi.staticfiles import StaticFiles
from fastapi.templating import Jinja2Templates

import cache
import computer
import sites
from models import DiveSite

BASE = Path(__file__).parent
GOOGLE_MAPS_KEY = os.environ.get("GOOGLE_MAPS_KEY", "")
CANONICAL_HOST = os.environ.get("CANONICAL_HOST", "")
REDIRECT_HOSTS = {
    h.strip().lower()
    for h in os.environ.get("REDIRECT_HOSTS", "").split(",")
    if h.strip()
}


@asynccontextmanager
async def lifespan(_app: FastAPI):
    cache.init_db()
    yield


app = FastAPI(title="DivePredictor", lifespan=lifespan)
app.mount("/static", StaticFiles(directory=BASE / "static"), name="static")
templates = Jinja2Templates(directory=str(BASE / "templates"))


@app.middleware("http")
async def canonical_host_redirect(request: Request, call_next):
    host = (request.headers.get("host") or "").split(":", 1)[0].lower()
    if CANONICAL_HOST and host in REDIRECT_HOSTS:
        target = f"https://{CANONICAL_HOST}{request.url.path}"
        if request.url.query:
            target += f"?{request.url.query}"
        return RedirectResponse(target, status_code=301)
    return await call_next(request)


@app.get("/healthz")
def healthz():
    return {"ok": True}


def _google_map_uri(site: DiveSite) -> str:
    loc = site.location
    if loc.latitude is not None and loc.longitude is not None:
        return (
            f"https://www.google.com/maps/embed/v1/view?key={GOOGLE_MAPS_KEY}"
            f"&zoom=14&center={loc.latitude},{loc.longitude}"
        )
    return (
        f"https://www.google.com/maps/embed/v1/place?key={GOOGLE_MAPS_KEY}"
        f"&zoom=14&q={quote_plus(loc.address)}"
    )


def _parse_date(s: str | None) -> date | None:
    if not s:
        return None
    try:
        return datetime.strptime(s, "%Y-%m-%d").date()
    except ValueError:
        return None


def _format_result(site: DiveSite, sol) -> dict:
    return {
        "site_name": site.name,
        "day": sol.time.strftime("%a, %b %d %Y"),
        "time": sol.time.strftime("%I:%M %p").lstrip("0"),
        "iso": sol.time.isoformat(),
        "description": sol.description,
    }


@app.get("/", response_class=HTMLResponse)
def index(
    request: Request,
    diveSite: str | None = None,
    startDate: str | None = None,
    endDate: str | None = None,
    numberOfSolutions: int | None = None,
    submitted: bool = False,
):
    all_sites = sites.list_sites()
    default_id = diveSite or all_sites[0].id
    start = _parse_date(startDate) or date.today()
    end = _parse_date(endDate) or (start + timedelta(days=60))
    count = numberOfSolutions or 3
    show_results = submitted or any([diveSite, startDate, endDate, numberOfSolutions])

    site = sites.site_by_id(default_id) or all_sites[0]
    results: list[dict] = []
    if show_results:
        for s in computer.solve(site.id, start, count, end_date=end):
            site_for_row = sites.site_by_id(s.site_id) or site
            results.append(_format_result(site_for_row, s))

    return templates.TemplateResponse(
        "index.html",
        {
            "request": request,
            "sites": all_sites,
            "selected_site": site,
            "start_date": start.isoformat(),
            "end_date": end.isoformat(),
            "number_of_solutions": count,
            "results": results,
            "show_results": show_results,
            "map_uri": _google_map_uri(site),
            "maps_key_present": bool(GOOGLE_MAPS_KEY),
        },
    )
