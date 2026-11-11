import os
import re
import sys
import tempfile
from pathlib import Path

os.environ.setdefault("DIVEPREDICTOR_DB", os.path.join(tempfile.mkdtemp(), "t.sqlite3"))
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

import pytest
from fastapi.testclient import TestClient

import app as app_module


@pytest.fixture(scope="module")
def client():
    # Context manager runs the lifespan (DB schema init).
    with TestClient(app_module.app) as c:
        yield c


def test_healthz(client):
    r = client.get("/healthz")
    assert r.status_code == 200
    assert r.json() == {"ok": True}


def test_index_form_renders(client):
    # Regression: Starlette 1.x requires TemplateResponse(request, name, ctx).
    r = client.get("/")
    assert r.status_code == 200
    assert "thegeneral" in r.text


@pytest.mark.skipif(not os.environ.get("DIVEPREDICTOR_LIVE"), reason="needs NOAA network access")
def test_live_dive_windows(client):
    r = client.get(
        "/",
        params={"diveSite": "thegeneral", "startDate": "2026-11-12",
                "endDate": "2026-11-20", "numberOfSolutions": 5, "submitted": "true"},
    )
    assert r.status_code == 200
    assert re.search(r"\d+:\d\d [AP]M", r.text)
