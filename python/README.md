# DivePredictor (Python port)

Python rewrite of the original Erlang DivePredictor. Predicts safe slack-current windows for 6 Pacific Northwest scuba sites by pulling NOAA tide + current predictions and applying per-site rules.

## Stack

- **FastAPI + Jinja2** — server-rendered HTML
- **SQLite** — persistent cache of NOAA predictions (`/data/divepredictor.sqlite3` in production)
- **httpx** — NOAA API client

## Run locally

```bash
cd python
python3 -m venv .venv && source .venv/bin/activate
pip install -r requirements.txt
uvicorn app:app --reload --port 8000
```

Open http://localhost:8000.

## Environment

| Var | Default | Purpose |
| --- | --- | --- |
| `DIVEPREDICTOR_DB` | `divepredictor.sqlite3` | SQLite cache path |
| `GOOGLE_MAPS_KEY` | (empty) | Google Maps Embed key; map hidden if unset |
| `PORT` | `8080` (Docker) | Listen port |

## Deploy to Fly.io

```bash
cd python
fly launch --no-deploy            # accept defaults; uses fly.toml here
fly volumes create divepredictor_data --size 1 --region sea
fly secrets set GOOGLE_MAPS_KEY=...
fly deploy
```

## Layout

```
python/
  app.py            # FastAPI app + routes
  computer.py       # Bounded 60-day solver
  cache.py          # SQLite get-or-fetch
  noaa.py           # NOAA datagetter client
  models.py         # dataclasses (Tide, Current, DiveSite, DiveSolution)
  sites/            # one module per dive site; each exports `site` + `find_solutions`
  templates/        # Jinja2
  static/           # CSS
  schema.sql        # SQLite DDL
  Dockerfile
  fly.toml
```
