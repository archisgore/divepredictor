#!/bin/sh
set -e

# The Fly-mounted volume at /data is owned by whoever wrote to it first
# (root, on early deploys). Re-chown it so the unprivileged app user can
# read+write the SQLite cache. Cheap; idempotent.
chown -R app:app /data 2>/dev/null || true

exec gosu app "$@"
