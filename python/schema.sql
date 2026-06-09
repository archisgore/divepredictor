CREATE TABLE IF NOT EXISTS tides (
  id   TEXT NOT NULL,
  day  TEXT NOT NULL,
  time TEXT NOT NULL,
  type TEXT NOT NULL,
  magnitude REAL NOT NULL,
  PRIMARY KEY (id, day, time)
);

CREATE TABLE IF NOT EXISTS currents (
  id   TEXT NOT NULL,
  day  TEXT NOT NULL,
  time TEXT NOT NULL,
  type TEXT NOT NULL,
  magnitude REAL NOT NULL,
  PRIMARY KEY (id, day, time)
);
