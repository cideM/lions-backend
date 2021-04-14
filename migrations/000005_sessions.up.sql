CREATE TABLE sessions (
  id INTEGER PRIMARY KEY,
  key TEXT NOT NULL check(length(key) >= 24),
  expires TEXT,
  userid INTEGER,
  FOREIGN KEY(userid) REFERENCES users(id)
);

CREATE UNIQUE INDEX sessions_by_key ON sessions (key);
