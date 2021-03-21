CREATE TABLE sessions (
  id TEXT PRIMARY KEY NOT NULL check(length(id) <= 24),
  expires TEXT,
  userid INTEGER,
  FOREIGN KEY(userid) REFERENCES users(id)
);

CREATE UNIQUE INDEX only_one_session_per_user
ON sessions (userid)


