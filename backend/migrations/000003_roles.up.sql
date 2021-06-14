CREATE TABLE roles (
  id INTEGER PRIMARY KEY,
  label TEXT NOT NULL
);

INSERT INTO roles (id, label) VALUES 
  (0, "admin"),
  (1, "user"),
  (2, "board"),
  (3, "president"),
  (4, "passive");

