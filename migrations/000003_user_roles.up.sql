CREATE TABLE user_roles (
  id INTEGER PRIMARY KEY, 
  label TEXT NOT NULL
);

INSERT INTO user_roles (id, label) VALUES 
  (0, "admin"),
  (1, "user"),
  (2, "board"),
  (3, "president");

