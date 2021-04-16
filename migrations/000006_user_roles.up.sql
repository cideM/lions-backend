CREATE TABLE user_roles (
  userid INTEGER NOT NULL,
  roleid INTEGER NOT NULL,
  FOREIGN KEY (userid) REFERENCES users(id) ON DELETE CASCADE,
  FOREIGN KEY (roleid) REFERENCES roles(id) ON DELETE CASCADE,
  UNIQUE (userid,roleid)
)
