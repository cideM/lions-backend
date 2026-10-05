-- One valid and one expired reset token for the Firebase user (id 3).
-- The token values are also in password.vars.
insert into reset_tokens (token, expires, userid) values
  ('valid-token-abcdefghijklmnopqrstuvwxyz',   '2099-01-01 00:00:00', 3),
  ('expired-token-abcdefghijklmnopqrstuvwxyz', '2000-01-01 00:00:00', 3);
