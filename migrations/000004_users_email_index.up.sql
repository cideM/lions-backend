CREATE UNIQUE INDEX users_unique_lower_email_idx
ON users (lower(email))
