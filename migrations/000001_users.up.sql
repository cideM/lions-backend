CREATE TABLE users
(
  id INTEGER PRIMARY KEY,
  email TEXT NOT NULL,
  password_digest TEXT NOT NULL,
  first_name TEXT,
  last_name TEXT,
  address TEXT,
  mobile_phone_nr TEXT,
  landline_nr TEXT,
  birthday TEXT,
  first_name_partner TEXT,
  last_name_partner TEXT,
  birthday_partner TEXT
)

