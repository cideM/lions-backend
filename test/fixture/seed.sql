-- Deterministic test data for the black box tests. Loaded into a freshly
-- migrated database by build.sh. Passwords (bcrypt, cost 10):
--   admin@example.com      admin-password
--   member@example.com     member-password
--   board@example.com      other-password
--   delete-me@example.com  delete-password
--   firebase@example.com   user1password   (Firebase scrypt, test vector from
--                                           backend/tests/Scrypt/Test.hs; needs
--                                           the matching LIONS_SCRYPT_* keys)
PRAGMA foreign_keys = ON;

INSERT INTO users (id, email, password_digest, salt, first_name, last_name, address, mobile_phone_nr, landline_nr, birthday, first_name_partner, last_name_partner, birthday_partner) VALUES
  (1, 'admin@example.com',     '$2y$10$7gU7Y34FAC571K3WY1cfruElz.rhV2ihtNagr1gOMYdNRxG69fgaC', NULL, 'Alice', 'Admin',     'Hauptstraße 1, 77855 Achern', '0171-1111111', '07841-11111', '01.01.1970', 'Albert', 'Admin', '02.02.'),
  (2, 'member@example.com',    '$2y$10$LXBMuo.QFl5s3qng85wbCOWo3kxp7UeRgkqBj2HxpW8HiZZXenHYW', NULL, 'Bob',   'Member',    'Nebenstraße 2, 77855 Achern', '0171-2222222', '',            '03.03.1980', '',       '',      ''),
  (3, 'firebase@example.com',  'lSrfV15cpx95/sZS2W9c9Kp6i/LVgQNDNC/qzrCnh1SAyZvqmZqAjTdn3aoItz+VHjoZilo78198JAdRuid5lQ==', '42xEC+ixf3L2lw==', 'Frida', 'Firebase', NULL, NULL, NULL, NULL, NULL, NULL, NULL),
  (4, 'board@example.com',     '$2y$10$7DkHFwFGPX.AnXePxjQM7eBb/6S38cdS4pnjn7i8.7YyEayLpRBFi', NULL, 'Carol', 'Board',     'Ringstraße 4, 77855 Achern',  '0171-4444444', '',            '04.04.1975', '',       '',      ''),
  (5, 'delete-me@example.com', '$2y$10$IKgY.c4QDyMf.rWttLzxH.66ke1aYcNE1joHTt37MZGW/ikxdUJbS', NULL, 'Dave',  'Deletable', '',                            '',             '',            '',           '',       '',      '');

-- roles: 0 admin, 1 user, 2 board, 3 president, 4 passive
INSERT INTO user_roles (userid, roleid) VALUES
  (1, 0), (1, 1),
  (2, 1),
  (3, 1),
  (4, 1), (4, 2),
  (5, 1), (5, 4);

INSERT INTO welcome_text (id, content, date) VALUES
  (1, 'Willkommen im **Mitgliederbereich**. Mehr unter https://lions-achern.de', '2025-01-10 10:00:00'),
  (2, 'Protokoll der letzten Sitzung im Anhang.', '2025-02-20 18:30:00');

INSERT INTO feed_attachments (id, postid, content, filename) VALUES
  (1, 2, CAST('Protokoll Inhalt' AS BLOB), 'protokoll.txt');

INSERT INTO events (id, title, date, family_allowed, description, location) VALUES
  (1, 'Sommerfest',       '2027-07-15 18:00:00', 1, 'Ein Fest für alle Mitglieder und Familien', 'Vereinsheim Achern'),
  (2, 'Vorstandssitzung', '2020-03-01 19:30:00', 0, 'Nur Vorstand',                               'Rathaus');

INSERT INTO event_attachments (id, eventid, filename, content) VALUES
  (1, 1, 'flyer.txt', CAST('Flyer Inhalt' AS BLOB));

INSERT INTO event_replies (userid, eventid, coming, guests) VALUES
  (2, 1, 1, 2),
  (4, 1, 0, 0);

INSERT INTO activities (id, name, description, location, date) VALUES
  (1, 'Weihnachtsmarkt', 'Glühweinstand', 'Marktplatz Achern', '2024-12-07 15:00:00'),
  (2, 'Blutspende',      NULL,            NULL,                NULL);

INSERT INTO activity_times (id, activity_id, user_id, hours, minutes, date) VALUES
  (1, 1, 2, 2, 30, '2024-12-07 00:00:00'),
  (2, 1, 4, 1, 0,  NULL),
  (3, 2, 2, 0, 45, NULL);
