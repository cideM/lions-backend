ALTER TABLE activities DROP COLUMN location;
ALTER TABLE activities ADD COLUMN TEXT NOT NULL CHECK (location <> '');

ALTER TABLE activities DROP COLUMN date;
ALTER TABLE activities ADD COLUMN date TEXT not NULL CHECK (date <> '');
