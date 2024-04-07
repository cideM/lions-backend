-- Drop column location and re-add it but nullable
ALTER TABLE activities DROP COLUMN location;
ALTER TABLE activities ADD COLUMN location TEXT;

-- Drop column date and re-add it but nullable
ALTER TABLE activities DROP COLUMN date;
ALTER TABLE activities ADD COLUMN date TEXT;

