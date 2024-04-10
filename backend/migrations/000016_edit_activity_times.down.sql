-- Drop column date and re-add it but nullable
ALTER TABLE activity_times DROP COLUMN date;
ALTER TABLE activity_times ADD COLUMN date TEXT not NULL CHECK (date <> '');
