CREATE TABLE activities (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    description TEXT,
    location TEXT NOT NULL CHECK (location <> ''),
    -- v--- ISO8601
    date TEXT not NULL CHECK (date <> '')
);
