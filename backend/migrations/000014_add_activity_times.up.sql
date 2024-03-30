CREATE TABLE activity_times (
    id INTEGER PRIMARY KEY,
    activity_id INTEGER NOT NULL,
    user_id INTEGER NOT NULL,
    hours INTEGER NOT NULL,
    minutes INTEGER NOT NULL,
    date TEXT NOT NULL CHECK (date <> ''),
    FOREIGN KEY (user_id) REFERENCES users(id),
    FOREIGN KEY (activity_id) REFERENCES activities(id)
);
