-- tachikoma up
CREATE TABLE Note(
    id INTEGER PRIMARY KEY,
    name TEXT,
    createdAt TEXT,
    updatedAt TEXT
);

-- tachikoma down
DROP TABLE Note;
