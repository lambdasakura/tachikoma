-- tachikoma up
CREATE TABLE User(
    id INTEGER PRIMARY KEY,
    name TEXT,
    password TEXT
    createdAt TEXT,
    updatedAt TEXT
);

CREATE TABLE Memo(
    id INTEGER PRIMARY KEY,
    author INTEGER,
    title TEXT,
    content TEXT,
    createdAt TEXT,
    updatedAt TEXT
);
-- tachikoma down
DROP TABLE User;
DROP TABLE Memo;
