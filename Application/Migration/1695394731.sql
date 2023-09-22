CREATE TABLE tasks (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    body TEXT NOT NULL,
    tasks_id UUID DEFAULT null,
    weight INT DEFAULT 0 NOT NULL
);
