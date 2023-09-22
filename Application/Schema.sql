-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TABLE tasks (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    body TEXT NOT NULL,
    task_id UUID DEFAULT NULL,
    weight INT DEFAULT 0 NOT NULL
);
