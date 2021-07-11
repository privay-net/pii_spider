CREATE USER robert WITH PASSWORD 'bhujasample4$';
CREATE ROLE dbowner nologin;
GRANT dbowner TO robert, postgres
CREATE DATABASE pii;
ALTER database pii OWNER TO dbowner;


\connect pii;

CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    email text,
    phone text,
    name text
);

INSERT INTO users (email, phone, name) VALUES ('robert@test.com', '0412345678', 'Robert')
