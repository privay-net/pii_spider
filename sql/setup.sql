CREATE USER robert WITH PASSWORD 'bhujasample4$';
CREATE ROLE dbowner nologin;
GRANT dbowner TO robert, postgres;
CREATE DATABASE pii;
ALTER database pii OWNER TO dbowner;


\connect pii;

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public
   GRANT ALL ON TABLES TO robert;
ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public
   GRANT ALL ON SEQUENCES TO robert;
ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public
   GRANT ALL ON FUNCTIONS TO robert;

CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    email text,
    phone text,
    name text
);

INSERT INTO users (email, phone, name) VALUES ('robert@test.com', '0412345678', 'Robert')
