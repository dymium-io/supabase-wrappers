-- #!migration
-- name: "customer/connections",
-- description: "Connections";

CREATE TABLE connections (
    id varchar(36) PRIMARY KEY DEFAULT public.uuid_generate_v4(),
    address varchar(36) NOT NULL,
    port integer NOT NULL,
    name varchar(128) NOT NULL,
    database_type varchar(128) NOT NULL,
    use_tls boolean NOT NULL,
    description varchar(256),
    dbname varchar(128)
);

-- #!migration
-- name: "customer/connections-source-type",
-- description: "Change source_type to ENUM",
-- requires: ["customer/connections"];

CREATE TYPE source_type AS ENUM (
    'postgres',
    'mysql',
    'mariadb',
    'sqlserver',
    'oracle'
);

ALTER TABLE connections ALTER COLUMN database_type TYPE source_type
      USING database_type::source_type;

-- #!migration
-- name: "customer/make-name-unique",
-- description: "make name unique",
-- requires: ["customer/connections-source-type"];
ALTER TABLE connections ADD CONSTRAINT name_unique UNIQUE (name);;