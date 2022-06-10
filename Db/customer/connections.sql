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
