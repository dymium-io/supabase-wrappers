-- #!migration
-- name: "customer/tables",
-- description: "Descriotion of foreign tables",
-- requires: ["customer/datascopes", "customer/connections", "customer/refs"];

CREATE TABLE tables (
    id varchar(36) PRIMARY KEY DEFAULT public.uuid_generate_v4(),
    datascope_id varchar(36) NOT NULL REFERENCES datascopes(id),
    connection_id varchar(36) NOT NULL REFERENCES connections(id),
    schem varchar(128) NOT NULL,
    tabl varchar(128) NOT NULL,
    action varchar(32) NOT NULL,
    col varchar(128) NOT NULL,
    typ varchar(32) NOT NULL,
    "position" integer,
    reference varchar(36) REFERENCES refs(id),
    semantics varchar(32) NOT NULL,
    UNIQUE (schem, tabl, col)
);
