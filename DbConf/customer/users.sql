-- #!migration
-- name: "customer/users",
-- description: "Database users";

CREATE TABLE users (
    id varchar(36) PRIMARY KEY DEFAULT public.uuid_generate_v4(),
    username varchar(128) NOT NULL UNIQUE,
    password varchar(128) NOT NULL,
    lastchanged timestamptz NOT NULL default CURRENT_TIMESTAMP
);


