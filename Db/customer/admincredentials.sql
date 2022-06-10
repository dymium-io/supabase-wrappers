-- #!migration
-- name: "customer/admincredentials",
-- description: "Admin credentials",
-- requires: ["customer/connections"];

CREATE TABLE admincredentials (
    id varchar(36) PRIMARY KEY DEFAULT public.uuid_generate_v4(),
    connection_id varchar(36) NOT NULL REFERENCES connections(id),
    username varchar(128) NOT NULL
);
