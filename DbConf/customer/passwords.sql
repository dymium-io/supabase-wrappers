-- #!migration
-- name: "customer/passwords",
-- description: "Passwords";

CREATE TABLE passwords (
    id varchar(36) PRIMARY KEY DEFAULT public.uuid_generate_v4(),
    password varchar(36) NOT NULL
);
