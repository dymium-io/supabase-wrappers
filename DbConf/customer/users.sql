-- #!migration
-- name: "customer/users",
-- description: "Database users";

CREATE TABLE users (
    id varchar(36) PRIMARY KEY DEFAULT public.uuid_generate_v4(),
    username varchar(128) NOT NULL UNIQUE,
    password varchar(128) NOT NULL,
    lastchanged timestamptz NOT NULL default CURRENT_TIMESTAMP
);


-- #!migration
-- name: "customer/users-change-password",
-- requires: ["customer/users"],
-- description: "Add passwordb field to hold encrypted password";
ALTER TABLE users ADD COLUMN passwordb bytea NOT NULL DEFAULT '\x';

-- #!migration
-- name: "customer/users-remove-old-password",
-- requires: ["customer/users-change-password"],
-- description: "Remove legacy password field ";
ALTER TABLE users DROP COLUMN password;