-- #!migration
-- name: "customer/passwords",
-- description: "Passwords";

CREATE TABLE passwords (
    id varchar(36) PRIMARY KEY DEFAULT public.uuid_generate_v4(),
    password varchar(36) NOT NULL
);



-- #!migration
-- name: "customer/convert-passwords",
-- requires: ["customer/passwords"],
-- description: "convert password field to bytea for future encrytion";

ALTER TABLE passwords
    ALTER COLUMN password TYPE bytea USING password::bytea
;