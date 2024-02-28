-- #!migration
-- name: "customer/superadmins",
-- description: "Super admins";


CREATE TABLE superadmins (
    id varchar(36) PRIMARY KEY DEFAULT public.uuid_generate_v4() NOT NULL,
    username varchar(128) NOT NULL UNIQUE
);

-- #!migration
-- name: "customer/superadmins_addhash",
-- requires: ["customer/superadmins"],
-- description: "Super admins - add username hash and change username to encrypted";
ALTER TABLE superadmins ADD COLUMN username_hash varchar(64) NOT NULL;
ALTER TABLE superadmins ALTER COLUMN username TYPE bytea USING username::bytea;
