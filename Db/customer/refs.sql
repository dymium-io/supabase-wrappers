-- #!migration
-- name: "customer/refs",
-- description: "Table to keep foreign keys descriptions";

CREATE TABLE refs (
    id varchar(36) PRIMARY KEY DEFAULT public.uuid_generate_v4(),
    schem varchar(128) NOT NULL,
    tabl varchar(128) NOT NULL,
    col varchar(128) NOT NULL
);
