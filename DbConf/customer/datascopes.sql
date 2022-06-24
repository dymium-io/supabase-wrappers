-- #!migration
-- name: "customer/datascopes",
-- description: "Datascopes";

CREATE TABLE datascopes (
    id varchar(36) PRIMARY KEY DEFAULT public.uuid_generate_v4(),
    name varchar(128) NOT NULL
);
