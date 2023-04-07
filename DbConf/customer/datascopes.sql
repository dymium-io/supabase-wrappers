-- #!migration
-- name: "customer/datascopes",
-- description: "Datascopes";

CREATE TABLE datascopes (
    id varchar(36) PRIMARY KEY DEFAULT public.uuid_generate_v4(),
    name varchar(128) NOT NULL
);

-- #!migration
-- name: "customer/datascopes-timestamps",
-- description: "add timestamps to datascopes",
-- requires: ["customer/datascopes"];

ALTER TABLE datascopes ADD COLUMN created timestamp NOT NULL  DEFAULT now();
ALTER TABLE datascopes ADD COLUMN modified timestamp NOT NULL  DEFAULT now();

-- #!migration
-- name: "customer/datascopes-timestamps-tz",
-- description: "add timestamps to datascopes",
-- requires: ["customer/datascopes-timestamps"];

ALTER TABLE datascopes ALTER created TYPE timestamptz ;
ALTER TABLE datascopes ALTER modified TYPE timestamptz ;

