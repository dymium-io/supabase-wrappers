-- #!migration
-- name: "customer/access",
-- description: "Access definition",
-- requires: ["customer/datascopes"];

CREATE TABLE access (
    datascope_id varchar(36) NOT NULL REFERENCES datascopes(id),
    group_id varchar(36) NOT NULL
);
