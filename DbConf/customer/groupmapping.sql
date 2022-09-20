-- #!migration
-- name: "customer/groupmapping",
-- description: "Group mapping";

CREATE TABLE groupmapping (
    id varchar(36) PRIMARY KEY DEFAULT public.uuid_generate_v4(),
    outergroup varchar(128) NOT NULL,
    innergroup varchar(128) NOT NULL,
    comment varchar
);

-- #!migration
-- name: "customer/groupmapping-unique",
-- requires: ["customer/groupmapping"],
-- description: "make outer group unique";

ALTER TABLE groupmapping ADD UNIQUE (outergroup);

-- #!migration
-- name: "customer/groupmapping-adminaccess",
-- requires: ["customer/groupmapping", "customer/groupmapping-unique"],
-- description: "mark group as granting admin access";

ALTER TABLE groupmapping ADD column adminaccess boolean NOT NULL default false;