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
-- description: "make outer group unique";

ALTER TABLE groupmapping ADD UNIQUE (outergroup);

-- #!migration
-- name: "customer/groupmapping-for-datascopes",
-- description: "associations between datascopes and groups";

CREATE table groupsfordatascopes (
    id character varying(36) PRIMARY KEY DEFAULT public.uuid_generate_v4() NOT NULL,
    datascope_id character varying(36) NOT NULL REFERENCES datascopes(id),
    group_id character varying(36) NOT NULL  REFERENCES groupmapping(id),
    UNIQUE (datascope_id, group_id)
)