-- #!migration
-- name: "customer/groupmapping-for-datascopes",
-- requires: ["customer/datascopes", "customer/groupmapping-unique"],
-- description: "associations between datascopes and groups";

CREATE table groupsfordatascopes (
    id character varying(36) PRIMARY KEY DEFAULT public.uuid_generate_v4() NOT NULL,
    datascope_id character varying(36) NOT NULL REFERENCES datascopes(id),
    group_id character varying(36) NOT NULL  REFERENCES groupmapping(id),
    UNIQUE (datascope_id, group_id)
)
