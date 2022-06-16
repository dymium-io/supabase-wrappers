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