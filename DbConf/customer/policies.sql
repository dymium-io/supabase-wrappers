-- #!migration
-- name: "customer/policies",
-- description: "Policies for Access Levels, and Ghost Databases",
-- requires: [];

CREATE TABLE policies (
    id varchar(36) PRIMARY KEY DEFAULT public.uuid_generate_v4(),
    name varchar(128) NOT NULL,
    policy jsonb NOT NULL default '{}'::jsonb
);
