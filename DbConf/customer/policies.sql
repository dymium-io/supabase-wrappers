-- #!migration
-- name: "customer/policies",
-- description: "Policies for Access Levels, and Ghost Databases",
-- requires: [];

CREATE TABLE policies (
    id varchar(36) PRIMARY KEY DEFAULT public.uuid_generate_v4(),
    name varchar(128) NOT NULL,
    policy jsonb NOT NULL default '{}'::jsonb
);

-- #!migration
-- name: "customer/connectors-enforce-single-row",
-- requires: ["customer/policies"],
-- description: "enforce single row";
ALTER TABLE policies ALTER id SET DEFAULT '0000-0000-0000';

ALTER TABLE policies ADD CONSTRAINT constr_id_unique UNIQUE(id);

ALTER TABLE policies ADD CONSTRAINT constr_id_value CHECK(id='0000-0000-0000');

ALTER TABLE policies DROP column name;


-- #!migration
-- name: "customer/connectors-enforce-unique-name",
-- requires: ["customer/connectors-enforce-single-row"],
-- description: "enforce unique name";
CREATE UNIQUE INDEX policies_unique_name ON policies( (policy->>'name') ) ;