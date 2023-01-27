-- #!migration
-- name: "global/customers",
-- description: "Customer's definition";

CREATE TABLE global.customers (
    id varchar(36) PRIMARY KEY DEFAULT public.uuid_generate_v4(),
    company_name character varying(128) NOT NULL,
    schema_name character varying(36) NOT NULL,
    organization character varying(64) NOT NULL,
    domain character varying(128) NOT NULL
);

-- #!migration
-- name: "global/defgroup",
-- description: "Add default admin group",
-- requires: ["global/customers"];

ALTER TABLE global.customers ADD COLUMN admin_group character varying(128) DEFAULT ''; 