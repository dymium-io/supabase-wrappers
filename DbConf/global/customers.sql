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

-- #!migration
-- name: "global/unique",
-- description: "Add restrictions",
-- requires: ["global/defgroup"];
ALTER TABLE global.customers ADD CONSTRAINT unique_customer_id UNIQUE (id);
ALTER TABLE global.customers ADD CONSTRAINT unique_customer_name UNIQUE (company_name);
ALTER TABLE global.customers ADD CONSTRAINT unique_customer_schema UNIQUE (schema_name);
ALTER TABLE global.customers ADD CONSTRAINT unique_customer_org UNIQUE (organization);