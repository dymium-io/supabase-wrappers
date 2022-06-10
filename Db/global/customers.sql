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
