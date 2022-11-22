-- #!migration
-- name: "customer/conectors",
-- requires: [],
-- description: "create a table with connectors";

CREATE table connectors (
    id character varying(36) PRIMARY KEY DEFAULT public.uuid_generate_v4() NOT NULL,
    name character varying(36) NOT NULL,
    targetaddress character varying(36) NOT NULL,
    targetport numeric NOT NULL,   
    localport numeric not NULL UNIQUE,
    status character varying(36) NOT NULL DEFAULT 'inactive'
)

-- #!migration
-- name: "customer/connectors-add-connectorname",
-- requires: ["customer/conectors"],
-- description: "add connectorname";
ALTER TABLE connectors ADD COLUMN connectorname varchar(36) NOT NULL

-- #!migration
-- name: "customer/connectors-add-auth",
-- requires: ["customer/conectors", "customer/connectors-add-connectorname"],
-- description: "add connectorname";
CREATE table connectorauth (
    id    varchar(36) PRIMARY KEY DEFAULT public.uuid_generate_v4() NOT NULL,
    name  varchar(36) NOT NULL,
    accesskey   varchar(36) NOT NULL,
    accesssecret  varchar(128) NOT NULL,
    createdat TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    status  varchar(36) NOT NULL DEFAULT 'inactive'    
);
ALTER TABLE connectors DROP COLUMN name;
ALTER TABLE connectors add COLUMN id_connectorauth varchar(36);


-- #!migration
-- name: "customer/connectors-add-foreignkey",
-- requires: ["customer/conectors", "customer/connectors-add-connectorname", "customer/connectors-add-auth"],
-- description: "add foreign key";
ALTER TABLE connectors
    ADD CONSTRAINT fk_id_connectorauth FOREIGN KEY (id_connectorauth) REFERENCES connectorauth (id);