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


-- #!migration
-- name: "customer/connectors-change-default-status",
-- requires: ["customer/conectors", "customer/connectors-add-connectorname", "customer/connectors-add-auth", "customer/connectors-add-foreignkey"],
-- description: "change default status";
ALTER TABLE connectors ALTER COLUMN status SET DEFAULT 'provisioned';

-- #!migration
-- name: "customer/connectors-change-default-status1",
-- requires: ["customer/conectors", "customer/connectors-add-connectorname", "customer/connectors-add-auth", "customer/connectors-add-foreignkey"],
-- description: "change default status";
ALTER TABLE connectorauth ALTER COLUMN status SET DEFAULT 'provisioned';


-- #!migration
-- name: "customer/extend-connector-host",
-- requires: ["customer/connectors-change-default-status1"],
-- description: "Change targetaddress length";
ALTER TABLE connectors ALTER COLUMN targetaddress TYPE varchar (253);


-- #!migration
-- name: "customer/connectors-change-password",
-- requires: ["customer/connectors-add-auth"],
-- description: "Add accesssecretb field to hold encrypted password";
ALTER TABLE connectorauth ADD COLUMN accesssecretb bytea NOT NULL DEFAULT '\x';

-- #!migration
-- name: "customer/connectors-unique-name",
-- requires: ["customer/extend-connector-host"],
-- description: "Force unique connector name etc";
ALTER TABLE connectorauth add  unique(accesskey);
ALTER TABLE connectorauth add  unique(name);

