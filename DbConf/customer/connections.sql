-- #!migration
-- name: "customer/connections",
-- description: "Connections";

CREATE TABLE connections (
    id varchar(36) PRIMARY KEY DEFAULT public.uuid_generate_v4(),
    address varchar(36) NOT NULL,
    port integer NOT NULL,
    name varchar(128) NOT NULL,
    database_type varchar(128) NOT NULL,
    use_tls boolean NOT NULL,
    description varchar(256),
    dbname varchar(128)
);

-- #!migration
-- name: "customer/connections-source-type",
-- description: "Change source_type to ENUM",
-- requires: ["customer/connections"];

CREATE TYPE source_type AS ENUM (
    'postgres',
    'mysql',
    'mariadb',
    'sqlserver',
    'oracle'
);

ALTER TABLE connections ALTER COLUMN database_type TYPE source_type
      USING database_type::source_type;

-- #!migration
-- name: "customer/make-name-unique",
-- description: "make name unique",
-- requires: ["customer/connections-source-type"];
ALTER TABLE connections ADD CONSTRAINT name_unique UNIQUE (name);;

-- #!migration
-- name: "customer/rename-enum",
-- description: "make enum consistent with the types in  yaml",
-- requires: ["customer/make-name-unique"];
ALTER TYPE source_type RENAME VALUE 'postgres' to 'PostgreSQL';  
ALTER TYPE source_type RENAME VALUE 'mysql' to 'MySQL';  
ALTER TYPE source_type RENAME VALUE 'mariadb' to 'MariaDB';  
ALTER TYPE source_type RENAME VALUE 'sqlserver' to 'SqlServer';  
ALTER TYPE source_type RENAME VALUE 'oracle' to 'OracleDB';  


-- #!migration
-- name: "customer/connections-nullable",
-- description: "Add connectors",
-- requires: ["customer/connections","customer/connections-source-type","customer/connections-source-type","customer/rename-enum"];
ALTER TABLE connections ADD column use_connector BOOLEAN DEFAULT FALSE;
ALTER TABLE connections ADD column connector_id varchar(128) ;

-- #!migration
-- name: "customer/connections-ftkeys",
-- description: "Add fks",
-- requires: ["customer/connections","customer/connections-source-type","customer/connections-source-type","customer/rename-enum", "customer/connections-nullable", "customer/connectors-add-auth"];
ALTER TABLE connections ADD CONSTRAINT connectorid_fk FOREIGN KEY (connector_id) REFERENCES connectorauth(id);
ALTER TABLE connections ADD column tunnel_id varchar(128) ;
ALTER TABLE connections ADD CONSTRAINT tunnelid_fk FOREIGN KEY (tunnel_id) REFERENCES connectors(id);

-- #!migration
-- name: "customer/connections-fix-hostname",
-- description: "Fix address length",
-- requires: ["customer/connections-ftkeys"];
ALTER TABLE connections ALTER COLUMN address TYPE varchar (253);

-- #!migration
-- name: "customer/connections-source-type-add-DB2",
-- description: "Add DB2 to source_type ENUM",
-- requires: ["customer/connections-source-type"];
ALTER TYPE source_type ADD VALUE 'DB2';

-- #!migration
-- name: "customer/connections-source-type-add-MongoDB",
-- description: "Add MongoDB to source_type ENUM",
-- requires: ["customer/connections-source-type"];
ALTER TYPE source_type ADD VALUE 'MongoDB';

-- #!migration
-- name: "customer/connections-source-type-add-Elasticsearch",
-- description: "Add Elasticsearch to source_type ENUM",
-- requires: ["customer/connections-source-type"];
ALTER TYPE source_type ADD VALUE 'Elasticsearch';

-- #!migration
-- name: "customer/connections-source-type-add-S3",
-- description: "Add Elasticsearch to source_type ENUM",
-- requires: ["customer/connections-source-type"];
ALTER TYPE source_type ADD VALUE 'S3';
