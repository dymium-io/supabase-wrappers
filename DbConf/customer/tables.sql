-- #!migration
-- name: "customer/tables",
-- description: "Descriotion of foreign tables",
-- requires: ["customer/datascopes", "customer/connections", "customer/refs"];

CREATE TABLE tables (
    id varchar(36) PRIMARY KEY DEFAULT public.uuid_generate_v4(),
    datascope_id varchar(36) NOT NULL REFERENCES datascopes(id),
    connection_id varchar(36) NOT NULL REFERENCES connections(id),
    schem varchar(128) NOT NULL,
    tabl varchar(128) NOT NULL,
    action varchar(32) NOT NULL,
    col varchar(128) NOT NULL,
    typ varchar(32) NOT NULL,
    "position" integer,
    reference varchar(36) REFERENCES refs(id),
    semantics varchar(32) NOT NULL,
    UNIQUE (schem, tabl, col)
);

-- #!migration
-- name: "customer/tables-isnullable",
-- description: "Add is_nullable and dflt fields to tables",
-- requires: ["customer/tables"];

ALTER TABLE tables
      ADD COLUMN is_nullable BOOLEAN NOT NULL DEFAULT 'false';
ALTER TABLE tables
      ALTER COLUMN is_nullable DROP DEFAULT;
ALTER TABLE tables
      ADD COLUMN dflt text;

-- #!migration
-- name: "customer/tables-remove-ref",
-- description: "remove reference, move its columns here",
-- requires: ["customer/tables"];      
ALTER TABLE tables 
      drop column reference cascade;
ALTER TABLE tables 
       add column ref_schem varchar(128);      
ALTER TABLE tables 
       add column ref_tabl varchar(128);
ALTER TABLE tables 
       add column ref_col varchar(128);


-- #!migration
-- name: "customer/tables-adjust-unique",
-- description: "add datascopeid to unique",
-- requires: ["customer/tables"];     
ALTER TABLE tables drop constraint tables_schem_tabl_col_key;
ALTER TABLE tables add constraint tables_datascope_id_schem_tabl_col_key unique(datascope_id, schem, tabl, col);

-- #!migration
-- name: "customer/tables-add-possibles",
-- description: "add possible actions",
-- requires: ["customer/tables", "customer/tables-remove-ref"]; 
ALTER TABLE tables add column possible_actions text[] default '{allow, block, redact, obfuscate}' not null;

-- #!migration
-- name: "customer/tables-remove-size",
-- description: "remove size spec",
-- requires: ["customer/tables", "customer/tables-add-possibles"]; 

ALTER TABLE tables ALTER COLUMN schem TYPE varchar;
ALTER TABLE tables ALTER COLUMN tabl TYPE varchar;
ALTER TABLE tables ALTER COLUMN col TYPE varchar;

-- #!migration
-- name: "customer/tables-remove-size1",
-- description: "remove size spec again",
-- requires: ["customer/tables", "customer/tables-remove-size"]; 
ALTER TABLE tables ALTER COLUMN ref_schem TYPE varchar;
ALTER TABLE tables ALTER COLUMN ref_tabl TYPE varchar;
ALTER TABLE tables ALTER COLUMN ref_col TYPE varchar;
ALTER TABLE tables ALTER COLUMN semantics TYPE varchar;


-- #!migration
-- name: "customer/tables-remove-size2",
-- description: "remove size spec again again",
-- requires: ["customer/tables", "customer/tables-remove-size1"]; 
ALTER TABLE tables ALTER COLUMN action TYPE varchar;
ALTER TABLE tables ALTER COLUMN typ TYPE varchar;


-- #!migration
-- name: "customer/tables-adjust-constraint",
-- description: "add connectionid to unique",
-- requires: ["customer/tables-remove-size2"];     
ALTER TABLE tables drop constraint tables_datascope_id_schem_tabl_col_key;
ALTER TABLE tables add constraint tables_datascope_id_connection_id_schem_tabl_col_key unique(datascope_id, connection_id, schem, tabl, col);
