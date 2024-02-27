-- #!migration
-- name: "global/invitations",
-- description: "Customer's invitations";

CREATE TABLE global.invitations (
    id varchar(36) PRIMARY KEY DEFAULT public.uuid_generate_v4(),
    email character varying(128) NOT NULL,
    name character varying(128) NOT NULL,
    referedby character varying(128),
    issued timestamptz NOT NULL default CURRENT_TIMESTAMP
);

-- #!migration
-- name: "customer/save-json",
-- requires: ["global/invitations"],
-- description: "Add intermediate configuration into json";
ALTER TABLE global.invitations ADD COLUMN config jsonb NOT NULL DEFAULT '{}';


-- #!migration
-- name: "global/status-json",
-- requires: ["customer/save-json"],
-- description: "Add status";
ALTER TABLE global.invitations ADD COLUMN status jsonb NOT NULL DEFAULT '{"status": [], "progress": ""}';