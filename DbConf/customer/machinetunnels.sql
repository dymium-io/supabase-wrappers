-- #!migration
-- name: "customer/machinetunnels",
-- requires: ["customer/groupmapping-inunique"],
-- description: "Machine Tunnels";

CREATE TABLE machinetunnelauth (
    id varchar(36) PRIMARY KEY DEFAULT public.uuid_generate_v4(),
    accesskey varchar(128) NOT NULL,
    accesssecret varchar(128) NOT NULL
);

CREATE TABLE machinetunnels (
    id varchar(36) PRIMARY KEY DEFAULT public.uuid_generate_v4(),
    name varchar(128) NOT NULL,
    id_auth varchar(36) NOT NULL   REFERENCES machinetunnelauth(id),
    created_at timestamp NOT NULL DEFAULT now(),
    status varchar(128) NOT NULL DEFAULT 'pending'
);

CREATE TABLE machinetunnelgroups (
    id varchar(36) PRIMARY KEY DEFAULT public.uuid_generate_v4(),
    tunnel_id varchar(36) NOT NULL REFERENCES machinetunnels(id),
    group_id varchar(36) NOT NULL REFERENCES groupmapping(id)
);

-- #!migration
-- name: "customer/add-machinetunnel-passwords",
-- requires: ["customer/machinetunnels"],
-- description: "add passwords to machinetunnels";
ALTER TABLE machinetunnels ADD COLUMN username text NOT NULL DEFAULT '';
ALTER TABLE machinetunnels ADD COLUMN password bytea NOT NULL DEFAULT '\x';






