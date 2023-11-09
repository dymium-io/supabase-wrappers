-- #!migration
-- name: "customer/machinetunnels",
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







