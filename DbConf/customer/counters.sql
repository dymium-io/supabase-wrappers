-- #!migration
-- name: "customer/counters",
-- description: "Counters";

CREATE TABLE counters (
    id integer UNIQUE default(1),
    logins bigint,
    bytesin bigint,
    bytesout bigint,
    Constraint check_counters_id CHECK (id = 1)
);

-- #!migration
-- name: "customer/initcounters",
-- description: "init counters table for a customer",
-- requires: ["customer/counters"];
insert into counters(logins, bytesin, bytesout) values(0, 0, 0);


-- #!migration
-- name: "customer/addtunnels",
-- description: "add a tunnels column",
-- requires: ["customer/counters", "customer/initcounters"];
alter table counters add tunnels bigint default 0;