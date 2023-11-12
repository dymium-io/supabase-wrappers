-- #!migration
-- name: "tables/phone",
-- description: "Phone numbers attached to a person.",
-- requires: ["tables/person"];

CREATE TABLE phone(
    id          BIGSERIAL       NOT NULL,
    owner_id    bigint          NOT NULL,
    digits      text            NOT NULL,

    PRIMARY KEY (id),
    
    FOREIGN KEY (owner_id) REFERENCES person(id)
);

-- #!migration
-- name: "tables/phone/name",
-- description: "Add name column to phone number.",
-- requires: ["tables/phone"];

ALTER TABLE phone ADD COLUMN name text;
