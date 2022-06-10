SET search_path TO mallard;

CREATE TABLE applied_migrations(
    id              bigserial       NOT NULL,
    name            text            NOT NULL,
    schema          text[]          NOT NULL CHECK (array_position(schema, NULL) IS NULL),
    description     text            NOT NULL,
    requires        text[]          NOT NULL CHECK (array_position(requires, NULL) IS NULL),
    checksum        bytea           NOT NULL,
    script_text     text            NOT NULL,
    applied_on      timestamptz     NOT NULL DEFAULT now(),

    PRIMARY KEY (id),
    UNIQUE(name)
);
