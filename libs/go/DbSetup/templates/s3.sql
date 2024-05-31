DO $$
  DECLARE
    key_id UUID;
    secret_key_id UUID;
BEGIN

SELECT insert_secret('vault_access_key_id','{{.User}}') INTO key_id;
SELECT insert_secret('vault_secret_key_id','{{.Password}}') INTO secret_key_id;

-- TODO: Make aws_region configurable
EXECUTE format('CREATE SERVER {{.Server}} FOREIGN DATA WRAPPER s3_wrapper OPTIONS (vault_access_key_id %L, vault_secret_access_key %L, aws_region %L )',key_id, secret_key_id, 'us-west-2');

EXCEPTION
    WHEN duplicate_object THEN
        RAISE EXCEPTION 'The server already exists.';
WHEN OTHERS THEN
        RAISE EXCEPTION 'An unexpected error occurred: %', SQLERRM;
END $$;
