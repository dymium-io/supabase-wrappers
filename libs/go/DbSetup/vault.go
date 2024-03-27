package DbSetup

func setupVault(exec func(string, ...interface{}) error, localUser string) error {
	var err error

	if err = exec(`CREATE EXTENSION IF NOT EXISTS supabase_vault CASCADE`); err != nil {
		return err
	}
	if err = exec("GRANT USAGE ON SCHEMA vault TO " + localUser); err != nil {
		return err
	}

	if err = exec(`DO $$
	   DECLARE
	       uname text := '` + localUser + `';
	       tbl_name text;
	   BEGIN
	       EXECUTE format('GRANT USAGE ON SCHEMA vault TO %I;', uname);
	       EXECUTE format('GRANT EXECUTE ON FUNCTION pgsodium.crypto_aead_det_decrypt(bytea, bytea, uuid, bytea) TO %I;',uname);
	       CREATE EXTENSION IF NOT EXISTS wrappers WITH SCHEMA public;
	       EXECUTE format('GRANT SELECT, INSERT, UPDATE ON wrappers_fdw_stats TO %I;',uname);
	       FOR tbl_name IN SELECT table_name FROM information_schema.tables WHERE table_schema = 'vault'
	       LOOP
		   EXECUTE format('GRANT SELECT ON vault.%I TO %I;', tbl_name, uname);
	       END LOOP;
	   END$$;`); err != nil {
		return err
	}

	if err = exec(`
          CREATE OR REPLACE FUNCTION insert_secret(p_name text, p_secret text) RETURNS UUID AS $$
          DECLARE
              v_id UUID;
          BEGIN
             -- Attempt to update first and capture the id
             UPDATE vault.secrets
             SET secret = p_secret
             WHERE name = p_name
             RETURNING key_id INTO v_id;

             -- If the update did not affect any rows, then insert
             IF NOT FOUND THEN
                 INSERT INTO vault.secrets (name, secret)
                 VALUES (p_name, p_secret)
                 RETURNING key_id INTO v_id;
             END IF;

             -- Return the id of the affected row
             RETURN v_id;
         END;
         $$
         LANGUAGE plpgsql`); err != nil {
		return err
	}
	return nil
}
