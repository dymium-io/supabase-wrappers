DO $$
BEGIN
  CREATE SERVER {{.Server}} FOREIGN DATA WRAPPER postgres_fdw OPTIONS (
    servername '{{.Address}}', port '{{.Port}}', database '{{.Database}}'
  );
  CREATE USER MAPPING FOR public SERVER {{.Server}} OPTIONS (
    username '{{.User}}', password '{{.Password}}'
  );
END $$;
