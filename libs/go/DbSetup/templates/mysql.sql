DO $$
BEGIN
  CREATE SERVER {{.Server}} FOREIGN DATA WRAPPER mysql_fdw OPTIONS (
    host '{{.Address}}', port '{{.Port}}'
  );
  CREATE USER MAPPING FOR public SERVER {{.Server}} OPTIONS (
    username '{{.User}}', password '{{.Password}}'
  );
END $$;