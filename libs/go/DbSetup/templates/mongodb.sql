DO $$
BEGIN
  CREATE SERVER {{.Server}} FOREIGN DATA WRAPPER mongo_fdw OPTIONS (
    address '{{.Address}}', port '{{.Port}}', authentication_database 'admin'{{if .Use_tls}}, ssl 'true', weak_cert_validation 'true'{{end}}
  );
  CREATE USER MAPPING FOR public SERVER {{.Server}} OPTIONS (
    username '{{.User}}', password '{{.Password}}'
  );
END $$;
