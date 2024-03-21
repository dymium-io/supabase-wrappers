DO $$
BEGIN
  CREATE SERVER {{.Server}} FOREIGN DATA WRAPPER db2_fdw OPTIONS (
    dbserver 'Driver=/var/lib/postgresql/sqllib/lib64/libdb2o.so;Hostname={{.Address}};Port={{.Port}};Protocol=TCPIP;Database={{.Database}};{{if .Use_tls}}Security=SSL;{{end}}'
  );
  CREATE USER MAPPING FOR public SERVER {{.Server}} OPTIONS (
    user '{{.User}}', password '{{.Password}}'
  );
END $$;
