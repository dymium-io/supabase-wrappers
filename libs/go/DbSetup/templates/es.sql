DO $$
BEGIN
  CREATE SERVER {{.Server}} FOREIGN DATA WRAPPER jdbc_fdw OPTIONS (
    drivername 'org.elasticsearch.xpack.sql.jdbc.EsDriver', url 'jdbc:es://{{if .Use_tls}}https{{else}}http{{end}}://{{.Address}}:{{.Port}}{{if .Use_tls}}?ssl=true{{end}}',jarfile '/jdbc_drv/x-pack-sql-jdbc-8.12.0.jar',maxheapsize '600'
  );
  CREATE USER MAPPING FOR public SERVER {{.Server}} OPTIONS (
    username '{{.User}}', password '{{.Password}}'
  );
END $$;
