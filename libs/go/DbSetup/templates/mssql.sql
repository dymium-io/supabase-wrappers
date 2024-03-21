DO $$
  DECLARE
    key_id UUID;
    ddl_command TEXT;
  BEGIN
    SELECT insert_secret ('{{.Server}}','Server={{.Address}},{{.Port}};User="{{.User}}";Password="{{.Password}}";Database={{.Database}};IntegratedSecurity=false;TrustServerCertificate=true;encrypt={{.Use_tls}};ApplicationName=dymium') INTO key_id;
    ddl_command := format('CREATE SERVER {{.Server}} FOREIGN DATA WRAPPER mssql_wrapper OPTIONS (conn_string_id  %L)',key_id);
    EXECUTE ddl_command;
  EXCEPTION
    WHEN duplicate_object THEN
        RAISE EXCEPTION 'The server already exists.';
    WHEN OTHERS THEN
        RAISE EXCEPTION 'An unexpected error occurred: %', SQLERRM;
END $$;
