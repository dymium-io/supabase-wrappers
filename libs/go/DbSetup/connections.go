package DbSetup

import (
	"fmt"
	"strings"

	"dymium.io/DbSetup/types"
)

func setupConnections(exec func(string, ...interface{}) error,
	gdbConnections []types.External_connection,
	connections map[string]types.Connection,
	credentials map[string]types.Credential,
) error {
	var err error
	connectionTypes := map[types.ConnectionType]struct{}{}
	for _, c := range gdbConnections {
		connectionTypes[connections[c.Connection_id].Database_type] = struct{}{}
	}

	for ct := range connectionTypes {
		if err = exec(ct_options[ct].ext); err != nil {
			return err
		}
	}

	for _, con := range gdbConnections {
		c, ok := connections[con.Connection_id]
		if !ok {
			return fmt.Errorf("Connection %s is not defined", con.Connection_id)
		}
		cred, ok := credentials[c.Id]
		if !ok {
			return fmt.Errorf("Can not find credentials for " + c.Id)
		}
		s3region := "us-west-2"
		dbName := c.Dbname
		if c.Database_type == types.CT_S3 {
			// S3 connection string is in the form of "region/dbname"
			// The region is optional, if not provided, it defaults to "us-west-2"
			//TODO: add separate parameter for region in UI
			params := strings.Split(c.Dbname, "/")
			if len(params) == 2 {
				s3region = params[0]
				dbName = params[1]
			}
		}
		if sql, err := ct_options[c.Database_type].server_def(serverName(c.Name), c.Address, c.Port, dbName, s3region, c.Use_tls, cred.User_name, cred.Password); err != nil {
			return fmt.Errorf("server_def(%q, %q, %d, %q, %v, %q, \"******\") returned %v", serverName(c.Name), c.Address, c.Port, c.Dbname, c.Use_tls, cred.User_name, cred.Password, err)
		} else if err = exec(sql); err != nil {
			return err
		}

		if err := exec("INSERT INTO _dymium.servers (server) VALUES ( $1 )", serverName(c.Name)); err != nil {
			return err
		}
	}

	return nil
}
