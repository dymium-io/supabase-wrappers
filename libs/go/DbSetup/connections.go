package DbSetup

import (
	"fmt"

	"dymium.io/DbSetup/types"
)

func setupConnections(exec func(string, ...interface{}) error,
	gdbConnections []string,
	connections map[string]types.Connection,
	credentials map[string]types.Credential,
) error {
	var err error
	connectionTypes := map[types.ConnectionType]struct{}{}
	for _, k := range gdbConnections {
		connectionTypes[connections[k].Database_type] = struct{}{}
	}

	for ct := range connectionTypes {
		if err = exec(ct_options[ct].ext); err != nil {
			return err
		}
	}

	for _, k := range gdbConnections {
		c, ok := connections[k]
		if !ok {
			return fmt.Errorf("Connection %s is not defined", k)
		}
		cred, ok := credentials[c.Id]
		if !ok {
			return fmt.Errorf("Can not find credentials for " + c.Id)
		}

		if sql, err := ct_options[c.Database_type].server_def(serverName(c.Name), c.Address, c.Port, c.Dbname, c.Use_tls, cred.User_name, cred.Password); err != nil {
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
