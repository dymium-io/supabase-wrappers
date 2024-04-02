package main

import (
	"dymium.com/dymium/log"

	"github.com/aws/aws-lambda-go/lambda"

	"database/sql"

	_ "github.com/lib/pq"

	"fmt"

	"golang.org/x/exp/maps"

	"crypto/aes"
	"crypto/cipher"
	"encoding/hex"

	. "dymium.io/DbSetup"
	"dymium.io/DbSetup/types"
)

var db *sql.DB

func LambdaHandler(c types.Request) (interface{}, error) {

	var cnf *conf
	var err error

	log.Infof("Request: %v", c)

	switch c.Action {
	case types.A_Delete:
		if cnf, err = getConf(c.Customer, true); err != nil {
			return nil, err
		}
		return doDelete(*c.Datascope, &cnf.GuardianConf)
	case types.A_Update:
		if cnf, err = getConf(c.Customer, true); err != nil {
			return nil, err
		}
	case types.A_Return:
		if cnf, err = getConf(c.Customer, false); err != nil {
			return nil, err
		}
	case types.A_ConfUser:
		if cnf, err = getConf(c.Customer, true); err != nil {
			return nil, err
		}
		return confUser(c.UserConf, &cnf.GuardianConf)
	case types.A_SqlTest:
		if cnf, err = getConf(c.Customer, true); err != nil {
			return nil, err
		}
		return sqlTest(c.Datascope, c.SqlTest, &cnf.GuardianConf)
	}
	if cnf == nil {
		return nil, fmt.Errorf("Wrong request: unrecognized action [%s]", c.Action)
	}

	if db == nil || db.Ping() != nil {
		if err := openDb(cnf); err != nil {
			return nil, err
		}
	}
	if err = db.Ping(); err != nil {
		return nil, err
	}

	log.Infof("getDatascopes: Customer: %v, Datascope: %v", c.Customer, c.Datascope)
	datascopes, err := getDatascopes(db, c.Customer, c.Datascope)
	if err != nil {
		return nil, err
	}

	log.Infof("getConnections: Customer: %v, Connector: %v", c.Customer, cnf.ConnectorDomain)
	connections, err := getConnections(db, c.Customer, cnf.ConnectorDomain)
	if err != nil {
		return nil, err
	}

	log.Infof("getCredentials: Customer: %v, Key: %v", c.Customer, cnf.GuardianConf.CustomerAESKey)
	credentials, err := getCredentials(db, c.Customer, cnf.GuardianConf.CustomerAESKey)
	if err != nil {
		return nil, err
	}

	switch c.Action {
	case types.A_Update:
		{
			if datascopes == nil || len(*datascopes) != 1 {
				return nil, fmt.Errorf("No data for datascope %q", *c.Datascope)
			} else {
				return doUpdate(
					&cnf.GuardianConf,
					&(*datascopes)[0],
					connections,
					credentials)
			}
		}
	case types.A_Return:
		return &types.CustomerData{
			Credentials: maps.Values(*credentials),
			Connections: maps.Values(*connections),
			Datascopes:  *datascopes}, nil
	}

	return nil, fmt.Errorf("Undefined action %v", c.Action)
}

func getCredentials(db *sql.DB, infoSchema, customerAESKey string) (*map[string]types.Credential, error) {
	rows, err := db.Query(fmt.Sprintf(`SELECT c.id, a.username, p.password
                                           FROM %s.connections c, %s.admincredentials a, %s.passwords p
                                           WHERE a.id = p.id AND a.connection_id = c.id`,
		infoSchema, infoSchema, infoSchema))
	if err != nil {
		return nil, err
	}
	creds := map[string]types.Credential{}
	for rows.Next() {
		var connectionId, userName string
		var bPassword []byte
		if err = rows.Scan(&connectionId, &userName, &bPassword); err != nil {
			return nil, err
		}
		var password string
		if customerAESKey == "" {
			password = string(bPassword)
		} else {
			if p, err := AESdecrypt(bPassword, customerAESKey); err != nil {
				return nil, err
			} else {
				password = string(p)
			}
		}
		creds[connectionId] = types.Credential{
			Connection_id: connectionId,
			User_name:     userName,
			Password:      password,
		}
	}
	log.Infof("Retrieved credentials")
	return &creds, nil
}

func getConnections(db *sql.DB, infoSchema, connectorDomain string) (*map[string]types.Connection, error) {
	rows, err := db.Query(fmt.Sprintf(`SELECT c.id, c.address, c.port, c.name, c.database_type, c.use_tls, c.dbname,
                                           c.use_connector, c.tunnel_id, connector.localport
                                           FROM %s.connections c
                                           LEFT JOIN %s.connectors connector ON c.tunnel_id = connector.id`,
		infoSchema, infoSchema))
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	connections := map[string]types.Connection{}
	for rows.Next() {
		var c types.Connection
		var useConnector bool
		var connectorPort *int
		var tunnelId string
		rows.Scan(&c.Id, &c.Address, &c.Port, &c.Name, &c.Database_type, &c.Use_tls, &c.Dbname,
			&useConnector, &tunnelId, &connectorPort)
		if useConnector {
			if connectorPort == nil {
				return &connections, fmt.Errorf("localport for connection %v with tunnel_id=%v is not defined", c.Id, tunnelId)
			}
			c.Address = infoSchema + connectorDomain
			c.Port = *connectorPort
		}
		if c.Address == "localhost" {
			c.Address = "docker.for.mac.host.internal"
		}
		connections[c.Id] = c
	}
	log.Infof("Retrieved connections")
	return &connections, nil
}

func getDatascopes(db *sql.DB, infoSchema string, infoDatascope *string) (*[]types.Scope, error) {
	infoDatascope_ := ""
	if infoDatascope != nil {
		infoDatascope_ = fmt.Sprintf("WHERE d.name = '%s'", esc(*infoDatascope))
	}
	rows, err := db.Query(fmt.Sprintf(`SELECT d.name, c.id,
                                      t.schem, t.tabl, t.col,
                                      t.typ, t.is_nullable,
                                      t.semantics, LOWER(t.action)
                               FROM %s.datascopes d
                               LEFT JOIN %s.tables t on t.datascope_id = d.id
                               LEFT JOIN %s.connections c on c.id = t.connection_id
                               %s
                               ORDER BY d.name, t.connection_id, t.schem, t.tabl, t."position"`,
		infoSchema, infoSchema, infoSchema, infoDatascope_))
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	var currentDatascope *types.Scope
	var currentConnection *types.External_connection
	var currentSchema *types.Schema
	var currentTable *types.Table

	datascopes := make([]types.Scope, 0)
	for rows.Next() {
		var dName string
		var cID, sName, tName, colName, typ, semantics *string
		var action *types.DataHandling
		var isNullable *bool
		err := rows.Scan(&dName, &cID, &sName, &tName, &colName, &typ, &isNullable, &semantics, &action)
		if err != nil {
			return nil, err
		}

		if cID == nil || sName == nil || tName == nil || colName == nil || typ == nil || semantics == nil || action == nil || isNullable == nil {
			fmt.Printf("cid=%v sname=%v tname=%v colName=%v typ=%v semantics=%v action=%v isNullable=%v\n",
				cID, sName, tName, colName, typ, semantics, action, isNullable)
			continue
		}

		if currentDatascope == nil || currentDatascope.Name != dName {
			datascopes = append(datascopes, types.Scope{Name: dName})
			currentDatascope = &datascopes[len(datascopes)-1]
			currentConnection = nil // Reset since we are in a new datascope
		}

		if currentConnection == nil || currentConnection.Connection_id != *cID {
			currentDatascope.External_connections = append(currentDatascope.External_connections,
				types.External_connection{Connection_id: *cID})
			currentConnection = &currentDatascope.External_connections[len(currentDatascope.External_connections)-1]
			currentSchema = nil // Reset since we are in a new connection
		}

		if currentSchema == nil || currentSchema.Name != *sName {
			currentConnection.Schemas = append(currentConnection.Schemas, types.Schema{Name: *sName})
			currentSchema = &currentConnection.Schemas[len(currentConnection.Schemas)-1]
			currentTable = nil // Reset since we are in a new schema
		}

		if currentTable == nil || currentTable.Name != *tName {
			currentSchema.Tables = append(currentSchema.Tables, types.Table{Name: *tName})
			currentTable = &currentSchema.Tables[len(currentSchema.Tables)-1]
		}

		currentTable.Columns = append(currentTable.Columns, types.Column{
			Name:       *colName,
			Typ:        *typ,
			IsNullable: *isNullable,
			Semantics:  *semantics,
			Action:     *action,
		})
	}

	if err = rows.Err(); err != nil {
		return nil, err
	}

	log.Infof("Retrieved datascopes")
	return &datascopes, nil
}

func openDb(cnf *conf) (err error) {

	sslmode_ := "disable"
	if cnf.DymiumTls {
		sslmode_ = "require"
	}

	psqlconn := fmt.Sprintf("host=%s port=%d dbname=%s user=%s password='%s' sslmode=%s",
		cnf.DymiumHost, cnf.DymiumPort, cnf.DymiumDatabase, cnf.DymiumUser, cnf.DymiumPassword, sslmode_)
	db, err = sql.Open("postgres", psqlconn)
	if err != nil {
		return err
	}
	return nil
}

func AESdecrypt(ciphertext []byte, keyhex string) ([]byte, error) {
	key, err := hex.DecodeString(keyhex)
	if err != nil {
		return nil, err
	}

	c, err := aes.NewCipher(key)
	if err != nil {
		return nil, err
	}

	gcm, err := cipher.NewGCM(c)
	if err != nil {
		return nil, err
	}

	nonceSize := gcm.NonceSize()
	if len(ciphertext) < nonceSize {
		return nil, fmt.Errorf("ciphertext too short")
	}

	nonce, ciphertext := ciphertext[:nonceSize], ciphertext[nonceSize:]
	return gcm.Open(nil, nonce, ciphertext, nil)
}

func esc(str string) string {
	return ParamEscape(str)
}

func main() {
	log.Init("DbSync")
	lambda.Start(LambdaHandler)
}
