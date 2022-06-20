package main

import (
	"github.com/aws/aws-lambda-go/lambda"

	"database/sql"
	_ "github.com/lib/pq"

	"github.com/aclements/go-gg/generic/slice"
	"fmt"

	"golang.org/x/exp/maps"

	"DbSync/types"
)

var db *sql.DB

func conType(ct string) (types.ConnectionType, error) {
	switch ct {
	case "postgres":
		return types.CT_PostgreSQL, nil
	case "mysql":
		return types.CT_MySQL, nil
	case "mariadb":
		return types.CT_MariaDB, nil
	case "sqlserver":
		return types.CT_SqlServer, nil
	case "oracle":
		return types.CT_OracleDB, nil
	}
	return "", fmt.Errorf("Connection type %s is not supported yet", ct)
}

func LambdaHandler(c types.Request) (interface{}, error) {

	var cnf *conf
	var err error

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
	}
	if cnf == nil {
		return nil, fmt.Errorf("Wrong request: %v",c)
	}


	if db == nil {
		if err := openDb(cnf); err != nil {
			return nil, err
		}
	}

	datascopes, err := getDatascopes(db, c.Customer, c.Datascope)
	if err != nil {
		return nil, err
	}

	connections, err := getConnections(db, c.Customer)
	if err != nil {
		return nil, err
	}

	credentials, err := getCredentials(db, c.Customer)
	if err != nil {
		return nil, err
	}

	switch c.Action {
	case types.A_Update: {
		if datascopes == nil || len(*datascopes) != 1 {
			return nil, fmt.Errorf("No data for datascope %q",*c.Datascope)
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

	return nil, fmt.Errorf("Undefined action %v",c.Action)
}

func getCredentials(db *sql.DB, infoSchema string) (*map[string]types.Credential, error) {
	rows, err := db.Query(fmt.Sprintf(`SELECT c.id, a.username, p.password
                                           FROM %s.connections c, %s.admincredentials a, %s.passwords p
                                           WHERE a.id = p.id AND a.connection_id = c.id`,
		infoSchema, infoSchema, infoSchema))
	if err != nil {
		return nil, err
	}
	creds := map[string]types.Credential{}
	for rows.Next() {
		var cred types.Credential
		if err = rows.Scan(&cred.Connection_id, &cred.User_name, &cred.Password); err != nil {
			return nil, err
		}
		creds[cred.Connection_id] = cred
	}
	return &creds, nil
}

func getConnections(db *sql.DB, infoSchema string) (*map[string]types.Connection, error) {
	rows, err := db.Query(fmt.Sprintf(`SELECT c.id, c.address, c.port, c.name, c.database_type, c.use_tls, c.dbname
                                           FROM %s.connections c`, infoSchema))
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	connections := map[string]types.Connection{}
	for rows.Next() {
		var c types.Connection
		var dt string
		rows.Scan(&c.Id, &c.Address, &c.Port, &c.Name, &dt, &c.Use_tls, &c.Dbname)
		if c.Database_type, err = conType(dt); err != nil {
			return nil, err
		}
		if c.Address == "localhost" {
			c.Address = "ocker.for.mac.host.internal"
		}
		connections[c.Id] = c
	}
	return &connections, nil
}

func getDatascopes(db *sql.DB, infoSchema string, infoDatascope *string) (*[]types.Datascope, error) {
	infoDatascope_ := ""
	if infoDatascope != nil {
		infoDatascope_ = fmt.Sprintf("d.name = '%s' AND ", esc(*infoDatascope))
	}
	rows, err := db.Query(fmt.Sprintf(`SELECT d.name, c.id,
                                      t.schem, t.tabl, t.col,
                                      t.typ, t.is_nullable,
                                      t.semantics, t.action
                               FROM %s.tables t, %s.datascopes d, %s.connections c
                               WHERE %s t.datascope_id = d.id AND t.connection_id = c.id
                               ORDER BY d.name, t.schem, t.tabl, t.connection_id, t."position"`,
		infoSchema, infoSchema, infoSchema, infoDatascope_))
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	datascopes := []types.Datascope{}
	var d *types.Datascope
	var s *types.Schema
	var t *types.Table
	for rows.Next() {
		var dscope, con, schem, tblName string
		var col types.Column
		err = rows.Scan(&dscope, &con, &schem, &tblName,
			&col.Name, &col.Typ, &col.IsNullable, &col.Semantics, &col.Action)
		if err != nil {
			return nil, err
		}
		if d == nil || dscope != d.Name {
			datascopes = append(datascopes, types.Datascope{
				Name:        dscope,
				Connections: []string{con},
				Schemas: []types.Schema{
					{
						Name: schem,
						Tables: []types.Table{
							{
								Name:       tblName,
								Connection: con,
								Columns:    []types.Column{col},
							},
						},
					},
				},
			})
			d = &datascopes[len(datascopes)-1]
			s = &d.Schemas[0]
			t = &s.Tables[0]
		} else if schem != s.Name {
			d.Schemas = append(d.Schemas, types.Schema{
				Name: schem,
				Tables: []types.Table{
					{
						Name:       tblName,
						Connection: con,
						Columns:    []types.Column{col},
					},
				},
			})
			d.Connections = append(d.Connections, con)
			s = &d.Schemas[len(d.Schemas)-1]
			t = &s.Tables[0]
		} else if tblName != t.Name || con != t.Connection {
			s.Tables = append(s.Tables, types.Table{
				Name:       tblName,
				Connection: con,
				Columns:    []types.Column{col},
			})
			d.Connections = append(d.Connections, con)
			t = &s.Tables[len(s.Tables)-1]
		} else {
			t.Columns = append(t.Columns, col)
		}
	}

	for k := range datascopes {
		d := &datascopes[k]
		slice.Sort(d.Connections)
		d.Connections = slice.Nub(d.Connections).([]string)
	}

	return &datascopes, nil
}

func openDb(cnf *conf) (err error) {
	
	sslmode_ := "disable"
	if cnf.DymiumTls { sslmode_ = "require" }
	
	psqlconn := fmt.Sprintf("host=%s port=%d dbname=%s user=%s password='%s' sslmode=%s",
		cnf.DymiumHost, cnf.DymiumPort, cnf.DymiumDatabase, cnf.DymiumUser, cnf.DymiumPassword, sslmode_)
	db, err = sql.Open("postgres", psqlconn)
	if err != nil {
		return err
	}
	return nil
}

func main() {
	lambda.Start(LambdaHandler)
}
