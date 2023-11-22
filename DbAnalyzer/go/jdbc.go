package main

import (
	"DbAnalyzer/detect"
	"DbAnalyzer/types"
	"DbAnalyzer/utils"
	"bytes"
	"dymium.com/dymium/log"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"strings"
)

type DbConnectDto struct {
	dbType     string `json:"dbType"`
	host       string `json:"host"`
	port       int    `json:"port"`
	database   string `json:"database"`
	properties string `json:"properties"`
	user       string `json:"user"`
	password   string `json:"password"`
}

type DbConnectResponse struct {
	status  string `json:"status"`
	message string `json:"message"`
}

type DbSchemasResponse struct {
	status  string   `json:"status"`
	message string   `json:"message"`
	schemas []string `json:"schemas"`
}

type DbTabTypesResponse struct {
	status   string   `json:"status"`
	message  string   `json:"message"`
	tabtypes []string `json:"tabtypes"`
}

type DbTabListResponse struct {
	status  string   `json:"status"`
	message string   `json:"message"`
	tables  []string `json:"tables"`
}

type DbColListResponse struct {
	status  string         `json:"status"`
	message string         `json:"message"`
	columns []DbColumnInfo `json:"tables"`
}

type DbColumnInfo struct {
	tableCat string `json:"tableCat"`
	//TABLE_CAT String => table catalog (may be null)
	tableSchema string `json:"tableSchema"`
	//TABLE_SCHEM String => table schema (may be null)
	tableName string `json:"tableName"`
	//TABLE_NAME String => table name
	columnName string `json:"columnName"`
	//COLUMN_NAME String => column name
	dataType int `json:"dataType"`
	//DATA_TYPE int => SQL type from java.sql.Types
	dataTypeStr string `json:"dataTypeStr"`
	//Added conversion from java.sql.Types to String
	typeName string `json:"typeName"`
	//TYPE_NAME String => Data source dependent type name,
	// for a UDT the type name is fully qualified
	columnSize int `json:"columnSize"`
	//COLUMN_SIZE int => column size.
	//The COLUMN_SIZE column specifies the column size for the given column.
	//For numeric data, this is the maximum precision.
	//For character data, this is the length in characters.
	//For datetime datatypes, this is the length in characters of the String representation
	//(assuming the maximum allowed precision of the fractional seconds component).
	//For binary data, this is the length in bytes.
	//For the ROWID datatype, this is the length in bytes. Null is returned for data types where the column size is not applicable.
	decimalDigits int `json:"decimalDigits"`
	//DECIMAL_DIGITS int => the number of fractional digits. Null is returned for data types where DECIMAL_DIGITS is not applicable.
	numPrecRadix int `json:"numPrecRadix"`
	//NUM_PREC_RADIX int => Radix (typically either 10 or 2)
	nullable int `json:"nullable"`
	//NULLABLE int => is NULL allowed.
	//columnNoNulls - might not allow NULL values
	//columnNullable - definitely allows NULL values
	//columnNullableUnknown - nullability unknown
	remarks string `json:"remarks"`
	//REMARKS String => comment describing column (may be null)
	columnDef string `json:"columnDef"`
	//COLUMN_DEF String => default value for the column, which should be interpreted
	// as a string when the value is enclosed in single quotes (may be null)
	charOctetLength int `json:"charOctetLength"`
	//CHAR_OCTET_LENGTH int => for char types the maximum number of bytes in the column
	ordinalPosition int `json:"ordinalPosition"`
	//ORDINAL_POSITION int => index of column in table (starting at 1)
	isNullable string `json:"isNullable"`
	//IS_NULLABLE String => ISO rules are used to determine the nullability for a column.
	//YES --- if the column can include NULLs
	//NO --- if the column cannot include NULLs
	//empty string --- if the nullability for the column is unknown
	scopeCatalog string `json:"scopeCatalog"`
	//SCOPE_CATALOG String => catalog of table that is the scope of a reference attribute (null if DATA_TYPE isn't REF)
	scopeSchema string `json:"scopeSchema"`
	//SCOPE_SCHEMA String => schema of table that is the scope of a reference attribute (null if the DATA_TYPE isn't REF)
	scopeTable string `json:"scopeTable"`
	//SCOPE_TABLE String => table name that this the scope of a reference attribute (null if the DATA_TYPE isn't REF)
	sourceData int `json:"sourceData"`
	//SOURCE_DATA_TYPE short => source type of a distinct type or user-generated Ref type, SQL type from java.sql.Types
	// (null if DATA_TYPE isn't DISTINCT or user-generated REF)
	sourceDataStr string `json:"sourceDataStr"`
	// Convert sourceData to name
	isAutoIncrement string `json:"isAutoIncrement"`
	//IS_AUTOINCREMENT String => Indicates whether this column is auto incremented
	//YES --- if the column is auto incremented
	//NO --- if the column is not auto incremented
	//empty string --- if it cannot be determined whether the column is auto incremented
	isGeneratedColumn string `json:"isGeneratedColumn"`
	//IS_GENERATEDCOLUMN String => Indicates whether this is a generated column
	//YES --- if this a generated column
	//NO --- if this not a generated column
	//empty string --- if it cannot be determined whether this is a generated column
}

type DbSampleResponse struct {
	status  string                   `json:"status"`
	message string                   `json:"message"`
	data    []map[string]interface{} `json:"data"`
}

type JdbcClient struct {
	//mcl *mongo.Client
	ktDAHost   string
	ktDAPort   int
	sourceType string
	host       string
	port       int
	database   string
	properties string
	user       string
	password   string
}

// Define a map to store the connection type to string mapping for known connection types
var connTypesURL = map[types.ConnectionType]string{
	types.CT_ElasticSearch: "elasticsearch",
}

func (cl *JdbcClient) sendRequest(path string) ([]byte, error) {
	postPayload := DbConnectDto{
		dbType:     cl.sourceType,
		host:       cl.host,
		port:       cl.port,
		database:   cl.database,
		properties: cl.properties, // TODO: add support for different connection types, TLS, etc.
		user:       cl.user,
		password:   cl.password,
	}
	// Convert the payload to JSON
	jsonPayload, err := json.Marshal(postPayload)
	if err != nil {
		log.Errorf("Error encoding JSON:", err)
		return nil, err
	}
	apiUrl := fmt.Sprintf("http://%s:%d/%s", cl.ktDAHost, cl.ktDAPort, path)
	// Create a POST request with the JSON payload
	req, err := http.NewRequest("POST", apiUrl, bytes.NewBuffer(jsonPayload))
	if err != nil {
		log.Errorf("Error creating request:", err)
		return nil, err
	}

	// Set the Content-Type header to specify JSON
	req.Header.Set("Content-Type", "application/json")
	// Create an HTTP client
	client := &http.Client{}

	// Send the POST request
	resp, err := client.Do(req)
	if err != nil {
		log.Errorf("Error sending request:", err)
		return nil, err
	}
	defer resp.Body.Close()

	// Check if the response status code is not 201 Created
	if resp.StatusCode != http.StatusOK {
		log.Errorf("API request failed with status code:", resp.StatusCode)
		return nil, errors.New("API request failed: %s" + resp.Status)
	}

	// Read the response body
	responseBytes, err := io.ReadAll(resp.Body)
	if err != nil {
		log.Errorf("Error reading response body:", err)
		return nil, err
	}
	return responseBytes, nil
}

func (cl *JdbcClient) Ping() error {
	// TODO - implement ping, in case of ElasticSearch we can use the _cat/health API, also need ConnectionParams
	return nil
}

func (cl *JdbcClient) Close() {
}

func (cl *JdbcClient) Connect(c *types.ConnectionParams) error {
	// TODO: add support for different connection types, TLS, etc.
	srcType, ok := connTypesURL[c.Typ]
	if !ok {
		log.Errorf("Connection type %s is not supported", c.Typ)
		return errors.New("connection type is not supported")
	}
	cl.sourceType = srcType
	cl.host = c.Address
	cl.port = c.Port
	cl.database = c.Database
	cl.properties = "" // FIXME - add support for different connection types, TLS, etc.
	cl.user = c.User
	cl.password = c.Password

	responseBytes, err := cl.sendRequest("api/dbanalyzer/dbping")
	if err != nil {
		log.Errorf("Error sending request:", err)
		return err
	}

	// Parse the JSON response into a struct
	var postResponse DbConnectResponse

	if err := json.Unmarshal(responseBytes, &postResponse); err != nil {
		log.Errorf("Error decoding JSON response:", err)
		return err
	}

	log.Infof("Connected to  server: %s", postResponse.message)

	return nil
}

func (cl *JdbcClient) GetDbInfo(dbName string) (*types.DatabaseInfoData, error) {
	if dbName == "" {
		// if no database name is specified (ex. there is no database name for given datasource), use the default database
		dbName = "_default_db_"
	}
	database := types.DatabaseInfoData{
		DbName:  dbName,
		Schemas: []types.Schema{},
	}

	responseBytes, err := cl.sendRequest("api/dbanalyzer/dbschemas")
	if err != nil {
		log.Errorf("Error getting schema info:", err)
		return nil, err
	}

	// Parse the JSON response into a struct
	var postResponse DbSchemasResponse

	if err := json.Unmarshal(responseBytes, &postResponse); err != nil {
		log.Errorf("Error decoding JSON response:", err)
		return nil, err
	}

	if len(postResponse.schemas) > 0 {
		for _, schema := range postResponse.schemas {
			database.Schemas = append(database.Schemas, types.Schema{
				Name:     schema,
				IsSystem: false, // TODO: add support for system schemas
				Tables:   []types.Table{},
			})
		}
	} else {
		// Database has no schemas, so we create a single schema with the database name
		database.Schemas = append(database.Schemas, types.Schema{
			Name:     dbName,
			IsSystem: false, // TODO: add support for system schemas
			Tables:   []types.Table{},
		})
	}

	for _, schema := range database.Schemas {
		reqURL := fmt.Sprintf("api/dbanalyzer/dbtables")
		if schema.Name != "_default_db_" {
			reqURL = fmt.Sprintf("%s/%s", reqURL, schema)
		}
		responseBytes, err := cl.sendRequest(reqURL)
		if err != nil {
			log.Errorf("Error getting table info:", err)
			return nil, err
		}

		// Parse the JSON response into a struct
		var postResponse DbTabListResponse

		if err := json.Unmarshal(responseBytes, &postResponse); err != nil {
			log.Errorf("Error decoding JSON response:", err)
			return nil, err
		}

		for _, table := range postResponse.tables {
			schema.Tables = append(schema.Tables, types.Table{
				Name:     table,
				IsSystem: false, // TODO: add support for system tables
			})
		}

	}

	return &database, nil
}

func (cl *JdbcClient) GetTblInfo(dbName string, tip *types.TableInfoParams) (*types.TableInfoData, error) {
	reqURL := fmt.Sprintf("api/dbanalyzer/dbcolumns")
	if tip.Schema != "" && tip.Schema != "_default_db_" {
		reqURL = fmt.Sprintf("%s/%s", reqURL, tip.Schema)
	}
	reqURL = fmt.Sprintf("%s/%s", reqURL, tip.Table)

	responseBytes, err := cl.sendRequest(reqURL)
	if err != nil {
		log.Errorf("Error getting columns info:", err)
		return nil, err
	}

	// Parse the JSON response into a struct
	var postResponse DbColListResponse
	if err := json.Unmarshal(responseBytes, &postResponse); err != nil {
		log.Errorf("Error decoding JSON response:", err)
		return nil, err
	}

	log.Infof("Got column info for:", tip.Table)

	ti := types.TableInfoData{
		DbName:  dbName,
		Schema:  tip.Schema,
		TblName: tip.Table,
	}

	type data struct {
		cName           string
		pos             int
		isNullable      bool
		dflt            *string
		cTyp            string
		cLength, cScale *int
	}

	descr := []*data{}

	for _, column := range postResponse.columns {
		var d data
		var isNullable_ string
		d.pos = column.ordinalPosition
		d.cName = column.columnName
		d.cTyp = column.dataTypeStr
		*d.cLength = column.columnSize
		*d.cScale = column.decimalDigits
		isNullable_ = column.isNullable
		if isNullable_ == "YES" {
			d.isNullable = true
		} else {
			d.isNullable = false
		}
		descr = append(descr, &d)
	}

	detectors, err := detect.Compile(tip.Rules)
	if err != nil {
		return nil, err
	}

	sample := make([]detect.Sample, len(descr))

	obfuscatable := &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Obfuscate, types.DH_Allow}
	allowable := &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Allow}
	blocked := &[]types.DataHandling{types.DH_Block}

	for k, d := range descr {
		var possibleActions *[]types.DataHandling
		var t string
		var sem *string
		dtk := func(isSamplable bool, sem ...*string) detect.Sample {
			var s *string
			if len(sem) == 1 {
				s = sem[0]
			}
			return detect.Sample{
				IsSamplable: isSamplable,
				IsNullable:  d.isNullable,
				Name:        d.cName,
				Semantics:   s,
			}
		}
		switch d.cTyp {
		case "ARRAY":
			// FIXME: we need to get the element type
			t = d.cTyp
			possibleActions = blocked
			sem = utils.Unsupported
			sample[k] = dtk(false, sem)
		case "BIGINT":
			t = "bigint"
			possibleActions = allowable
			sample[k] = dtk(true)
		case "DECIMAL":
			if d.cLength != nil && *d.cLength != 0 {
				if d.cScale != nil && *d.cScale >= 0 {
					t = fmt.Sprintf("decimal(%d,%d)", *d.cLength, *d.cScale)
				} else {
					t = fmt.Sprintf("decimal(%d)", *d.cLength)
				}
			} else {
				t = "numeric"
			}
			possibleActions = allowable
			sample[k] = dtk(true)
		case "NUMERIC":
			if d.cLength != nil && *d.cLength != 0 {
				if d.cScale != nil && *d.cScale >= 0 {
					t = fmt.Sprintf("numeric(%d,%d)", *d.cLength, *d.cScale)
				} else {
					t = fmt.Sprintf("numeric(%d)", *d.cLength)
				}
			} else {
				t = "numeric"
			}
			possibleActions = allowable
			sample[k] = dtk(true)
		case "BIT":
			if d.cLength != nil && *d.cLength != 0 {
				t = fmt.Sprintf("bit(%d)", *d.cLength)
			} else {
				t = "bit"
			}
		case "BLOB", "BINARY", "VARBINARY":
			possibleActions = allowable
			t = "bytea"
			sample[k] = dtk(false)
		case "BOOLEAN":
			t = "boolean"
			possibleActions = allowable
			sample[k] = dtk(true)
		case "CHAR":
			if d.cLength != nil && *d.cLength > 0 {
				possibleActions = obfuscatable
				t = fmt.Sprintf("character(%d)", *d.cLength)
			} else {
				possibleActions = allowable
				t = "bpchar"
			}
			sample[k] = dtk(true)
		case "CLOB":
			t = "text"
			possibleActions = obfuscatable
			sample[k] = dtk(false)
		case "DATALINK":
			//Identifies the generic SQL type DATALINK.
			// TODO: check if this is correct
			t = d.cTyp
			possibleActions = blocked
			sem = utils.Unsupported
			sample[k] = dtk(false, sem)
		case "DATE":
			t = "date"
			possibleActions = allowable
			sample[k] = dtk(true)
		case "DISTINCT":
			//Identifies the generic SQL type DISTINCT.
			// TODO: check if this is correct
			t = d.cTyp
			possibleActions = blocked
			sem = utils.Unsupported
			sample[k] = dtk(false, sem)
		case "DOUBLE":
			t = "double precision"
			possibleActions = allowable
			sample[k] = dtk(true)
		case "FLOAT", "REAL":
			t = "real"
			possibleActions = allowable
			sample[k] = dtk(true)
		case "INTEGER":
			t = "integer"
			possibleActions = allowable
			sample[k] = dtk(true)
		case "JAVA_OBJECT":
			//Indicates that the SQL type is database-specific and gets mapped
			//to a Java object that can be accessed via the methods getObject and setObject.
			// TODO: check if this is correct
			t = d.cTyp
			possibleActions = blocked
			sem = utils.Unsupported
			sample[k] = dtk(false, sem)
		case "LONGNVARCHAR":
			//Identifies the generic SQL type LONGNVARCHAR.
			if d.cLength != nil && *d.cLength > 0 {
				t = fmt.Sprintf("varchar(%d)", *d.cLength)
			} else {
				t = "text"
			}
			possibleActions = obfuscatable
			sample[k] = dtk(true)
		case "LONGVARBINARY":
			//Identifies the generic SQL type LONGVARBINARY.
			// TODO: check if this is correct
			possibleActions = allowable
			t = "bytea"
			sample[k] = dtk(false)
		case "LONGVARCHAR":
			//Identifies the generic SQL type LONGVARCHAR.
			if d.cLength != nil && *d.cLength > 0 {
				t = fmt.Sprintf("varchar(%d)", *d.cLength)
			} else {
				t = "text"
			}
			possibleActions = obfuscatable
			sample[k] = dtk(true)
		case "NCHAR":
			//Identifies the generic SQL type NCHAR.
			// TODO: check if this is correct
			if d.cLength != nil && *d.cLength > 0 {
				possibleActions = obfuscatable
				t = fmt.Sprintf("character(%d)", *d.cLength)
			} else {
				possibleActions = allowable
				t = "bpchar"
			}
			sample[k] = dtk(true)
		case "NCLOB":
			//Identifies the generic SQL type NCLOB.
			// TODO: check if this is correct
			if d.cLength != nil && *d.cLength > 0 {
				t = fmt.Sprintf("varchar(%d)", *d.cLength)
			} else {
				t = "text"
			}
			possibleActions = obfuscatable
			sample[k] = dtk(true)
		case "NULL":
			//Identifies the generic SQL value NULL.
			// TODO: check if this is correct
			t = d.cTyp
			possibleActions = blocked
			sem = utils.Unsupported
			sample[k] = dtk(false, sem)
		case "NVARCHAR":
			//Identifies the generic SQL type NVARCHAR.
			// TODO: check if this is correct
			if d.cLength != nil && *d.cLength > 0 {
				t = fmt.Sprintf("varchar(%d)", *d.cLength)
			} else {
				t = "text"
			}
			possibleActions = obfuscatable
			sample[k] = dtk(true)
		case "OTHER":
			//Indicates that the SQL type is database-specific and gets mapped to a Java object that can be accessed via the methods getObject and setObject.
			// TODO: check if this is correct
			t = d.cTyp
			possibleActions = blocked
			sem = utils.Unsupported
			sample[k] = dtk(false, sem)
		case "REF":
			//Identifies the generic SQL type REF.
			// TODO: check if this is correct
			t = d.cTyp
			possibleActions = blocked
			sem = utils.Unsupported
			sample[k] = dtk(false, sem)
		case "REF_CURSOR":
			//Identifies the generic SQL type REF_CURSOR.
			// TODO: check if this is correct
			t = d.cTyp
			possibleActions = blocked
			sem = utils.Unsupported
			sample[k] = dtk(false, sem)
		case "ROWID":
			//Identifies the SQL type ROWID.
			// TODO: check if this is correct
			t = d.cTyp
			possibleActions = blocked
			sem = utils.Unsupported
			sample[k] = dtk(false, sem)
		case "SMALLINT":
			t = "smallint"
			possibleActions = allowable
			sample[k] = dtk(true)
		case "SQLXML":
			t = "xml"
			possibleActions = allowable
			sample[k] = dtk(true)
		case "STRUCT":
			//Identifies the generic SQL type STRUCT.
			// TODO: check if this is correct
			t = d.cTyp
			possibleActions = blocked
			sem = utils.Unsupported
			sample[k] = dtk(false, sem)
		case "TIME":
			switch {
			case d.cScale == nil:
				t = "time"
			case *d.cScale == 0:
				t = "time"
			case *d.cScale > 6:
				t = "time (6) without time zone"
			default:
				t = fmt.Sprintf("time (%d) without time zone", *d.cScale)
			}
			possibleActions = allowable
			sample[k] = dtk(true)
		case "TIME_WITH_TIMEZONE":
			if d.cLength != nil && *d.cLength < 6 {
				t = fmt.Sprintf("time(%d) with time zone", *d.cLength)
			} else {
				t = "time with time zone"
			}
			possibleActions = allowable
			sample[k] = dtk(true)
		case "TIMESTAMP":
			if d.cScale != nil {
				t = fmt.Sprintf("timestamp (%d) with time zone", *d.cScale)
			} else {
				t = "time with time zone"
			}
			possibleActions = allowable
			sample[k] = dtk(true)
		case "TIMESTAMP_WITH_TIMEZONE":
			if d.cLength != nil && *d.cLength < 6 {
				t = fmt.Sprintf("timestamp(%d) with time zone", *d.cLength)
			} else {
				t = "timestamp with time zone"
			}
			possibleActions = allowable
			sample[k] = dtk(true)
		case "TINYINT":
			// TODO: check if this is correct
			t = "smallint"
			possibleActions = allowable
			sample[k] = dtk(true)
		case "VARCHAR":
			if d.cLength != nil && *d.cLength > 0 {
				t = fmt.Sprintf("varchar(%d)", *d.cLength)
			} else {
				t = "text"
			}
			possibleActions = obfuscatable
			sample[k] = dtk(true)
		default:
			t = d.cTyp
			possibleActions = blocked
			sem = utils.Unsupported
			sample[k] = dtk(false, sem)
		}
		c := types.Column{
			Name:            d.cName,
			Position:        d.pos,
			Typ:             t,
			IsNullable:      d.isNullable,
			Default:         nil,
			Reference:       nil,
			Semantics:       sem,
			PossibleActions: *possibleActions,
		}
		ti.Columns = append(ti.Columns, c)
	}

	if err = cl.getSample(tip.Schema, tip.Table, sample); err != nil {
		return nil, err
	}

	if err = detectors.FindSemantics(sample); err != nil {
		return nil, err
	}

	for k := range ti.Columns {
		if sample[k].Semantics != nil {
			ti.Columns[k].Semantics = sample[k].Semantics
		}
	}

	return &ti, nil
}

func (cl *JdbcClient) getSample(schema, table string, sample []detect.Sample) error {

	nColumns := len(sample)

	for _, s := range sample {
		if s.IsSamplable {
			s.Data = make([]*string, 0, detect.SampleSize)
		} else {
			s.Data = make([]*string, 0)
		}
	}

	i := make([]interface{}, 0, nColumns)
	s := make([]string, nColumns)
	p := make([]*string, nColumns)
	var colNames strings.Builder
	start := true
	for k := 0; k != nColumns; k++ {
		if sample[k].IsSamplable {
			if sample[k].IsNullable {
				i = append(i, &p[k])
			} else {
				i = append(i, &s[k])
			}
			if start {
				start = false
			} else {
				colNames.WriteString(", ")
			}
			//colNames.WriteString("[" + sample[k].Name + "]")
			colNames.WriteString("\"" + sample[k].Name + "\"")
		}
	}

	responseBytes, err := cl.sendRequest("api/dbanalyzer/dbschemas")
	if err != nil {
		log.Errorf("Error getting schema info:", err)
		return err
	}

	// Parse the JSON response into a struct
	var postResponse DbSampleResponse

	if err := json.Unmarshal(responseBytes, &postResponse); err != nil {
		log.Errorf("Error decoding JSON response:", err)
		return err
	}

	rows := postResponse.data

	for _, row := range rows {
		for k := range sample {
			if sample[k].IsSamplable {
				var strVal string
				// Convert the value of "fieldName" to string using fmt.Sprintf
				if val, ok := row[sample[k].Name]; ok {
					strVal = fmt.Sprintf("%v", val)
				} else {
					strVal = ""
				}
				if sample[k].IsNullable {
					if strVal == "" {
						sample[k].Data = append(sample[k].Data, nil)
					} else {
						sample[k].Data = append(sample[k].Data, &strVal)
					}
				} else {
					sample[k].Data = append(sample[k].Data, &strVal)
				}
			}
		}

	}

	return nil
}
