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
	"net/url"
	"strings"
	"time"
)

type DbConnectDto struct {
	DbType     string `json:"dbType"`
	Host       string `json:"host"`
	Port       int    `json:"port"`
	Database   string `json:"database"`
	Properties string `json:"properties"`
	User       string `json:"user"`
	Password   string `json:"password"`
	Columns    string `json:"columns"`
	Tls        bool   `json:"tls"`
}

type DbConnectResponse struct {
	Status  string `json:"status"`
	Message string `json:"message"`
}

type DbSchemasResponse struct {
	Status  string   `json:"status"`
	Message string   `json:"message"`
	Schemas []string `json:"schemas"`
}

type DbTabTypesResponse struct {
	Status   string   `json:"status"`
	Message  string   `json:"message"`
	Tabtypes []string `json:"tabtypes"`
}

type DbTabListResponse struct {
	Status  string   `json:"status"`
	Message string   `json:"message"`
	Tables  []string `json:"tables"`
}

type DbColumnInfo struct {
	TableCat string `json:"tableCat"`
	//TABLE_CAT String => table catalog (may be null)
	TableSchema string `json:"tableSchema"`
	//TABLE_SCHEM String => table schema (may be null)
	TableName string `json:"tableName"`
	//TABLE_NAME String => table name
	ColumnName string `json:"columnName"`
	//COLUMN_NAME String => column name
	DataType int `json:"dataType"`
	//DATA_TYPE int => SQL type from java.sql.Types
	DataTypeStr string `json:"dataTypeStr"`
	//Added conversion from java.sql.Types to String
	TypeName string `json:"typeName"`
	//TYPE_NAME String => Data source dependent type name,
	// for a UDT the type name is fully qualified
	ColumnSize int `json:"columnSize"`
	//COLUMN_SIZE int => column size.
	//The COLUMN_SIZE column specifies the column size for the given column.
	//For numeric data, this is the maximum precision.
	//For character data, this is the length in characters.
	//For datetime datatypes, this is the length in characters of the String representation
	//(assuming the maximum allowed precision of the fractional seconds component).
	//For binary data, this is the length in bytes.
	//For the ROWID datatype, this is the length in bytes. Null is returned for data types where the column size is not applicable.
	DecimalDigits int `json:"decimalDigits"`
	//DECIMAL_DIGITS int => the number of fractional digits. Null is returned for data types where DECIMAL_DIGITS is not applicable.
	NumPrecRadix int `json:"numPrecRadix"`
	//NUM_PREC_RADIX int => Radix (typically either 10 or 2)
	Nullable int `json:"nullable"`
	//NULLABLE int => is NULL allowed.
	//columnNoNulls - might not allow NULL values
	//columnNullable - definitely allows NULL values
	//columnNullableUnknown - nullability unknown
	Remarks string `json:"remarks"`
	//REMARKS String => comment describing column (may be null)
	ColumnDef string `json:"columnDef"`
	//COLUMN_DEF String => default value for the column, which should be interpreted
	// as a string when the value is enclosed in single quotes (may be null)
	CharOctetLength int `json:"charOctetLength"`
	//CHAR_OCTET_LENGTH int => for char types the maximum number of bytes in the column
	OrdinalPosition int `json:"ordinalPosition"`
	//ORDINAL_POSITION int => index of column in table (starting at 1)
	IsNullable string `json:"isNullable"`
	//IS_NULLABLE String => ISO rules are used to determine the nullability for a column.
	//YES --- if the column can include NULLs
	//NO --- if the column cannot include NULLs
	//empty string --- if the nullability for the column is unknown
	ScopeCatalog string `json:"scopeCatalog"`
	//SCOPE_CATALOG String => catalog of table that is the scope of a reference attribute (null if DATA_TYPE isn't REF)
	ScopeSchema string `json:"scopeSchema"`
	//SCOPE_SCHEMA String => schema of table that is the scope of a reference attribute (null if the DATA_TYPE isn't REF)
	ScopeTable string `json:"scopeTable"`
	//SCOPE_TABLE String => table name that this the scope of a reference attribute (null if the DATA_TYPE isn't REF)
	SourceData int `json:"sourceData"`
	//SOURCE_DATA_TYPE short => source type of a distinct type or user-generated Ref type, SQL type from java.sql.Types
	// (null if DATA_TYPE isn't DISTINCT or user-generated REF)
	SourceDataStr string `json:"sourceDataStr"`
	// Convert sourceData to name
	IsAutoIncrement string `json:"isAutoIncrement"`
	//IS_AUTOINCREMENT String => Indicates whether this column is auto incremented
	//YES --- if the column is auto incremented
	//NO --- if the column is not auto incremented
	//empty string --- if it cannot be determined whether the column is auto incremented
	IsGeneratedColumn string `json:"isGeneratedColumn"`
	//IS_GENERATEDCOLUMN String => Indicates whether this is a generated column
	//YES --- if this a generated column
	//NO --- if this not a generated column
	//empty string --- if it cannot be determined whether this is a generated column
}

type DbColListResponse struct {
	Status  string            `json:"status"`
	Message string            `json:"message"`
	Columns []json.RawMessage `json:"columns"`
}

type DbSampleResponse struct {
	Status  string            `json:"status"`
	Message string            `json:"message"`
	Data    []json.RawMessage `json:"data"`
}

type JdbcClient struct {
	//mcl *mongo.Client
	KtDAHost   string
	KtDAPort   int
	SourceType string
	Host       string
	Port       int
	Database   string
	Properties string
	User       string
	Password   string
	Tls        bool
}

// Define a map to store the connection type to string mapping for known connection types
var connTypesURL = map[types.ConnectionType]string{
	types.CT_Elasticsearch: "elasticsearch",
	types.CT_S3:            "s3",
}

func (cl *JdbcClient) sendRequest(path string, optionalParams map[string]string) ([]byte, error) {
	maxRetryAttempts := 3
	var err error
	for i := 0; i < maxRetryAttempts; i++ {
		log.Infof("Sending request to %s", path)
		var responseBytes []byte
		responseBytes, err = cl.attemptSendRequest(path, optionalParams)
		if err == nil {
			return responseBytes, nil
		}
		log.Errorf("Error sending request: %s", err)
		time.Sleep(4 * time.Second) // Wait before retrying
	}

	// If the code reaches this point, all retry attempts have failed, so return the last error
	log.Errorf("Failed after %d retry attempts: %s", maxRetryAttempts, err)
	return nil, fmt.Errorf("failed after %d retry attempts: %w", maxRetryAttempts, err)
}

func (cl *JdbcClient) attemptSendRequest(path string, optionalParams map[string]string) ([]byte, error) {
	postPayload := DbConnectDto{
		DbType:     cl.SourceType,
		Host:       cl.Host,
		Port:       cl.Port,
		Database:   cl.Database,
		Properties: cl.Properties, // TODO: add support for different connection types, TLS, etc.
		User:       cl.User,
		Password:   cl.Password,
		Tls:        cl.Tls,
	}

	if optionalParams != nil {
		// placeholder for optional parameters - source specific and request specific (ex. list of sampleable columns)
		if _, ok := optionalParams["columns"]; ok {
			postPayload.Columns = optionalParams["columns"]
		}
	}

	// Convert the payload to JSON
	jsonPayload, err := json.Marshal(postPayload)
	if err != nil {
		log.Errorf("Error encoding JSON: %s", err)
		return nil, err
	}

	apiUrl := fmt.Sprintf("http://%s:%d/%s", cl.KtDAHost, cl.KtDAPort, path)
	// Create a POST request with the JSON payload
	req, err := http.NewRequest("POST", apiUrl, bytes.NewBuffer(jsonPayload))
	if err != nil {
		log.Errorf("Error creating request: %s", err)
		return nil, err
	}

	// Set the Content-Type header to specify JSON
	req.Header.Set("Content-Type", "application/json")
	// Create an HTTP client
	client := &http.Client{}

	// Send the POST request
	resp, err := client.Do(req)
	if err != nil {
		log.Errorf("Error sending request: %s", err)
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
		log.Errorf("Error reading response body: %s", err)
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
	log.Infof("Connecting to %s", c.Typ)
	// TODO: add support for different connection types, TLS, etc.
	srcType, ok := connTypesURL[c.Typ]
	if !ok {
		log.Errorf("Connection type %s is not supported", c.Typ)
		return errors.New("connection type is not supported")
	}
	cl.SourceType = srcType
	cl.Host = c.Address
	cl.Port = c.Port
	cl.Database = c.Database
	if cl.SourceType == "s3" {
		// TODO: make aws zone configurable
		cl.Properties = "us-west-2"
	} else {
		cl.Properties = "" // FIXME - add support for different connection types, TLS, etc.
	}
	cl.User = c.User
	cl.Password = c.Password
	cl.Tls = c.Tls

	log.Infof("Connecting to Host:%s Port:%d DB:%s tls=%v", cl.Host, cl.Port, cl.Database, cl.Tls)

	responseBytes, err := cl.sendRequest("api/dbanalyzer/dbping", nil)
	if err != nil {
		log.Errorf("Error sending request: %s", err)
		return err
	}

	// Parse the JSON response into a struct
	var postResponse DbConnectResponse

	if err := json.Unmarshal(responseBytes, &postResponse); err != nil {
		log.Errorf("Error decoding JSON response: %s", err)
		return err
	}

	log.Infof("Connected to  server: %s", postResponse.Message)

	return nil
}

func (cl *JdbcClient) GetDbInfo(dbName string) (*types.DatabaseInfoData, error) {
	log.Infof("Getting database info for: %s", dbName)
	if dbName == "" {
		// if no database name is specified (ex. there is no database name for given datasource), use the default database
		dbName = "_defaultdb_"
	}
	database := types.DatabaseInfoData{
		DbName:  dbName,
		Schemas: []types.Schema{},
	}

	responseBytes, err := cl.sendRequest("api/dbanalyzer/dbschemas", nil)
	if err != nil {
		log.Errorf("Error getting schema info: %s", err)
		return nil, err
	}

	// Parse the JSON response into a struct
	var postResponse DbSchemasResponse

	if err := json.Unmarshal(responseBytes, &postResponse); err != nil {
		log.Errorf("Error decoding JSON response: %s", err)
		return nil, err
	}

	log.Infof("Got schemas: %s", postResponse.Schemas)

	if len(postResponse.Schemas) > 0 {
		for _, schema := range postResponse.Schemas {
			log.Debugf("Adding schema: %s", schema)
			database.Schemas = append(database.Schemas, types.Schema{
				Name:     schema,
				IsSystem: false, // TODO: add support for system schemas
				Tables:   []types.Table{},
			})
		}
	} else {
		// Database has no schemas, so we create a single schema with the database name
		log.Debugf("Adding schema (default): %s", dbName)
		database.Schemas = append(database.Schemas, types.Schema{
			Name:     dbName,
			IsSystem: false, // TODO: add support for system schemas
			Tables:   []types.Table{},
		})
	}

	for currentSchema, schema := range database.Schemas {
		reqURL := fmt.Sprintf("api/dbanalyzer/dbtables")

		sch := schema.Name
		if cl.SourceType == "s3" {
			sch = url.PathEscape(schema.Name)
		}

		if schema.Name != "_defaultdb_" && schema.Name != "/" {
			reqURL = fmt.Sprintf("%s/%s", reqURL, sch)
		}

		log.Infof("Getting tables for schema: %s", sch)
		responseBytes, err := cl.sendRequest(reqURL, nil)
		if err != nil {
			log.Errorf("Error getting table info: %s", err)
			return nil, err
		}

		// Parse the JSON response into a struct
		var postResponse DbTabListResponse

		if err := json.Unmarshal(responseBytes, &postResponse); err != nil {
			log.Errorf("Error decoding JSON response: %s", err)
			return nil, err
		}

		log.Debugf("Got tables: %s", postResponse.Tables)

		for _, table := range postResponse.Tables {
			database.Schemas[currentSchema].Tables = append(database.Schemas[currentSchema].Tables, types.Table{
				Name:     table,
				IsSystem: cl.isSystemTable(table), // TODO: add support for system tables
			})
		}
		log.Debugf("Added tables: %v", database.Schemas[currentSchema].Tables)
	}
	log.Debugf("Got database info: %v", database.DbName)
	return &database, nil
}

func (cl *JdbcClient) isSystemTable(table string) bool {
	//TODO: add support for different system tables
	return strings.HasPrefix(table, "pg_") ||
		strings.HasPrefix(table, "sql_") ||
		strings.HasPrefix(table, "information_schema.") ||
		strings.HasPrefix(table, "sys.") ||
		strings.HasPrefix(table, "sys_") ||
		strings.HasPrefix(table, ".")
}

func (cl *JdbcClient) GetTblInfo(dbName string, tip *types.TableInfoParams) (*types.TableInfoData, error) {
	log.Infof("Getting columns for table: %s", tip.Table)

	sch := tip.Schema
	tbl := tip.Table
	if cl.SourceType == "s3" {
		if tip.Schema == "" || tip.Schema == "/" {
			sch = ""
		} else {
			sch = url.PathEscape(tip.Schema)
		}
		tbl = url.PathEscape(tip.Table)
	}

	reqURL := fmt.Sprintf("api/dbanalyzer/dbcolumns")
	if tip.Schema != "" && tip.Schema != "_defaultdb_" && tip.Schema != "/" {
		reqURL = fmt.Sprintf("%s/%s", reqURL, sch)
	}

	reqURL = fmt.Sprintf("%s/%s", reqURL, tbl)
	if cl.SourceType == "s3" {
		reqURL = fmt.Sprintf("%s?samplesize=%d", reqURL, detect.SampleSize)
	}

	responseBytes, err := cl.sendRequest(reqURL, nil)
	if err != nil {
		log.Errorf("Error getting columns info: %s", err)
		return nil, err
	}

	// Parse the JSON response into a struct
	var postResponse DbColListResponse
	if err := json.Unmarshal(responseBytes, &postResponse); err != nil {
		log.Errorf("Error decoding JSON response: %s", err)
		return nil, err
	}

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
		cTypName        string
		cLength, cScale *int
	}

	descr := []*data{}

	for _, columnRaw := range postResponse.Columns {
		var column DbColumnInfo
		err := json.Unmarshal(columnRaw, &column)
		if err != nil {
			log.Errorf("Error decoding JSON response: %s", err)
			return nil, err
		}

		var d data
		var isNullable_ string
		d.pos = column.OrdinalPosition
		d.cName = column.ColumnName
		d.cTyp = column.DataTypeStr
		d.cTypName = column.TypeName
		d.cLength = &column.ColumnSize
		d.cScale = &column.DecimalDigits
		isNullable_ = column.IsNullable
		// In case of Elasticsearch, we consider all columns as nullable
		if cl.SourceType == "s3" || cl.SourceType == "elasticsearch" || cl.SourceType == "es" || isNullable_ == "YES" || isNullable_ == "yes" || isNullable_ == "Y" || isNullable_ == "y" {
			d.isNullable = true
		} else {
			d.isNullable = false
		}
		log.Debugf("Got description for %s: %v", d.cName, d)
		descr = append(descr, &d)
	}

	log.Debugf("Got columns description: %v", descr)

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
		log.Debugf("Processing column %s, type %s", d.cName, d.cTyp)
		if cl.SourceType == "s3" && (strings.HasSuffix(tbl, ".csv") || strings.HasSuffix(tbl, ".json") || strings.HasSuffix(tbl, ".jsonl")) {
			// TODO: currently fdw for S3 supports mapping for csv and json fields to text only
			t = "text"
			possibleActions = obfuscatable
			sample[k] = dtk(true)
		} else {
			switch strings.ToUpper(d.cTyp) {
			case "ARRAY":
				// FIXME: we need to get the element type
				t = d.cTyp
				possibleActions = blocked
				sem = utils.Unsupported
				sample[k] = dtk(false, sem)
			case "BIGINT", "INT64TYPE", "INT64":
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
				//Currently, we don't support BLOB, BINARY, VARBINARY (for Elasticsearch)
				if cl.SourceType == "elasticsearch" || cl.SourceType == "es" {
					t = "bytea"
					possibleActions = blocked
					sem = utils.Unsupported
					sample[k] = dtk(false, sem)
				} else {
					if cl.SourceType == "s3" {
						t = "text"
						possibleActions = obfuscatable
						sample[k] = dtk(true)
					} else {
						if d.cTypName == "STRING" {
							t = "text"
							sample[k] = dtk(true)
						} else {
							t = "bytea"
							sample[k] = dtk(false)
						}
					}
				}
			case "BOOLEAN", "BOOLEANTYPE":
				t = "boolean"
				possibleActions = allowable
				sample[k] = dtk(true)
			case "CHAR", "INT8TYPE", "INT8":
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
			case "DATE", "DATE64TYPE", "DATE64":
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
			case "DOUBLE", "NUMBER", "FLOAT64TYPE", "FLOAT64":
				t = "double precision"
				possibleActions = allowable
				sample[k] = dtk(true)
			case "FLOAT", "REAL", "FLOAT32TYPE", "FLOAT32":
				if d.cLength != nil && *d.cLength > 6 {
					t = "double precision"
				} else {
					t = "float4"
				}
				possibleActions = allowable
				sample[k] = dtk(true)
			case "INTEGER", "INT32TYPE", "INT32":
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
				if d.cLength != nil && *d.cLength > 0 && *d.cLength < 65536 {
					t = "varchar"
					//t = fmt.Sprintf("varchar(%d)", *d.cLength)
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
				if d.cLength != nil && *d.cLength > 0 && *d.cLength < 65536 {
					//t = fmt.Sprintf("varchar(%d)", *d.cLength)
					t = "varchar"
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
				if d.cLength != nil && *d.cLength > 0 && *d.cLength < 65536 {
					t = "varchar"
					//t = fmt.Sprintf("varchar(%d)", *d.cLength)
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
			case "NVARCHAR", "STRING", "JSON", "BYTEARRAYTYPE", "BYTEARRAY":
				//Identifies the generic SQL type NVARCHAR.
				// TODO: check if this is correct
				if d.cLength != nil && *d.cLength > 0 && *d.cLength < 65536 {
					t = "varchar"
					//t = fmt.Sprintf("varchar(%d)", *d.cLength)
				} else {
					t = "text"
				}
				possibleActions = obfuscatable
				sample[k] = dtk(true)
			case "OTHER", "NONE":
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
			case "SMALLINT", "INT16TYPE", "INT16":
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
				if d.cLength != nil && *d.cLength > 0 && *d.cLength < 6 {
					t = fmt.Sprintf("time(%d) with time zone", *d.cLength)
				} else {
					t = "time with time zone"
				}
				possibleActions = allowable
				sample[k] = dtk(true)
			case "TIMESTAMP_WITH_TIMEZONE":
				if d.cLength != nil && *d.cLength > 0 && *d.cLength < 6 {
					t = fmt.Sprintf("timestamp(%d) with time zone", *d.cLength)
				} else {
					t = "timestamp with time zone"
				}
				possibleActions = allowable
				sample[k] = dtk(true)
			case "TIMESTAMP", "TIMESTAMPNANOSECONDTYPE", "TIMESTAMPNANOSECOND":
				if d.cScale != nil && *d.cScale > 0 {
					t = fmt.Sprintf("timestamp (%d) with time zone", *d.cScale)
				} else {
					t = "timestamp"
				}
				possibleActions = allowable
				sample[k] = dtk(true)
			case "TINYINT":
				// TODO: check if this is correct
				t = "smallint"
				possibleActions = allowable
				sample[k] = dtk(true)
			case "VARCHAR":
				if d.cLength != nil && *d.cLength > 0 && *d.cLength < 65536 {
					t = "varchar"
					//t = fmt.Sprintf("varchar(%d)", *d.cLength)
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

	log.Debugf("Got table info: %v", ti.TblName)

	log.Debugf("Sampling the table: %s", tip.Table)
	if err = cl.getSample(sch, tbl, sample); err != nil {
		log.Errorf("Error getting sample: %s", err)
		return nil, err
	}

	log.Debugf("Detecting semantics for the table: %s", tip.Table)
	if err = detectors.FindSemantics(sample); err != nil {
		log.Errorf("Error detecting semantics: %s", err)
		return nil, err
	}

	for k := range ti.Columns {
		if sample[k].Semantics != nil {
			ti.Columns[k].Semantics = sample[k].Semantics
		}
	}

	return &ti, nil
}

func (cl *JdbcClient) getSample(schema string, table string, sample []detect.Sample) error {
	log.Infof("Getting sample for schema: %s, table: %s", schema, table)
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
	var reqUrl = fmt.Sprintf("api/dbanalyzer/dbsample?table=%s&samplesize=%d", table, detect.SampleSize)
	if schema != "" && schema != "_defaultdb_" {
		reqUrl = fmt.Sprintf("%s&schema=%s", reqUrl, schema)
	}

	var optionalParams = make(map[string]string)
	if colNames.Len() > 0 {
		optionalParams["columns"] = colNames.String()
	}
	log.Infof("Getting sample for schema: %s, table: %s, sample: %d", schema, table, detect.SampleSize)
	responseBytes, err := cl.sendRequest(reqUrl, optionalParams)
	if err != nil {
		log.Errorf("Error getting schema info: %s", err)
		return err
	}

	// Parse the JSON response into a struct
	var postResponse DbSampleResponse
	if err := json.Unmarshal(responseBytes, &postResponse); err != nil {
		log.Errorf("Error decoding JSON response: %s", err)
		return err
	}

	rows := postResponse.Data
	for _, rowRaw := range rows {
		var row map[string]interface{}
		err := json.Unmarshal(rowRaw, &row)
		if err != nil {
			log.Errorf("Error decoding JSON response: %s", err)
			return err
		}
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
