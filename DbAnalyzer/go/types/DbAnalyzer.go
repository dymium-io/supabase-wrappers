// this file is automatically generated.
// !!! DO NOT EDIT !!!

package types



type DataType string
const (
  DT_Test DataType = "Test"
  DT_DatabaseInfo DataType = "DatabaseInfo"
  DT_TableInfo DataType = "TableInfo"
)

type AnalyzerRequest struct {
   Dtype DataType `json:"dtype"`
   Connection ConnectionParams `json:"connection"`
   TableInfo *TableInfoParams `json:"tableInfo"`
}

type AnalyzerResponse struct {
   Dtype DataType `json:"dtype"`
   DbInfo *DatabaseInfoData `json:"dbInfo"`
   TblInfo *TableInfoData `json:"tblInfo"`
}

type ConnectionDetailRequest struct {
   ConnectionId string `json:"connectionId"`
}

type ConnectionDetailResponse struct {
   Status string `json:"status"`
   Errormessage string `json:"errormessage"`
   Response *AnalyzerResponse `json:"response"`
}

type ConnectionParams struct {
   Typ ConnectionType `json:"typ"`
   Address string `json:"address"`
   Port int `json:"port"`
   User string `json:"user"`
   Password string `json:"password"`
   Database string `json:"database"`
   Tls bool `json:"tls"`
}

type DatabaseInfoData struct {
   DbName string `json:"dbName"`
   Schemas []Schema `json:"schemas"`
}

type TableInfoData struct {
   DbName string `json:"dbName"`
   Schema string `json:"schema"`
   TblName string `json:"tblName"`
   Columns []Column `json:"columns"`
}

type TableInfoParams struct {
   Schema string `json:"schema"`
   Table string `json:"table"`
   Rules []PIIDetector `json:"rules"`
}

type Column struct {
   Name string `json:"name"`
   Position int `json:"position"`
   Typ string `json:"typ"`
   IsNullable bool `json:"isNullable"`
   Default *string `json:"default"`
   Reference *Reference `json:"reference"`
   Semantics *string `json:"semantics"`
   PossibleActions []DataHandling `json:"possibleActions"`
}

type Schema struct {
   Name string `json:"name"`
   IsSystem bool `json:"isSystem"`
   Tables []Table `json:"tables"`
}

type Table struct {
   Name string `json:"name"`
   IsSystem bool `json:"isSystem"`
}
