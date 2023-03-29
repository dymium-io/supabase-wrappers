// this file is automatically generated.
// !!! DO NOT EDIT !!!

package types



type ConnectionType string
const (
  CT_PostgreSQL ConnectionType = "PostgreSQL"
  CT_MySQL ConnectionType = "MySQL"
  CT_MariaDB ConnectionType = "MariaDB"
  CT_SqlServer ConnectionType = "SqlServer"
  CT_OracleDB ConnectionType = "OracleDB"
)

type DataHandling string
const (
  DH_Allow DataHandling = "allow"
  DH_Block DataHandling = "block"
  DH_Obfuscate DataHandling = "obfuscate"
  DH_Redact DataHandling = "redact"
)

type PIIDetectionType string
const (
  PIIDT_Comprehend PIIDetectionType = "comprehend"
  PIIDT_Columnregexp PIIDetectionType = "columnregexp"
  PIIDT_Contentregexp PIIDetectionType = "contentregexp"
)

type AccessLevels struct {
   Levels []DataAction `json:"levels"`
}

type AuthStatus struct {
   Status string `json:"status"`
   Errormessage string `json:"errormessage"`
   Token string `json:"token"`
}

type ConnectionRecord struct {
   Name string `json:"name"`
   Dbtype string `json:"dbtype"`
   Usesconnector bool `json:"usesconnector"`
   Connectorname string `json:"connectorname"`
   Connectorid string `json:"connectorid"`
   Tunnelname string `json:"tunnelname"`
   Tunnelid string `json:"tunnelid"`
   Address string `json:"address"`
   Port int `json:"port"`
   Dbname string `json:"dbname"`
   UseTLS bool `json:"useTLS"`
   Description string `json:"description"`
   Username *string `json:"username"`
   Password *string `json:"password"`
   Id *string `json:"id"`
   Credid *string `json:"credid"`
}

type ConnectionResponse struct {
   Status string `json:"status"`
   Errormessage string `json:"errormessage"`
   Data []ConnectionRecord `json:"data"`
}

type ConnectionsQuery struct {
   Status string `json:"status"`
   Errormessage *string `json:"errormessage"`
   Records *[]ConnectionRecord `json:"records"`
}

type DataAction struct {
   Role string `json:"role"`
   Index *int `json:"index"`
   Handling DataHandling `json:"handling"`
}

type DataPolicy struct {
   Actions []DataAction `json:"actions"`
   Piisuggestions []PIISuggestor `json:"piisuggestions"`
}

type Datascope struct {
   Name string `json:"name"`
   Id *string `json:"id"`
   Records []DatascopeRecord `json:"records"`
}

type DatascopeAndGroups struct {
   Id string `json:"id"`
   Name string `json:"name"`
   Groupid string `json:"groupid"`
   Groupname string `json:"groupname"`
}

type DatascopeId struct {
   Id string `json:"id"`
}

type DatascopeIdName struct {
   Name string `json:"name"`
   Id string `json:"id"`
}

type DatascopeInfoStatus struct {
   Status string `json:"status"`
   Errormessage string `json:"errormessage"`
   Record *Datascope `json:"record"`
}

type DatascopeRecord struct {
   Id *string `json:"id"`
   Connection string `json:"connection"`
   ConnectionId *string `json:"connectionId"`
   Schema string `json:"schema"`
   Table string `json:"table"`
   Typ string `json:"typ"`
   Position int `json:"position"`
   Reference *Reference `json:"reference"`
   Action string `json:"action"`
   Col string `json:"col"`
   Semantics string `json:"semantics"`
   Dflt *string `json:"dflt"`
   Isnullable bool `json:"isnullable"`
}

type DatascopeTable struct {
   Connection string `json:"connection"`
   Datascope string `json:"datascope"`
   Schema string `json:"schema"`
   Table string `json:"table"`
}

type DatascopeTables struct {
   Status string `json:"status"`
   Errormessage string `json:"errormessage"`
   Tables []DatascopeTable `json:"tables"`
}

type DatascopesStatus struct {
   Status string `json:"status"`
   Errormessage string `json:"errormessage"`
   Records []DatascopeIdName `json:"records"`
}

type GroupAssignment struct {
   Name string `json:"name"`
   Id string `json:"id"`
   Groups []DatascopeIdName `json:"groups"`
}

type GroupMapping struct {
   Id *string `json:"id"`
   Dymiumgroup string `json:"dymiumgroup"`
   Directorygroup string `json:"directorygroup"`
   Comments string `json:"comments"`
   Adminaccess bool `json:"adminaccess"`
}

type GroupMappingStatus struct {
   Status string `json:"status"`
   Errormessage string `json:"errormessage"`
   Records []GroupMapping `json:"records"`
}

type OperationStatus struct {
   Status string `json:"status"`
   Errormessage string `json:"errormessage"`
}

type PIIDetector struct {
   Id *string `json:"id"`
   Name string `json:"name"`
   Method PIIDetectionType `json:"method"`
   Data string `json:"data"`
}

type PIISuggestor struct {
   Actions []DataAction `json:"actions"`
   Detector PIIDetector `json:"detector"`
}

type RequestById struct {
   Id string `json:"id"`
}

type TableQuery struct {
   Id string `json:"id"`
   Schema string `json:"schema"`
   Table string `json:"table"`
}

type Usage struct {
   Connections int `json:"connections"`
   Datascopes int `json:"datascopes"`
   Logins int `json:"logins"`
   Tunnels int `json:"tunnels"`
   Blocked int `json:"blocked"`
   Obfuscated int `json:"obfuscated"`
   Redacted int `json:"redacted"`
   Connectors int `json:"connectors"`
   Connectortunnels int `json:"connectortunnels"`
   Bytesin string `json:"bytesin"`
   Bytesout string `json:"bytesout"`
}

type UserDatascopes struct {
   Schema string `json:"schema"`
   Username string `json:"username"`
   Password string `json:"password"`
   Datascopes []DatascopeIdName `json:"datascopes"`
}

type Reference struct {
   Schema string `json:"schema"`
   Table string `json:"table"`
   Column string `json:"column"`
}
