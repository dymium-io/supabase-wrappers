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

type DataSemantics string
const (
  DS_FamilyName DataSemantics = "FamilyName"
  DS_Email DataSemantics = "Email"
  DS_SSN DataSemantics = "SSN"
)

type AuthStatus struct {
   Status string `json:"status"`
   Errormessage string `json:"errormessage"`
   Token string `json:"token"`
}

type ConnectionRecord struct {
   Name string `json:"name"`
   Dbtype string `json:"dbtype"`
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

type Datascope struct {
   Name string `json:"name"`
   Id string `json:"id"`
   Records []DatascopeRecord `json:"records"`
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
   Dflt string `json:"dflt"`
   Isnullable bool `json:"isnullable"`
}

type DatascopesStatus struct {
   Status string `json:"status"`
   Errormessage string `json:"errormessage"`
   Records []DatascopeIdName `json:"records"`
}

type GroupMapping struct {
   Id *string `json:"id"`
   Dymiumgroup string `json:"dymiumgroup"`
   Directorygroup string `json:"directorygroup"`
   Comments string `json:"comments"`
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

type Reference struct {
   Schema string `json:"schema"`
   Table string `json:"table"`
   Column string `json:"column"`
}
