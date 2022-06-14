// this file is automatically generated.
// !!! DO NOT EDIT !!!

package types





type Column struct {
   Name string `json:"name"`
   Typ string `json:"typ"`
   IsNullable bool `json:"isNullable"`
   Semantics string `json:"semantics"`
   Action string `json:"action"`
}

type Connection struct {
   Id string `json:"id"`
   Address string `json:"address"`
   Port int `json:"port"`
   Name string `json:"name"`
   Database_type ConnectionType `json:"database_type"`
   Use_tls bool `json:"use_tls"`
   Dbname string `json:"dbname"`
}

type CustomerData struct {
   Connections []Connection `json:"connections"`
   Datascopes []Datascope `json:"datascopes"`
}

type Datascope struct {
   Name string `json:"name"`
   Connections []string `json:"connections"`
   Schemas []Schema `json:"schemas"`
}

type Schema struct {
   Name string `json:"name"`
   Tables []Table `json:"tables"`
}

type Table struct {
   Name string `json:"name"`
   Connection string `json:"connection"`
   Columns []Column `json:"columns"`
}
