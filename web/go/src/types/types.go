package types
import "github.com/dgrijalva/jwt-go"

type AuthStatus struct {
	Status string
	Text   string
	Token  string
}

type Claims struct {
	Roles      []string `json:"roles"`
	Picture    string `json:"picture"`
	Schema     string `json:"schema"`
	Orgid      string `json:"orgid"`
	jwt.StandardClaims
}

type AdminClaims struct {
	Roles      []string `json:"roles"`
	Picture    string `json:"picture"`
	jwt.StandardClaims
}

/*
type OperationStatus struct {
	Status string
	Text   string
}

type ConnectionRecord struct {
	Name string`json:"name"`
	Dbtype string `json:"dbtype"`
	Address string   `json:"address"`
	Port int  `json:"port"`
	Dbname string  `json:"dbname"`
	UseTLS bool  `json:"useTLS"`
	Description string  `json:"description"`
	Username string  `json:"username",omitempty`
	Password string  `json:"password",omitempty`

	Id string  `json:"id",omitempty`
	Credid  string  `json:"credid",omitempty`
}

type DatascopeRecord struct {
	Id string `json:"id",omitempty`	
	Connection string `json:"connection"`
	ConnectionId string `json:"connectionid",omitempty`	
	Schema string `json:"schema"`
	Table string `json:"table" `
	Typ string `json:"typ"`
	Position int `json:"position"`
	Reference *Reference `json:"reference"`
	Action string `json:"action"`
	Col string `json:"col"`
	Semantics string  `json:"semantics"`
	Dflt string  `json:"dflt"`
	Isnullable bool `json:"isnullable"`
}
*/


type DatascopeIdName struct {
	Name string `json:"name"`
	Id string `json:"id"`	
}
type Datascope struct {
	Name string `json:"name"`
	Id string `json:"id",omitempty`		
	Records []DatascopeRecord `json:"records"`
}

type GroupAssignment struct {
	Name string `json:"name"`
	Id string `json:"id"`	
	Groups []DatascopeIdName `json:"groups"`	
}

type DatascopeAndGroups struct {
	Id string `json:"id"`		
	Name string `json:"name"`
	Groupid string `json:"groupid"`	
	Groupname string `json:"groupname"`	
}