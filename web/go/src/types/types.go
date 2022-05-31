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
	Orgid      string `json:"picture"`
	jwt.StandardClaims
}

type AdminClaims struct {
	Roles      []string `json:"roles"`
	Picture    string `json:"picture"`
	jwt.StandardClaims
}

type OperationStatus struct {
	Status string
	Text   string
}

type Connection struct {
	Name string`json:"name"`
	Dbtype string `json:"dbtype"`
	Address string   `json:"address"`
	Port int  `json:"port"`
	UseTLS bool  `json:"useTLS"`
	Description string  `json:"description"`
	Username string  `json:"username",omitempty`
	Password string  `json:"password",omitempty`

	Id string  `json:"id",omitempty`
	Credid  string  `json:"credid",omitempty`
}