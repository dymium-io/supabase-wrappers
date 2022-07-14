package types
import "github.com/dgrijalva/jwt-go"

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

type Invoke_t func(string, *string, []byte) ([]byte, error)

