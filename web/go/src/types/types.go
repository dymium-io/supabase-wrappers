package types
import "github.com/dgrijalva/jwt-go"

type Claims struct {
	Name       string`json:"name"`
	Email       string`json:"email"`
	Groups     []string `json:"groups"`
	Picture    string `json:"picture"`
	Schema     string `json:"schema"`
	Orgid      string `json:"orgid"`
	jwt.StandardClaims
}

type AdminClaims struct {
	Name       string`json:"name"`
	Email       string`json:"email"`
	Groups      []string `json:"groups"`
	Picture    string `json:"picture"`
	jwt.StandardClaims
}

type Invoke_t func(string, *string, []byte) ([]byte, error)

