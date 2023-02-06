package gotypes
import "github.com/dgrijalva/jwt-go"

const RoleUser = "user"
const RoleAdmin = "admin"
const RoleInstaller = "installer"

type Claims struct {
	Name       string`json:"name"`
	Session    string`json:"session"`
	Email      string`json:"email"`
	Groups     []string `json:"groups"`
	Roles      []string `json:"roles"`
	Picture    string `json:"picture"`
	Schema     string `json:"schema"`
	Port       int  `json:"port"`
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

