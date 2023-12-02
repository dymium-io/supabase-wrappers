package types
import "github.com/golang-jwt/jwt"

const RoleUser = "user"
const RoleAdmin = "admin"

// This is a duplication from webserver. 
// Need to think how to make this right
type Claims struct {
	Name       string`json:"name"`
	Email       string`json:"email"`
	Groups     []string `json:"groups"`
	Roles     []string `json:"roles"`	
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
