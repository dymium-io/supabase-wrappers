package gotypes
import "github.com/golang-jwt/jwt"

const RoleUser = "user"
const RoleAdmin = "admin"
const RoleSuperAdmin = "superadmin"
const RoleInstaller = "installer"
const RoleInitialSigner = "signer"

type Claims struct {
	Name       string`json:"name"`
	Session    string`json:"session"`
	Email      string`json:"email"`
	Groups     []string `json:"groups"`
	Roles      []string `json:"roles"`
	Picture    string `json:"picture"`
	Schema     string `json:"schema"`
	Domain	   string `json:"domain"`
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

type InvitationClaims struct {
	Name       string`json:"name"`
	Email       string`json:"email"`
	Invitationid string`json:"invitationid"`
	jwt.StandardClaims
}

type Invoke_t func(string, *string, []byte) ([]byte, error)

