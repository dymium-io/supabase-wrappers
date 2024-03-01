package authentication

import (
	"bytes"
	"crypto/rand"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"image"
	"io"
	"net/http"
	"os"
	"strings"

	"dymium.com/dymium/log"
	"dymium.com/dymium/types"
	"github.com/Jeffail/gabs"
)

func TestNameAndLogo(shortname, name, logo string) error {
	// see if the name is already in use in the global.customers table
	sql := `select count(*) from global.customers where schema_name=$1;`
	row := db.QueryRow(sql, shortname)
	var count int
	err := row.Scan(&count)
	if err != nil {
		log.Errorf("TestNameAndLogo error: %s", err.Error())
		return err
	}
	if count > 0 {
		return errors.New("Short name already in use")
	}
	sql = `select count(*) from global.customers where company_name=$1;`
	row = db.QueryRow(sql, name)
	err = row.Scan(&count)
	if err != nil {
		log.Errorf("TestNameAndLogo error: %s", err.Error())
		return err
	}
	if count > 0 {
		return errors.New("Name already in use")
	}

	// now check if the logo is a real URL that contains an image
	resp, err := http.Get(logo)
	if err != nil {
		log.Errorf("TestNameAndLogo error: %s", err.Error())
		return err
	}
	defer resp.Body.Close()
	if resp.StatusCode != 200 {
		return errors.New("Logo URL not found")
	}
	// check if it is an image
	_, _, err = image.DecodeConfig(resp.Body)
	if err != nil {
		return errors.New("Logo URL not an image")
	}

	return nil
}

func getToken() (string, error) {
	var body []byte
	payload :=
		fmt.Sprintf(
			`{
		"client_id":"%s",
		"client_secret":"%s",
		"audience":"%s",
		"grant_type":"client_credentials",
		"scope":"read:client_grants create:client_grants delete:client_grants update:client_grants read:users update:users delete:users create:users read:users_app_metadata update:users_app_metadata delete:users_app_metadata create:users_app_metadata read:user_custom_blocks create:user_custom_blocks delete:user_custom_blocks create:user_tickets read:clients update:clients delete:clients create:clients read:client_keys update:client_keys delete:client_keys create:client_keys read:connections update:connections delete:connections create:connections read:resource_servers update:resource_servers delete:resource_servers create:resource_servers read:device_credentials update:device_credentials delete:device_credentials create:device_credentials read:rules update:rules delete:rules create:rules read:rules_configs update:rules_configs delete:rules_configs read:hooks update:hooks delete:hooks create:hooks read:actions update:actions delete:actions create:actions read:email_provider update:email_provider delete:email_provider create:email_provider blacklist:tokens read:stats read:insights read:tenant_settings update:tenant_settings read:logs read:logs_users read:shields create:shields update:shields delete:shields read:anomaly_blocks delete:anomaly_blocks update:triggers read:triggers read:grants delete:grants read:guardian_factors update:guardian_factors read:guardian_enrollments delete:guardian_enrollments create:guardian_enrollment_tickets read:user_idp_tokens create:passwords_checking_job delete:passwords_checking_job read:custom_domains delete:custom_domains create:custom_domains update:custom_domains read:email_templates create:email_templates update:email_templates read:mfa_policies update:mfa_policies read:roles create:roles delete:roles update:roles read:prompts update:prompts read:branding update:branding delete:branding read:log_streams create:log_streams delete:log_streams update:log_streams create:signing_keys read:signing_keys update:signing_keys read:limits update:limits create:role_members read:role_members delete:role_members read:entitlements read:attack_protection update:attack_protection read:organizations_summary create:authentication_methods read:authentication_methods update:authentication_methods delete:authentication_methods read:organizations update:organizations create:organizations delete:organizations create:organization_members read:organization_members delete:organization_members create:organization_connections read:organization_connections update:organization_connections delete:organization_connections create:organization_member_roles read:organization_member_roles delete:organization_member_roles create:organization_invitations read:organization_invitations delete:organization_invitations delete:phone_providers create:phone_providers read:phone_providers update:phone_providers delete:phone_templates create:phone_templates read:phone_templates update:phone_templates create:encryption_keys read:encryption_keys update:encryption_keys delete:encryption_keys read:sessions delete:sessions read:refresh_tokens delete:refresh_tokens read:client_credentials create:client_credentials update:client_credentials delete:client_credentials"
	}`, auth_api_client_id, auth_api_client_secret, auth_api_audience)
	// get the token
	log.Infof("CreateNewTenant, payload: %s", payload)
	log.Infof("CreateNewTenant, url: %s", auth_api_domain+"oauth/token")
	resp, err := http.Post(auth_api_domain+"oauth/token", "application/json", bytes.NewBuffer([]byte(payload)))
	if err != nil {
		return "", err
	}
	defer resp.Body.Close()
	if resp.StatusCode != 200 {
		body, _ = io.ReadAll(resp.Body)
		return "", errors.New("Auth0 token retrieval failed")
	}
	body, err = io.ReadAll(resp.Body)
	if err != nil {
		return "", err
	}
	// parse the token
	outt := struct {
		Access_token string `json:"access_token"`
	}{}
	err = json.Unmarshal(body, &outt)
	if err != nil {
		return "", err
	}
	access_token := outt.Access_token
	return access_token, nil
}

func getParsedConfig(schema string) (string, string, string, string, string, string, string, string, string, []string, error) {
	// retrieve the json from the schema
	sql := `select config from global.invitations where id=$1;`
	row := db.QueryRow(sql, schema)
	var body []byte
	err := row.Scan(&body)
	if err != nil {
		return "", "", "", "", "", "", "", "", "", []string{}, err
	}

	jsonParsed, err := gabs.ParseJSON(body)
	logo := jsonParsed.Path("logo").Data().(string)
	name := jsonParsed.Path("name").Data().(string)
	shortname := jsonParsed.Path("shortname").Data().(string)
	domain := jsonParsed.Path("domain").Data().(string)
	clientid := jsonParsed.Path("clientid").Data().(string)
	secret := jsonParsed.Path("secret").Data().(string)
	issuer := jsonParsed.Path("issuer").Data().(string)
	fore := jsonParsed.Path("fore").Data().(string)
	back := jsonParsed.Path("back").Data().(string)
	var admins []string
	// For the slice of strings
	adminsInterface, ok := jsonParsed.Path("admins").Data().([]interface{})
	if !ok {
		// handle error
	} else {

		for _, v := range adminsInterface {
			str, ok := v.(string)
			if !ok {
				// handle error
				continue
			}
			admins = append(admins, str)
		}
	}
	return logo, name, shortname, domain, clientid, secret, issuer, fore, back, admins, nil
}

type IssuerDetails struct {
	Issuer                 string
	Authorization_endpoint string
	Token_endpoint         string
	Userinfo_endpoint      string
	Jwks_uri               string
}

func retrieveIssuer(issuer string) (*IssuerDetails, error) {
	// hit the issuer URL
	client := &http.Client{}
	urlStr := fmt.Sprintf("%s.well-known/openid-configuration", issuer)

	nr, err := http.NewRequest(http.MethodGet, urlStr, nil) // URL-encoded payload
	if err != nil {
		return nil, err
	}
	nr.Header.Add("Content-Type", "application/x-www-form-urlencoded")
	resp, err := client.Do(nr)
	if err != nil {
		return nil, err
	}

	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}

	var o IssuerDetails

	err = json.Unmarshal(body, &o)
	if err != nil {
		return nil, err
	}
	return &o, nil
}

func createConnection(o *IssuerDetails, shortname, issuer, clientid, secret, access_token string) (string, error) {
	// create the connection
	payload := fmt.Sprintf(`{"name":"%s", "strategy":"oidc", "options":{ "type": "back_channel", "discovery_url":"%s.well-known/openid-configuration","token_endpoint": "%s", "userinfo_endpoint": "%s","response_mode": "form_post",  
"token_endpoint_auth_method":"client_secret_post", "response_type":"code", "jwks_uri":"%s", "authorization_endpoint":"%s",  "client_id":"%s", "client_secret":"%s", "issuer":"%s", "scope":"openid profile email" }, "is_domain_connection": false, "show_as_button": false}`, shortname, issuer, o.Token_endpoint, o.Userinfo_endpoint, o.Jwks_uri, o.Authorization_endpoint, clientid, secret, o.Issuer)
	log.Infof("CreateNewTenant, payload: %s", payload)
	nr, err := http.NewRequest(http.MethodPost, auth_api_domain+"api/v2/connections", bytes.NewBuffer([]byte(payload))) // URL-encoded payload
	if err != nil {
		return "", err
	}
	nr.Header.Add("Content-Type", "application/json")
	nr.Header.Add("Authorization", "Bearer "+access_token)
	client := &http.Client{}
	resp, err := client.Do(nr)
	if err != nil {
		return "", err
	}

	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return "", err
	}
	log.Infof("CreateNewTenant, body: %s", string(body))
	jsonParsed, err := gabs.ParseJSON(body)

	rr := jsonParsed.Path("error").Data()
	if rr != nil {
		serror := jsonParsed.Path("error").Data().(string)
		message := jsonParsed.Path("message").Data().(string)

		if serror != "" {
			return "", errors.New(message)
		}
	}

	con_id := jsonParsed.Path("id").Data().(string)
	return con_id, nil
}

func createOrganization(access_token, shortname, name, logo, back, fore, con_id string) (string, error) {
	payload := fmt.Sprintf(`{"name":"%s","display_name":"%s","branding":{"logo_url":"%s","colors":{"primary":"%s","page_background":"%s"}},"metadata":{}, "enabled_connections": [{"connection_id": "%s", "assign_membership_on_login": true, "show_as_button": true}]}`,
		shortname, name, logo, back, fore, con_id)

	nr, err := http.NewRequest(http.MethodPost, auth_api_domain+"api/v2/organizations", bytes.NewBuffer([]byte(payload))) // URL-encoded payload
	if err != nil {
		return "", err
	}
	nr.Header.Add("Content-Type", "application/json")
	nr.Header.Add("Authorization", "Bearer "+access_token)
	client := &http.Client{}
	resp, err := client.Do(nr)
	if err != nil {
		return "", err
	}

	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		log.Errorf("CreateNewTenant error: %s", err.Error())
		return "", err
	}
	log.Infof("CreateNewTenant, body: %s", string(body))
	jsonParsed, err := gabs.ParseJSON(body)
	rr := jsonParsed.Path("error").Data()
	if rr != nil {
		serror := jsonParsed.Path("error").Data().(string)
		message := jsonParsed.Path("message").Data().(string)

		if serror != "" {
			return "", errors.New(message)
		}
	}

	org_id := jsonParsed.Path("id").Data().(string)

	return org_id, nil
}

func updateStatus(schema string, status []string) error {
	// Marshall the output
	b, err := json.Marshal(status)
	if err != nil {
		log.Errorf("CreateNewTenant/marshall error: %s", err.Error())
		return err
	}

	sql := `update global.invitations set status=$1 where id=$2;`
	_, err = db.Exec(sql, b, schema)
	if err != nil {
		log.Errorf("updateStatus error: %s", err.Error())
		return err
	}

	return nil
}

func populateSuperAdmin(shortname string, admins []string) error {
	// add username to the superadmins table in schema .shortname
	// rewrite the sql to encrypt username, and also add  sha256 hash of the username to username_hash
	// generate a hex AES256 key for encrypting usernames
	key := make([]byte, 32) // AES-256 keys are 32 bytes long
	_, err := rand.Read(key)
	if err != nil {
		return err
	}

	name := strings.ToUpper(shortname) + "_KEY"
	hexkey, err := GetSecret(name)
	if err != nil {
		hexkey = hex.EncodeToString(key)
		CreateSecret(name, hexkey)
		os.Setenv(name, hexkey)
	}

	sql := `insert into %s.superadmins (username, username_hash) values ($1, $2);`
	sql = fmt.Sprintf(sql, shortname)
	for _, admin := range admins {
		h := sha256.New()
		h.Write([]byte(admin))
		hashedAdmin := hex.EncodeToString(h.Sum(nil))

		enc, err := AESencrypt([]byte(admin), hexkey)
		if err != nil {
			return err
		}
		_, err = db.Exec(sql, enc, hashedAdmin)
		if err != nil {
			return err
		}
	}
	return nil
}
func CreateNewTenant(schema string) error {
	// retrieve the json from the schema
	var out []string

	updateStatus(schema, out)

	logo, name, shortname, domain, clientid, secret, issuer, fore, back, admins, err := getParsedConfig(schema)
	if err != nil {
		log.Errorf("CreateNewTenant/getParsedConfig error: %s", err.Error())
		out = append(out, "Error: "+err.Error())
		updateStatus(schema, out)
		return err
	}

	log.Infof("CreateNewTenant, logo: %s, name: %s, shortname: %s, domain: %s, clientid: %s, secret: %s, issuer: %s", logo, name, shortname, domain, clientid, secret, issuer)

	access_token, err := getToken()
	if err != nil {
		log.Errorf("CreateNewTenant/getToken error: %s", err.Error())
		out = append(out, "Error: "+err.Error())
		updateStatus(schema, out)
		return err
	}
	out = append(out, "Auth0 Access token obtained")
	updateStatus(schema, out)
	log.Infof("CreateNewTenant, access_token: %s", access_token)

	if issuer[len(issuer)-1] != '/' {
		issuer = issuer + "/"
	}
	o, err := retrieveIssuer(issuer)
	if err != nil {
		log.Errorf("CreateNewTenant/retrieveIssuer error: %s", err.Error())
		out = append(out, "Error: "+err.Error())
		updateStatus(schema, out)
		return err
	}
	out = append(out, "IdP issuer information retrieved")
	out = append(out, "Start integration with Auth0...")
	updateStatus(schema, out)
	con_id, err := createConnection(o, shortname, issuer, clientid, secret, access_token)
	if err != nil {
		out = append(out, "Error: "+err.Error())
		updateStatus(schema, out)
		log.Errorf("CreateNewTenant/createConnection error: %s", err.Error())
		return err
	}
	out = append(out, "Auth0 Connection created")
	updateStatus(schema, out)

	org_id, err := createOrganization(access_token, shortname, name, logo, back, fore, con_id)

	if err != nil {
		log.Errorf("CreateNewTenant/createOrganization error: %s", err.Error())
		out = append(out, "Error: "+err.Error())
		updateStatus(schema, out)
		return err
	}
	out = append(out, "Auth0 Organization created")
	out = append(out, "Populating configuration database...")
	updateStatus(schema, out)

	customer := types.Customer{
		Name:       name,
		Orgid:      org_id,
		Schema:     shortname,
		Domain:     domain,
		Admingroup: "",
	}
	err = CreateNewCustomer(customer)
	if err != nil {
		log.Errorf("CreateNewTenant/CreateNewCustomer error: %s", err.Error())
		out = append(out, "Error: "+err.Error())
		updateStatus(schema, out)
		return err
	}
	out = append(out, "Database configuration created")
	updateStatus(schema, out)

	err = populateSuperAdmin(shortname, admins)
	if err != nil {
		log.Errorf("CreateNewTenant/populateSuperAdmin error: %s", err.Error())
		out = append(out, "Error: "+err.Error())
		updateStatus(schema, out)
		return err
	}
	out = append(out, "Success!")
	updateStatus(schema, out)
	sql := 	`update global.invitations set progress='Completed' where id=$1;`
	_, _ = db.Exec(sql, schema)
	return nil
}

func StartCreatingFootprint(schema string) error {
	log.Infof("Creating footprint for schema: %s", schema)
	go CreateNewTenant(schema)
	return nil
}

func CreatingFootprintStatus(schema string) ([]byte, error) {
	// retrieve the status from the invitations table
	sql := `select status from global.invitations where id=$1;`
	row := db.QueryRow(sql, schema)
	var body []byte
	err := row.Scan(&body)
	if err != nil {
		return []byte{}, err
	}
	return body, nil
}

func queryConnection(access_token, shortname string) (string, error) {
	// get Auth0 connections
	nr, err := http.NewRequest(http.MethodGet, auth_api_domain+"api/v2/connections", nil) // URL-encoded payload
	if err != nil {
		return "", err
	}
	nr.Header.Add("Content-Type", "application/json")
	nr.Header.Add("Authorization", "Bearer "+access_token)
	client := &http.Client{}
	resp, err := client.Do(nr)
	if err != nil {
		return "", err
	}

	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return "", err
	}

	jsonParsed, err := gabs.ParseJSON(body)

	con_id := ""

	// Directly iterate over the children if the root is an array.
	children, err := jsonParsed.Children()
	if err != nil {
		return "", err
	}

	for _, child := range children {
		// Safely extract the 'id' and 'name' values.
		id, ok := child.Path("id").Data().(string)
		if !ok {
			continue // or handle error
		}
		name, ok := child.Path("name").Data().(string)
		if !ok {
			continue // or handle error
		}
		if name == shortname {
			con_id = id
			break
		}
	}
	return con_id, nil
}
func deleteConnection(access_token, shortname string) error {
	con_id, err := queryConnection(access_token, shortname)
	if err != nil {
		return err
	}

	if con_id != "" {
		nr, err := http.NewRequest(http.MethodDelete, auth_api_domain+"api/v2/connections/"+con_id, nil) // URL-encoded payload
		if err != nil {
			return err
		}
		nr.Header.Add("Content-Type", "application/json")
		nr.Header.Add("Authorization", "Bearer "+access_token)
		client := &http.Client{}
		resp, err := client.Do(nr)
		if err != nil {
			return err
		}

		defer resp.Body.Close()
		body, err := io.ReadAll(resp.Body)
		log.Infof("deleteConnection, body: %s", string(body))
	}
	return nil
}

func queryOrganization(access_token, shortname string) (string, error) {
	// get organizations
	nr, err := http.NewRequest(http.MethodGet, auth_api_domain+"api/v2/organizations", nil) // URL-encoded payload
	if err != nil {
		return "", err
	}
	nr.Header.Add("Content-Type", "application/json")
	nr.Header.Add("Authorization", "Bearer "+access_token)
	client := &http.Client{}
	resp, err := client.Do(nr)
	if err != nil {
		return "", err
	}

	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return "", err
	}

	jsonParsed, err := gabs.ParseJSON(body)
	children, err := jsonParsed.Children()
	if err != nil {
		return "", err
	}
	org_id := ""
	for _, child := range children {
		// Safely extract the 'id' and 'name' values.
		id, ok := child.Path("id").Data().(string)
		if !ok {
			continue // or handle error
		}
		name, ok := child.Path("name").Data().(string)
		if !ok {
			continue // or handle error
		}
		if name == shortname {
			org_id = id
			break
		}
	}
	return org_id, nil
}
func deleteOrganization(access_token, shortname string) error {
	org_id, err := queryOrganization(access_token, shortname)
	if err != nil {
		return err
	}	
	if org_id != "" {
		nr, err := http.NewRequest(http.MethodDelete, auth_api_domain+"api/v2/organizations/"+org_id, nil) // URL-encoded payload
		if err != nil {
			return err
		}
		nr.Header.Add("Content-Type", "application/json")
		nr.Header.Add("Authorization", "Bearer "+access_token)
		client := &http.Client{}
		resp, err := client.Do(nr)
		if err != nil {
			return err
		}

		defer resp.Body.Close()
		body, err := io.ReadAll(resp.Body)
		log.Infof("deleteOrganization, body: %s", string(body))
	}
	return nil
}

func ClearResetTenantFromInvite(schema string) error {
	// delete the schema from the database
	// get the ord id from the schema
	//logo, name, shortname, domain, clientid, secret, issuer, fore, back, admins, err := getParsedConfig(schema)
	_, _, shortname, _, _, _, _, _, _, _, err := getParsedConfig(schema)
	access_token, err := getToken()
	if err != nil {
		log.Errorf("ClearResetTenantFromInvite/getToken error: %s", err.Error())
		return err
	}
	err = deleteConnection(access_token, shortname)
	err = deleteOrganization(access_token, shortname)

	// delete the schema from the global.customers table
	err = DeleteCustomer("", shortname)
	sql := `delete from global.customers where schema_name=$1;`
	_, err = db.Exec(sql, shortname)

	return nil
}

func CheckTenantInvitationStatus(schema string) (bool, error) {
	_, _, shortname, _, _, _, _, _, _, _, err := getParsedConfig(schema)
	if err != nil {
		return false, err
	}

	access_token, err := getToken()
	if err != nil {
		return false, err
	}
	org_id, err := queryOrganization(access_token, shortname)
	if org_id == "" {
		return false, err
	}	
	con_id, err := queryConnection(access_token, shortname)
	if con_id == "" {
		return false, err
	}
	// check the records in the global.customers table
	sql := `select count(*) from global.customers where schema_name=$1;`
	row := db.QueryRow(sql, shortname)
	var count int
	err = row.Scan(&count)
	if count == 0 {
		return false, err
	}
	// check the records in the <customer>.superadmins table
	sql = `select count(*) from %s.superadmins;`
	sql = fmt.Sprintf(sql, shortname)
	row = db.QueryRow(sql)
	err = row.Scan(&count)
	if count == 0 {
		return false, err
	}

	return true, nil
}

func GetInvitations() ([]byte, error) {
	var ret []types.Invitation
	sql := `select id, name, email, issued, progress from global.invitations;`
	rows, err := db.Query(sql)
	if nil == err {
		defer rows.Close()

		for rows.Next() {
			var id, name, email, issued, status string
		
			err = rows.Scan(&id, &name, &email, &issued, &status)
			if err != nil {
				log.Errorf("GetInvitations error: %s", err.Error())
				break
			}
			i := types.Invitation{Id:id, ContactName: name, Email:email, Created: issued,
				Status: status}
			ret = append(ret, i)
		}
	} else {
		return []byte{}, err
	}
	// convert to json
	js, err := json.Marshal(ret)
	log.Infof("GetInvitations, js: %s", string(js)	)
	return js, err
}

func DeleteInvitation(id string) error {
	sql := `delete from global.invitations where id=$1;`
	_, err := db.Exec(sql, id)
	return err
}

func ReissueInvitation(id string) error {
	sql := `update global.invitations set issued=now(), progress='Issued' where id=$1;`
	_, err := db.Exec(sql, id)
	if err != nil {
		return err
	}
	sql  = `select email, name from global.invitations where id=$1;`
	row := db.QueryRow(sql, id)
	var email, name string
	err = row.Scan(&email, &name)

	// resend the email
	InviteCustomerById(id, email, name ) 
	return err
}