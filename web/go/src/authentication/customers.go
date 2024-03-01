// Copyright (c) 2022-2024 Dymium, Inc. All rights reserved.
// written by igor@dymium.io
package authentication

import (
	"bytes"
	"dymium.com/dymium/gotypes"
	"dymium.com/dymium/log"
	"dymium.com/dymium/types"
	"errors"
	"fmt"
	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/ses"
	"github.com/golang-jwt/jwt"
	"html/template"
	"net/url"
	"os"
	"os/exec"
	"time"
)

func generateIntivationJWT(name, email, id string) (string, error) {
	// generate JWT right header
	issueTime := time.Now()
	expirationTime := issueTime.Add(7 * 24 * 60 * time.Minute)

	claim := &gotypes.InvitationClaims{
		// TODO
		Name:  name,
		Email: email,
		Invitationid: id,
		StandardClaims: jwt.StandardClaims{
			// In JWT, the expiry time is expressed as unix milliseconds
			ExpiresAt: expirationTime.Unix(),
			IssuedAt:  issueTime.Unix(),
		},
	}
	jwtKey := []byte(os.Getenv("SESSION_SECRET"))
	token := jwt.NewWithClaims(jwt.SigningMethodHS256, claim)
	// Create the JWT string
	tokenString, err := token.SignedString(jwtKey)

	return tokenString, err
}

func CreateNewCustomer(customer types.Customer) error {
	sql := `insert into global.customers(company_name, schema_name, organization, domain, admin_group)
		values($1,$2,$3,$4,$5);`
	_, err := db.Exec(sql, customer.Name, customer.Schema, customer.Orgid, customer.Domain, customer.Admingroup)
	if err != nil {
		return err
	}
	var mallard string
	var where string
	if "" == os.Getenv("LOCAL_ENVIRONMENT") {
		// cloud, hence linux
		mallard = "./mallard"
		where = "/mallard"
	} else {
		// local Mac
		mallard = "../bin/mallard"
		where = "../../../dbConf"
	}
	dbhost := os.Getenv("DATABASE_HOST")
	dbpassword := os.Getenv("DATABASE_PASSWORD")
	dbport := os.Getenv("DATABASE_PORT")
	dbuser := os.Getenv("DATABASE_USER")
	dbname := os.Getenv("DATABASE_NAME")
	if dbport == "" {
		dbport = "5432"
	}
	if dbname == "" {
		dbname = "dymium"
	}
	cmd := exec.Command(mallard, "migrate", "-s", customer.Schema, "-r", "customer",
		"--host", dbhost,
		"--port", dbport,
		"--user", dbuser,
		"--password", "'"+dbpassword+"'",
		"--database", dbname,
		"--apply")

		fmt.Printf("password: %s\n", dbpassword)
	fmt.Printf("Run %s in dir: %s, port: %s\n", mallard, where, dbport)
	fmt.Printf("args: %v\n", cmd.Args)
	cmd.Dir = where
	var out bytes.Buffer
	var stderr bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &stderr
	err = cmd.Run()
	if err != nil {
		log.Errorf("Error running mallard: %s", err.Error())
		return errors.New(fmt.Sprintf("%s, mallard output: %s", err.Error(), stderr.String()))
	} else {
		return err
	}
}
func GetCustomers() ([]types.Customer, error) {
	var ret []types.Customer
	sql := `select id, company_name, schema_name, organization, domain, admin_group from global.customers;`

	rows, err := db.Query(sql)
	if nil == err {
		defer rows.Close()

		for rows.Next() {
			var id, company_name, schema_name, organization, domain, admin_group string
			err = rows.Scan(&id, &company_name, &schema_name, &organization, &domain, &admin_group)
			if err != nil {
				break
			}
			c := types.Customer{&id, company_name, organization, schema_name, domain, admin_group}
			ret = append(ret, c)
		}
	}
	return ret, err
}

func DeleteCustomer(id string, schema string) error {
	if id == "" {
		// read
		sql := `delete from global.customers where schema_name=$1;`
		_, err := db.Exec(sql, id)
		if err != nil {
			return err
		}
	} else {
		sql := `delete from global.customers where id=$1;`
		_, err := db.Exec(sql, id)
		if err != nil {
			return err
		}
	}
	var mallard, where string

	if "" == os.Getenv("LOCAL_ENVIRONMENT") {
		// cloud, hence linux
		mallard = "./mallard"
		where = "./mallard"
	} else {
		// local Mac
		mallard = "../bin/mallard"
		where = "../../../dbConf"
	}
	dbhost := os.Getenv("DATABASE_HOST")
	dbpassword := os.Getenv("DATABASE_PASSWORD")
	dbport := os.Getenv("DATABASE_PORT")
	dbuser := os.Getenv("DATABASE_USER")
	dbname := os.Getenv("DATABASE_NAME")
	if dbport == "" {
		dbport = "5432"
	}
	if dbname == "" {
		dbname = "dymium"
	}

	cmd := exec.Command(mallard, "drop", "-s", schema,
		"--host", dbhost,
		"--port", dbport,
		"--user", dbuser,
		"--password", dbpassword,
		"--database", dbname)
	cmd.Dir = where
	var out bytes.Buffer
	var stderr bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &stderr
	err := cmd.Run()
	if err != nil {
		return errors.New(fmt.Sprintf("%s, mallard output: %s", err.Error(), stderr.String()))
	} else {
		return err
	}

}

func UpdateCustomer(customer types.Customer) error {
	sql := `update global.customers set organization=$1, domain=$2, admin_group=$3 where id=$4;`
	_, err := db.Exec(sql, customer.Orgid, customer.Domain, customer.Admingroup, customer.Id)
	if err != nil {
		return err
	}

	return err
}

func sendEmailSESHtml(to, subject, htmlBody string) error {
	// Create a new session in the us-west-2 region.
	// Ensure you have AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY set in your environment or configured in ~/.aws/credentials
	sess, err := session.NewSession(&aws.Config{
		Region: aws.String("us-west-2")},
	)

	if err != nil {
		return err
	}
	log.Debugf("To: %s\nMessage: %s\n", to, htmlBody)
	// Create an SES session.
	svc := ses.New(sess)

	// Assemble the email.
	input := &ses.SendEmailInput{
		Destination: &ses.Destination{
			ToAddresses: []*string{
				aws.String(to),
			},
		},
		Message: &ses.Message{
			Body: &ses.Body{
				Html: &ses.Content{
					Charset: aws.String("UTF-8"),
					Data:    aws.String(htmlBody),
				},
			},
			Subject: &ses.Content{
				Charset: aws.String("UTF-8"),
				Data:    aws.String(subject),
			},
		},
		Source: aws.String("Dymium <support@dymium.io>"),
	}

	// Attempt to send the email.
	_, err = svc.SendEmail(input)

	// Return error if any.
	return err
}

func InviteCustomerById(id, email, contactName string ) error {

	// create a JWT with the time, id and email
	jwt, err := generateIntivationJWT(contactName, email, id)
	if err != nil {
		log.Errorf("Error generating JWT: %s", err.Error())
		return err
	}
	// now use html template to create email body
	const emailTemplate = `
        <html>
            <body>
				<h3>Invitation to Dymium</h3>
                <p>Hello, {{.Name}}!</p>
                <div style="margin-top: 2em; margin-bottom: 2em;" >Welcome to the Dymium service! <a target="Dymium" href="{{.Domain}}/api/invitationsink?inv={{.Jwt}}">Please click on this link</a> to start configuring your corporate account
				</div>
					This invitation link is good for a week. If you have any questions, please contact us at mailto:support@dymium.com
				<div>
				</div>
<p>© 2024 Dymium, Inc. All rights reserved.
<br/>
Dymium, Inc, PO Box; 481 N.Santa Cruz Suite 300 Los Gatos CA 95030, USA.
<br/>
| <a href="https://www.dymium.io">www.dymium.io</a> | <a href="https://www.dymium.io/privacy">Privacy Policy</a> | <a href="https://www.dymium.io/terms">Terms of Service</a> |	
</p>
            </body>
        </html>
    `
	parsedURL, err := url.Parse(os.Getenv("AUTH0_PORTAL_REDIRECT_URL"))
	if err != nil {
		fmt.Println("Error parsing URL:", err)
		return err
	}
	domain := fmt.Sprintf("%s://%s", parsedURL.Scheme, parsedURL.Host)

	// Define a struct to hold template data
	data := struct {
		Name   string
		Jwt    string
		Domain string
	}{
		Name:   contactName,
		Jwt:    jwt,
		Domain: domain,
	}

	t, err := template.New("email").Parse(emailTemplate)
	if err != nil {
		log.Errorf("Error parsing template: %s", err.Error())
		return err
	}

	var buf bytes.Buffer
	if err := t.Execute(&buf, data); err != nil {
		log.Errorf("Error executing template: %s", err.Error())
		return err
	}

	personalizedEmail := buf.String()
	err = sendEmailSESHtml(email, "Welcome to Dymium", personalizedEmail)
	if err != nil {
		log.Errorf("Error sending email: %s", err.Error())
		return err
	}
	return err
}
func InviteNewCustomer(email, contactName string) error {
	// create a new customer record in global.customers
	sql := ` insert into global.invitations(email, name)
		values($1,$2) returning id;`
	var id string
	err := db.QueryRow(sql, email, contactName).Scan(&id)
	if err != nil {
		log.Errorf("Error creating seed of customer record: %s", err.Error())
		return err
	}
	return InviteCustomerById(id, email, contactName)
}

func ProcessInvitation(token string) (string, string, error) {
	// validate the JWT
	// if valid, create a new customer record in global.customers
	// if not valid, return error
	jwtKey := []byte(os.Getenv("SESSION_SECRET"))
	claim := &gotypes.InvitationClaims{}

	tkn, err := jwt.ParseWithClaims(token, claim, func(token *jwt.Token) (interface{}, error) {
		return jwtKey, nil
	})

	if nil == err {
		if !tkn.Valid {
			err = errors.New("No error, but invalid token")
			return "", "", err
		}
	} else {
		return "", "", err
	}
	// create a new token with the signer role, and redirect to the GUI portal
	newtoken, err := GeneratePortalJWT("/avatar.png", "", claim.Name, claim.Email, []string{}, []string{gotypes.RoleInitialSigner}, 
	"", 600, claim.Invitationid)

	nonce, _ := GenerateRandomString(32)
	return `<html>
	<head>
	<script nonce="` + nonce + `">
	 !function() {
		sessionStorage.setItem("Session", "` + newtoken + `")
		window.location.href = "/app/"
	 }()
	</script>
	</head>
	<body>Callback arrived</body>
	</html>`, nonce, err

}

func PostInvitationJson(body, session string) error {
	sql := `update global.invitations set config=$1, progress='In Progress' where id=$2;`
	_, err := db.Exec(sql, body, session)
	if err != nil {
		return err
	}
	return nil
}

func GetInvitationJson(session string) ( []byte, error) {
	sql := `select config from global.invitations where id=$1;`
	var config []byte
	err := db.QueryRow(sql, session).Scan(&config)
	if err != nil {
		return nil, err
	}
	return config, nil
}

func ValidateSchema(schema string) error {
	sql := `select schema_name from global.customers where schema_name=$1;`
	var schema_name string
	err := db.QueryRow(sql, schema).Scan(&schema_name)
	if err == nil {
		return fmt.Errorf("Schema %s already exists", schema)
	}
	return nil
}