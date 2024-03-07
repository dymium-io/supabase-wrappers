package certificates

import (
	"crypto/rand"
	"crypto/rsa"
	"crypto/x509"
	"crypto/x509/pkix"
	"encoding/json"
	"encoding/pem"
	"errors"
	"fmt"
	"math/big"
	"os"
	"strings"
	"time"

	"dymium.com/dymium/log"
)

// GenerateCustomerCertificate generates a new customer certificate
// based on our certificate authority, valid for 3 months
const (
	csrPEMBlockType = "CERTIFICATE REQUEST"
)

var CaKey *rsa.PrivateKey
var CaCert *x509.Certificate


func GenerateRandomBytes(n int) ([]byte, error) {
	b := make([]byte, n)
	_, err := rand.Read(b)

	if err != nil {
		return nil, err
	}

	return b, nil
}

func GenerateRandomString(n int) (string, error) {
	const letters = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-"
	ret := make([]byte, n)
	for i := 0; i < n; i++ {
		num, err := rand.Int(rand.Reader, big.NewInt(int64(len(letters))))
		if err != nil {
			return "", err
		}
		ret[i] = letters[num.Int64()]
	}
	return string(ret), nil
}

func GenerateUsernameString(n int) (string, error) {
	const letters = "0123456789abcdefghijklmnopqrstuvwxyz"
	ret := make([]byte, n)
	for i := 0; i < n; i++ {
		var num *big.Int
		var err error
		if i == 0 {
			num, err = rand.Int(rand.Reader, big.NewInt(int64(len(letters)-10)))
		} else {
			num, err = rand.Int(rand.Reader, big.NewInt(int64(len(letters))))
		}
		if err != nil {
			return "", err
		}
		if i == 0 {
			ret[i] = letters[10:][num.Int64()]
		} else {
			ret[i] = letters[num.Int64()]
		}
	}
	return string(ret), nil
}

func pemCSR(derBytes []byte) []byte {
	pemBlock := &pem.Block{
		Type:    csrPEMBlockType,
		Headers: nil,
		Bytes:   derBytes,
	}
	out := pem.EncodeToMemory(pemBlock)
	return out
}
func ParsePEMPrivateKey(pemBytes []byte, passphrase string) (*rsa.PrivateKey, error) {
	block, _ := pem.Decode(pemBytes)
	if block == nil {
		return nil, errors.New("no valid private key found")
	}

	switch block.Type {
	case "RSA PRIVATE KEY":
		var privKeyBytes []byte
		var err error

		if x509.IsEncryptedPEMBlock(block) {
			privKeyBytes, err = x509.DecryptPEMBlock(block, []byte(passphrase))
			if err != nil {
				return nil, errors.New("could not decrypt private key")
			}
		} else {
			privKeyBytes = block.Bytes
		}

		rsaPrivKey, err := x509.ParsePKCS1PrivateKey(privKeyBytes)
		if err != nil {
			return nil, fmt.Errorf("could not parse DER encoded key: %v", err)
		}

		return rsaPrivKey, nil

	default:
		return nil, fmt.Errorf("unsupported key type %q", block.Type)
	}
}
func Init() error {
	ca_cert := os.Getenv("CA_AUTHORITY")
	t := struct {
		Key         string
		Certificate string
	}{}
	err := json.Unmarshal([]byte(ca_cert), &t)
	if err != nil {
		log.Errorf("Cert unmarshaling error: %s", err.Error())
		return nil
	}

	pemcert, _ := pem.Decode([]byte(t.Certificate))

	CaCert, err = x509.ParseCertificate(pemcert.Bytes)
	if err != nil {
		log.Errorf("Cert parsing error: %s", err.Error())
		return nil
	}

	passphrase := os.Getenv("CA_PASSPHRASE")
	CaKey, err = ParsePEMPrivateKey([]byte(t.Key), passphrase)

	if err != nil {
		log.Errorf("Key parsing error: %s", err.Error())
		return nil
	}
	return nil
}
func GenerateCustomerTunnelServerCertificate(schema string) (string, string, error) {
	// our CA certificate and key are in
	// authentication.CaCert and authentication.CaKey
	// we'll generate a private key for the customer, and also a passphgrase for it

	// generate the key
	var err error
	certKey, err := rsa.GenerateKey(rand.Reader, 4096)
	if err != nil {
		return "", "", err
	}

	// Assuming this is your generated secure password
	password, _ := GenerateRandomString(64)

	// Step 2 & 3: Encrypt the RSA Private Key
	// Convert the RSA key to PKCS#8 format
	privKeyBytes, err := x509.MarshalPKCS8PrivateKey(certKey)
	if err != nil {
		return "", "", err
	}

	// Encrypt the private key with your password
	encryptedKeyBlock, err := x509.EncryptPEMBlock(rand.Reader, "RSA PRIVATE KEY", privKeyBytes, []byte(password), x509.PEMCipherAES256)
	if err != nil {
		return "", "", err
	}

	// Step 4: Encode the Encrypted Key as PEM
	pemEncoded := pem.EncodeToMemory(encryptedKeyBlock)

	host := os.Getenv("CUSTOMER_HOST")
	// remove the part before the first dot
	host = host[strings.Index(host, "."):]
	
	tunnel := schema + host
	mesh := schema + "-mesh" + host
	dnsnames := []string{tunnel, mesh}
	log.Infof("GenerateCSR: customer %s", schema)
	template := &x509.CertificateRequest{
		SignatureAlgorithm: x509.SHA256WithRSA,
		PublicKeyAlgorithm: x509.RSA,
		PublicKey:          &certKey.PublicKey,
		Subject:            pkix.Name{
			CommonName: schema,
			Organization:       []string{"Dymium, Inc"},
			OrganizationalUnit: []string{"Engineering"},
			Locality:           []string{"Los Gatos"},
			Province:           []string{"California"},
			Country:            []string{"US"},
		},
		DNSNames:           dnsnames,
	}
	log.Info("Generating certificate request...")
	csrDER, err := x509.CreateCertificateRequest(rand.Reader, template, certKey)
	if err != nil {
		return "", "", err
	}
	out := pemCSR(csrDER)

	pemBlock, _ := pem.Decode(out)
	if pemBlock == nil {
		return "", "", err // wrong
	}

	clientCSR, err := x509.ParseCertificateRequest(pemBlock.Bytes)
	if err != nil {
		return "", "", err
	}

	if err = clientCSR.CheckSignature(); err != nil {
		return "", "", err
	}

	// create certificate template
	clientCRTTemplate := x509.Certificate{
		Signature:          clientCSR.Signature,
		SignatureAlgorithm: clientCSR.SignatureAlgorithm,

		PublicKeyAlgorithm: clientCSR.PublicKeyAlgorithm,
		PublicKey:          clientCSR.PublicKey,

		SerialNumber: big.NewInt(2),
		Issuer:       CaCert.Subject,
		Subject:      clientCSR.Subject,
		NotBefore:    time.Now().Add(-300 * time.Second),      // grace time
		NotAfter:     time.Now().Add(3 * 30 * 24 * time.Hour), // 3 months
		KeyUsage:     x509.KeyUsageDigitalSignature,
		ExtKeyUsage:  []x509.ExtKeyUsage{x509.ExtKeyUsageClientAuth},
		DNSNames:     clientCSR.DNSNames,
	}

	// create client certificate from template and CA public key
	clientCRTRaw, err := x509.CreateCertificate(rand.Reader, &clientCRTTemplate, CaCert,
		clientCSR.PublicKey, CaKey)

	if err != nil {
		return "", "", err
	}

	cert := pem.EncodeToMemory(&pem.Block{Type: "CERTIFICATE", Bytes: clientCRTRaw})

	data := struct {
		Certificate string `json:"certificate"`
		Key         string `json:"key"`
	}{
		Certificate: string(cert),
		Key:         string(pemEncoded),
	}

	// Marshal the struct to a JSON string
	jsBytes, _ := json.Marshal(data)

	// Convert bytes to string for output
	js := string(jsBytes)
	return password, js, nil
}
