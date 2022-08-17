//
// Copyright (c) 2022 Dymium, Inc. All rights reserved.
// written by igor@dymium.io
//
package main
import (
	"fmt"
	"flag"
	"os"
	"net/url"
	"runtime"
	"net/http"
	"io"
	"crypto/rand"
	"crypto/rsa"
	"crypto/x509"
	"crypto/x509/pkix"
	"encoding/pem"
	_"github.com/Jeffail/gabs"
	"encoding/json"
	"dymium.com/client/types"
	"bytes"
	"github.com/pkg/browser"
	"github.com/gorilla/mux"
	"context"
)
const (
	csrPEMBlockType = "CERTIFICATE REQUEST"
	Nocache = "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"
)
var certKey *rsa.PrivateKey
var clientCert *x509.Certificate
var customerid string
var portalurl string
func generateError(w http.ResponseWriter, r *http.Request, header string, body string) error {
	/*
	Failed to get userinfo: "+err.Error()
	*/
	fmt.Printf("Error %s: %s\n", header, body)
	w.Header().Set("Cache-Control", Nocache)
	w.Header().Set("Content-Type", "text/html")
		w.Write([]byte(`<html>
		<head>
		<script>
		 !function() {
			window.location.href = "/app/error?header=`+url.QueryEscape(header)+`&body=`+url.QueryEscape(body)+`"
		 }()
		</script>
		</head>
		<body>Callback arrived</body>
		</html>`))

	return nil
}
// convert DER to PEM format
func pemCSR(derBytes []byte) []byte {
	pemBlock := &pem.Block{
		Type:    csrPEMBlockType,
		Headers: nil,
		Bytes:   derBytes,
	}
	out := pem.EncodeToMemory(pemBlock)
	return out
}
func generateCSR(name string, groups []string) ([]byte, error) { 
	var err error
	certKey, err = rsa.GenerateKey(rand.Reader, 4096)
	if err != nil {
		return []byte{}, err
	}

	template := &x509.CertificateRequest{
		SignatureAlgorithm: x509.SHA256WithRSA,
		PublicKeyAlgorithm: x509.RSA,
		PublicKey:          &certKey.PublicKey,
		Subject:            pkix.Name{CommonName: name},
		DNSNames:           groups,
	}
	fmt.Println("Generating CSR")
	csrDER, err := x509.CreateCertificateRequest(rand.Reader, template, certKey)
	if err != nil {
		return []byte{}, err
	}
	out := pemCSR(csrDER)

	return out, nil
}

func sendCSR(csr []byte, token string) error {
	var outbody types.CertificateRequest
	outbody.Csr = string(csr)
	js, err := json.Marshal(outbody)
	if err != nil {
		fmt.Printf("Error: %s\n", err.Error()) 
		os.Exit(1)		
	}

	urlStr := fmt.Sprintf("%sapi/getclientcertificate", portalurl)

	req, err := http.NewRequest("POST", urlStr, bytes.NewBuffer(js))
	if err != nil {
		fmt.Printf("Error: %s\n", err.Error()) 
		os.Exit(1)		
	}		
	req.Header.Add("Content-Type", "application/json")
	req.Header.Add("Authorization", "Bearer " + token)

	fmt.Printf("token %s\n", token)
	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		fmt.Printf("Error: %s\n", err.Error()) 
		os.Exit(1)		
	}	

	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		fmt.Printf("Error: %s\n", err.Error()) 
		os.Exit(1)		
	}

	var back types.CSRResponse
	err = json.Unmarshal(body, &back)
	if err != nil {
		fmt.Printf("Error: %s\n", err.Error()) 
		os.Exit(1)		
	}	

	pemBlock, _ := pem.Decode( []byte(back.Certificate) )
    if pemBlock == nil {
		fmt.Printf("Certiicate decode failed!")
		return nil
    }

	clientCert, err = x509.ParseCertificate(pemBlock.Bytes) 
	if err != nil {
		fmt.Printf("Error: %s\n", err.Error()) 
		os.Exit(1)		
	}	
	
	fmt.Printf("cert: %v\n", back.Certificate)
	return nil
}

func main() {

	flag.StringVar( &customerid, "c", "", "Customer ID")
	flag.StringVar(&portalurl, "p", "", "Portal URL")
	flag.Parse()

    if len(customerid) == 0 && len(portalurl) == 0  {
		if runtime.GOOS == "windows" {
			fmt.Println("Usage: tclient.exe -c <customer ID> -p <portal URL>")
		} else {
	        fmt.Println("Usage: tclient -c <customer ID> -p <portal URL>")
		}
        os.Exit(1)
    }

	var outbody types.CustomerIDRequest
	outbody.Customerid = customerid
	js, err := json.Marshal(outbody)
	if err != nil {
		fmt.Printf("Error: %s\n", err.Error()) 
		os.Exit(1)		
	}

	urlStr := fmt.Sprintf("%sapi/querytunnel", portalurl)
	resp, err := http.Post(urlStr, "application/json", bytes.NewBuffer(js) )

	if err != nil {
		fmt.Printf("Error: %s\n", err.Error()) 
		os.Exit(1)		
	}
	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		fmt.Printf("Error: %s\n", err.Error()) 
		os.Exit(1)		
	}
	var back types.CustomerIDResponse
	err = json.Unmarshal(body, &back)
	if err != nil {
		fmt.Printf("Error: %s\n", err.Error()) 
		os.Exit(1)		
	}	
	fmt.Printf("%v\n", back )
	browser.OpenURL(back.LoginURL)
	p := mux.NewRouter()
	loggingMiddleware := func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			fmt.Printf("%s%s\n", r.Host, r.RequestURI)
			// Call the next handler, which can be another middleware in the chain, or the final handler.
			next.ServeHTTP(w, r)
		})
	}
	p.Use(loggingMiddleware)



	p.HandleFunc("/auth/redirect", func(w http.ResponseWriter, r *http.Request) {
		error := mux.Vars(r)["error"]
		error_description := mux.Vars(r)["error_description"]		

		generateError(w, r, error, error_description)
	}).
	Queries("error", "{error}").
    Queries("error_description", "{error_description}").Methods("GET")
	s := http.Server{Addr: "127.0.0.1:63000", Handler: p}

	p.HandleFunc("/auth/redirect", func(w http.ResponseWriter, r *http.Request) {
		code := r.URL.Query().Get("code")
		if code == "" {
			generateError(w, r, "Error: ", r.URL.Query().Get("error_description"))
			return
		} else {
			fmt.Printf("Returned code: %s\n", code)
		}
		//w.Header().Set("Cache-Control", common.Nocache)
		w.Header().Set("Content-Type", "text/html")			
		head := `<html>
		<head>
		<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap@4.6.2/dist/css/bootstrap.min.css" integrity="sha384-xOolHFLEh07PJGoPkLv1IbcEPTNtaed2xpHsD9ESMhqIYd0nLMwNLD69Npy4HI+N" crossorigin="anonymous">
		<script src="https://cdn.jsdelivr.net/npm/bootstrap@4.6.2/dist/js/bootstrap.min.js" integrity="sha384-+sLIOodYLS7CIrQpBjl+C7nPvqq+FbNUBDunl/OZv93DB7Ln/533i8e/mZXLi/P+" crossorigin="anonymous"></script>
		<style>
		#anicanvas {
			width: 100%;
			height: 100%;

			background: linear-gradient(-5deg, #015f81 0%, #0278a2 20%, rgb(0, 151,206) 50%, rgb(255,158,24) 50%, rgb(220, 137, 20)  80%, rgb(199, 124, 18) 100%);

			z-index: 0;
		  }
		#divbox {
			position: absolute;
			top: 30%;
			left: 0px;
			width: 100%;
			height: 100vh;
			z-index: 100;
			text-shadow: 1px 1px #FEFEFE;
		}
		</style>

		</head>
		<body class="text-center">
		<canvas style={{ position: 'relative', top: '0px', left: '0px', z-index: '0' }} id="anicanvas">

		</canvas>
		<div id='divbox'>
		<h3 class="mt-5">Sending authentication code to Dymium...</h3>
		`
		w.Write( []byte (head))		
		if f, ok := w.(http.Flusher); ok {
			f.Flush()
		}

		var outbody types.AuthorizationCodeRequest
		outbody.Customerid = customerid
		outbody.Code = code

		js, err := json.Marshal(outbody)
		if err != nil {
			fmt.Printf("Error: %s\n", err.Error()) 
			os.Exit(1)		
		}
	
		urlStr := fmt.Sprintf("%sapi/authenticatebycode", portalurl)
		resp, err := http.Post(urlStr, "application/json", bytes.NewBuffer(js) )
	
		if err != nil {
			fmt.Printf("Error: %s\n", err.Error()) 
			os.Exit(1)		
		}
		defer resp.Body.Close()
		body, err := io.ReadAll(resp.Body)
		if err != nil {
			fmt.Printf("Error: %s\n", err.Error()) 
			os.Exit(1)		
		}
		var groups types.AuthorizationCodeResponse
		err = json.Unmarshal(body, &groups)
		if err != nil {
			fmt.Printf("Error: %s\n", err.Error()) 
			os.Exit(1)		
		}	

		obody := `

		<h3 class="mt-5">Authenticated!</h3>
		<h3 class="mt-5">Creating a short term client certificate...</h3>

		`
		w.Write( []byte (obody))		
		if f, ok := w.(http.Flusher); ok {
			f.Flush()
		}

		csr, err := generateCSR(groups.Name, groups.Groups) 

		sendCSR(csr, groups.Token)

		tail := `
		<h3 class="mt-5">Certificate obtained! Now establishing a secure tunnel...</h3>
			</div>
			</body>
			</html>		
			`
		w.Write( []byte (tail))		
		if f, ok := w.(http.Flusher); ok {
			f.Flush()
		}	

		if err := s.Shutdown(context.TODO()); err != nil {
			panic(err) // failure/timeout shutting down the server gracefully
		}

	}).Methods("GET")




	s.ListenAndServe()
	fmt.Println("Done with the control channel")
	


}