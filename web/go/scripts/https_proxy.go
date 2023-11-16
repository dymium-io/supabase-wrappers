package main

import (
    "crypto/tls"
    "log"
    "net/http"
    "net/http/httputil"
    "net/url"
)

var httpsPortsToHttpTargets map[string]string
const (
	certFile = "./cert.pem"
	keyFile  = "./key.pem"
)

	
func init() {
	httpsPortsToHttpTargets = map[string]string{
		"3001": "http://localhost:3080",
		"3000": "http://localhost:3080",
	}
}

func createProxy(target string) http.Handler {
    url, err := url.Parse(target)
    if err != nil {
        log.Fatal("Error parsing URL:", err)
    }

    return httputil.NewSingleHostReverseProxy(url)
}

func startServer(port string, target string, certFile string, keyFile string) {
    proxy := createProxy(target)

    server := &http.Server{
        Addr:    ":" + port,
        Handler: proxy,
        TLSConfig: &tls.Config{
            // Add any necessary TLS configuration
        },
    }

    log.Printf("Starting HTTPS server on port %s, proxying to %s", port, target)
    log.Fatal(server.ListenAndServeTLS(certFile, keyFile))
}

func main() {
    // Define your ports and target HTTP endpoints

    for port, target := range httpsPortsToHttpTargets {
        go startServer(port, target, certFile, keyFile)
    }

    // Prevent the main function from returning immediately
    select {}
}
