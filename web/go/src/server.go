//
// Copyright (c) 2022 Dymium, Inc. All rights reserved.
// written by igor@dymium.io
//
package main

import (
	_ "errors"
	"flag"
	"io"
	"net/http"
	"os"
	"time"
	"fmt"
	"strings"
	"github.com/go-http-utils/etag"
	"github.com/gorilla/mux"
	cache "github.com/victorspringer/http-cache"
	"github.com/victorspringer/http-cache/adapter/memory"
	"dymium.com/dymium/customer"
	"dymium.com/dymium/admin"
	"dymium.com/dymium/common"
	"dymium.com/dymium/authentication"
	"dymium.com/dymium/log"
)


// CustomETagMiddleware applies custom logic and then etag middleware
func CustomETagMiddleware(h http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        // Your custom pre-etag logic here
		if strings.HasPrefix(r.URL.Path, "/bin/") || strings.HasPrefix(r.URL.Path, "/api/") || strings.HasPrefix(r.URL.Path, "/auth/") {
			h.ServeHTTP(w, r)
			return
		}
        // Apply etag middleware
        etagHandler := etag.Handler(h, false)
        etagHandler.ServeHTTP(w, r)

        // Your custom post-etag logic here
    })
}

func main() {
	selfAddr := flag.String("address", "", "IP address to listen on")
	ssl := flag.Bool("ssl", false, "Use https")
	flag.Parse()

	dbhost := os.Getenv("DATABASE_HOST")
	dbpassword := os.Getenv("DATABASE_PASSWORD")
	dbport := os.Getenv("DATABASE_PORT")
	dbuser := os.Getenv("DATABASE_USER")
	dbname := os.Getenv("DATABASE_NAME")
	dbtls  := os.Getenv("DATABASE_TLS")
	log.Init("webserver")

	log.Debugf("dbname: %s", dbname)
	if(dbtls == "")	{
		dbtls = "disable"
	}
	if(dbname == "") {
		dbname = "dymium"
	}


	err := authentication.Init(dbhost, dbport, dbuser, dbpassword, dbname, dbtls)	
	if(err != nil) {
		log.Fatal(err.Error())
	}
	p := mux.NewRouter()

	loggingMiddleware := func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			// Do stuff here

			if r.RequestURI != "/healthcheck" {
				// Loop over header 
				var out []string
				for name, values := range r.Header {
					// Loop over all values for the name.
					for _, value := range values {
						s := fmt.Sprintf("%s: %s", name, value)
						out = append(out, s)
					}
				}

				token := common.TokenFromHTTPRequest(r)
				msg := fmt.Sprintf("://%s%s", r.Host, r.RequestURI)
				if token != "" {
					schema, roles, groups, email, _, session, err := authentication.GetSchemaRolesFromToken(token)
					if err != nil {
						log.InfoUserArrayf(schema, session, email, groups, roles, msg, out)
					} else {
						log.InfoArrayf(msg, out)			
					}

				} else {
					log.InfoArrayf(msg, out)

				}
			}
			// Call the next handler, which can be another middleware in the chain, or the final handler.
			next.ServeHTTP(w, r)
		})
	}
	p.Use(loggingMiddleware)
	memcached, err := memory.NewAdapter(
		memory.AdapterWithAlgorithm(memory.LRU),
		memory.AdapterWithCapacity(20000000),
	)
	if err != nil {
		log.Panic(err)
	}

	cacheClient, err := cache.NewClient(
		cache.ClientWithAdapter(memcached),
		cache.ClientWithTTL(10*time.Minute),
		cache.ClientWithRefreshKey("opn"),
	)

	if err != nil {
		log.Panic(err)
	}
	p.HandleFunc("/healthcheck", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Cache-Control", common.Nocache)
		w.Header().Set("Content-Type", "text/html")
	
		io.WriteString(w, "<html><body>OK</body></html>")
	}).Methods("GET")

	os.Setenv("AWS_ACCESS_KEY_ID", os.Getenv("SES_KEY"))
	os.Setenv("AWS_SECRET_ACCESS_KEY", os.Getenv("SES_SECRET"))
	os.Setenv("AWS_DEFAULT_REGION", "us-east-1")

	authentication.AuthenticationAdminHandlers(p)
	authentication.AuthenticationPortalHandlers(p)
	admin.AdminHandlers(p)
	customer.CustomerHandlers(p)

	_ = cacheClient.Middleware(CompressHandler(etag.Handler(p, false)))
	cp := CompressHandler(CustomETagMiddleware(p))
	//cp := CompressHandler(p)

	if *ssl {
		log.Infof("Start listening on %s", *selfAddr+":443")
		log.Panic(http.ListenAndServeTLS(*selfAddr+":443", "../../devcerts/dymium.crt", "../../devcerts/dymium.key", cp))
	} else {
		log.Infof("Start listening on %s", *selfAddr+":80")
		log.Panic(http.ListenAndServe(*selfAddr+":80", cp))

	}
}
