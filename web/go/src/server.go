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
	log.Init()

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
				log.Debugf("%s%s", r.Host, r.RequestURI)
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
	cp := CompressHandler(etag.Handler(p, false))
	if *ssl {
		log.Infof("Start listening on %s", *selfAddr+":443")
		log.Panic(http.ListenAndServeTLS(*selfAddr+":443", "dymiumai.crt", "dymiumai.key", cp))
	} else {
		log.Infof("Start listening on %s", *selfAddr+":80")
		log.Panic(http.ListenAndServe(*selfAddr+":80", cp))

	}
}
