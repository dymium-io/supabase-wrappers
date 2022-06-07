package main

import (
	"os"
	"os/signal"
        "syscall"
	"io"
	"bytes"
	"time"

	"net/http"

	"log"
	"fmt"

	"context"
	
	"aws"
)

func main() {

	var serverPort string
	var ok bool

	if serverPort, ok = os.LookupEnv("SERVER_PORT"); !ok {
                serverPort = "80"
        }

	http.HandleFunc("/healthcheck", func(w http.ResponseWriter, r *http.Request) {})

        http.HandleFunc("/callFunction", handler)

	var srv http.Server
        srv.Addr = ":" + serverPort
        idleConnsClosed := make(chan struct{})
        go func() {
                sigint := make(chan os.Signal, 1)
                signal.Notify(sigint, os.Interrupt, syscall.SIGINT, syscall.SIGTERM)
                <-sigint

                // We received an interrupt signal, shut down.
                if err := srv.Shutdown(context.Background()); err != nil {
                        // Error from closing listeners, or context timeout:
                        log.Printf("HTTP server Shutdown: %v", err)
                }
                close(idleConnsClosed)
        }()

        log.Println("Server started")
        if err := srv.ListenAndServe(); err != http.ErrServerClosed {
                // Error starting or closing listener:
                log.Fatalf("HTTP server ListenAndServe: %v", err)
        }
        <-idleConnsClosed
        log.Println("Server ended")
}

func handler(w http.ResponseWriter, r *http.Request) {

	fName := r.FormValue("fname")
	if fName == "" {
		log.Println("mandatory `fname` parameter not defined")
		http.NotFound(w,r)
		return
	}
	tag := r.FormValue("tag")
        ptag := &tag
	if tag == "" { ptag = nil }
	body,_ := io.ReadAll(r.Body)

	res, err := aws.Invoke(fName, ptag, body)
	if err != nil {
		log.Printf("aws.Invoke(%s) retruned err: %+v",fName,err)
		http.Error(w,fmt.Sprintf("%+v",err),500)
		return
	}

	http.ServeContent(w,r, fName, time.Now(),bytes.NewReader(res))
	
}

/*
   	res, err := aws.Invoke("DbAnalyzer", "latest",
		[]byte(`{
                           "typ": "PostgreSQL",
                           "address": "docker.for.mac.host.internal",
                           "port": 15432,
                           "user": "postgres",
                           "database": "z1",
                           "password": "docker",
                           "tls": false
                       }`))
*/
