package main

import (
	"fmt"
	"aws"
)

func main() {
	r, err := aws.Invoke("DbAnalyzer", "latest",
		[]byte(`{
                           "typ": "PostgreSQL",
                           "address": "docker.for.mac.host.internal",
                           "port": 15432,
                           "user": "postgres",
                           "database": "z1",
                           "password": "docker",
                           "tls": false
                       }`))
	if err != nil {
		fmt.Printf("%+v",err)
	} else {
		fmt.Println(string(r))
	}
}
