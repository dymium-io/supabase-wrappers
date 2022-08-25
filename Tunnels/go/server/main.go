package main
import (
	"fmt"
	"flag"
	"os"
	"log"
	"runtime"	
	"encoding/json"
	"dymium.com/server/tunnel"
)

var address string
var port int
func main() {

	flag.IntVar( &port, "p", 0, "Port")
	flag.StringVar(&address, "a", "", "Address")
	flag.Parse()
	fmt.Printf("port %d, address %s\n", port, address)

    if len(address) == 0 && port == 0  {
		if runtime.GOOS == "windows" {
			fmt.Println("Usage: tserver.exe -c <customer ID> -p <portal URL>")
		} else {
	        fmt.Println("Usage: tserver -c <customer ID> -p <portal URL>")
		}
        os.Exit(1)
    }
	certificatejson := os.Getenv("CERTIFICATE")
	t := struct {
		Key string
		Certificate string
	}{}	
	err := json.Unmarshal([]byte(certificatejson), &t)
	if err != nil {
		log.Printf("Cert unmarshaling error: %s\n", err.Error() )
		os.Exit(1)
	}
	passphrase := os.Getenv("PASSPHRASE")

	cajson := os.Getenv("CA_CERTIFICATE")
	tt := struct {
		Certificate string
	}{}	
	fmt.Printf("%s\n", cajson)
	err = json.Unmarshal([]byte(cajson), &tt)
	if err != nil {
		log.Printf("CA Cert unmarshaling error: %s\n", err.Error() )
		os.Exit(1)
	}

	tunnel.Server(address, port, []byte(t.Certificate), []byte(t.Key), passphrase, []byte(tt.Certificate))
}