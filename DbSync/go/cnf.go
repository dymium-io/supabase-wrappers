package main

import (
	"encoding/json"
	"fmt"
	"net"
	"os"
	"strconv"

	"aws"
)

type guardianConf struct {
	GuardianAddress       []string `json:"guardian_address"`
	GuardianPort          int      `json:"guardian_port"`
	GuardianTls           *bool    `json:"guardian_tls"`
	GuardianUser          string   `json:"guardian_user"`
	GuardianDatabase      string   `json:"guardian_database"`
	GuardianAdminPassword string   `json:"guardian_password"`
	CustomerAESKey        string   `json:"customer_aes_key"`
}

type conf struct {
	DymiumHost      string `json:"dymium_host"`
	DymiumPort      int    `json:"dymium_port"`
	DymiumTls       bool   `json:"dymium_tls"`
	DymiumDatabase  string `json:"dymium_database"`
	DymiumUser      string `json:"dymium_user"`
	DymiumPassword  string `json:"dymium_password"`
	ConnectorDomain string `json:"connector_domain"`
	GuardianConf    guardianConf
}

func getConf(customer string, confGuardianAddress bool) (c *conf, err error) {

	returnError := func(err error) (*conf, error) {
		return nil, err
	}

	var ok bool

	cnf := conf{}

	if cnf.DymiumHost, ok = os.LookupEnv("DATABASE_HOST"); !ok {
		return returnError(fmt.Errorf("DATABASE_HOST is not defined"))
	} else if cnf.DymiumHost == "localhost" {
		cnf.DymiumHost = "docker.for.mac.host.internal"
	}

	if port, ok := os.LookupEnv("DATABASE_PORT"); !ok {
		return returnError(fmt.Errorf("DATABASE_PORT is not defined"))
	} else if cnf.DymiumPort, err = strconv.Atoi(port); err != nil || cnf.DymiumPort < 1 || cnf.DymiumPort >= 0xFFFF {
		return returnError(fmt.Errorf("Wrong DATABASE_PORT %s", port))
	}

	if tls, ok := os.LookupEnv("DATABASE_TLS"); !ok {
		return returnError(fmt.Errorf("DATABASE_TLS is not defined"))
	} else if cnf.DymiumTls, err = strconv.ParseBool(tls); err != nil {
		return returnError(fmt.Errorf("Wrong DATABASE_TLS %s", tls))
	}

	if cnf.DymiumDatabase, ok = os.LookupEnv("DATABASE_DB"); !ok {
		return returnError(fmt.Errorf("DATABASE_DB is not defined"))
	}

	if cnf.DymiumUser, ok = os.LookupEnv("DATABASE_USER"); !ok {
		return returnError(fmt.Errorf("DATABASE_USER is not defined"))
	}

	if cnf.ConnectorDomain, ok = os.LookupEnv("CONNECTOR_DOMAIN"); !ok {
		cnf.ConnectorDomain = ".mesh.local"
	}

	if cnf.DymiumPassword, ok = os.LookupEnv("DATABASE_PASSWORD"); !ok {
		return returnError(fmt.Errorf("DATABASE_PASSWORD is not defined"))
	} else {
		if p, err := aws.GetSecret(cnf.DymiumPassword); err == nil {
			cnf.DymiumPassword = p
		} else {
			return returnError(fmt.Errorf("DATABASE_PASSWORD is not defined: %v", err))
		}
	}

	var cnfM map[string]guardianConf

	if cnfS, ok := os.LookupEnv("GUARDIAN_CONF"); ok {
		if err = json.Unmarshal([]byte(cnfS), &cnfM); err != nil {
			return returnError(fmt.Errorf("wrong GUARDIAN_CONF definition: %v", err))
		}
	} else {
		return returnError(fmt.Errorf("GUARDIAN_CONF is not defined"))
	}

	if r, ok := cnfM["DEFAULT"]; ok {
		cnf.GuardianConf = r
	} else {
		return returnError(fmt.Errorf("GUARDIAN_CONF[%q] nor GUARDIAN_CONF[\"DEFAULT\"] are not defined", customer))
	}

	if r, ok := cnfM[customer]; ok {
		if r.GuardianPort != 0 {
			cnf.GuardianConf.GuardianPort = r.GuardianPort
		}
		if cnf.GuardianConf.GuardianPort <= 0 || cnf.GuardianConf.GuardianPort >= 0xFFFF {
			return returnError(fmt.Errorf("Wrong guardian port number %d", cnf.GuardianConf.GuardianPort))
		}

		if r.GuardianDatabase != "" {
			cnf.GuardianConf.GuardianDatabase = r.GuardianDatabase
		}
		if cnf.GuardianConf.GuardianDatabase == "" {
			return returnError(fmt.Errorf("Guardian database name is not defined"))
		}

		if r.GuardianUser != "" {
			cnf.GuardianConf.GuardianUser = r.GuardianUser
		}
		if cnf.GuardianConf.GuardianUser == "" {
			return returnError(fmt.Errorf("Guardian user is not defined"))
		}

		if r.GuardianAdminPassword != "" {
			cnf.GuardianConf.GuardianAdminPassword = r.GuardianAdminPassword
		}
		if cnf.GuardianConf.GuardianAdminPassword == "" {
			return returnError(fmt.Errorf("Guardian admin password is not defined"))
		} else {
			if p, err := aws.GetSecret(cnf.GuardianConf.GuardianAdminPassword); err == nil {
				cnf.GuardianConf.GuardianAdminPassword = p
			} else {
				return returnError(fmt.Errorf("Gardian admin password not defined: %v", err))
			}
		}

		if r.CustomerAESKey != "" {
			cnf.GuardianConf.CustomerAESKey = r.CustomerAESKey
		}
		if cnf.GuardianConf.CustomerAESKey != "" {
			if p, err := aws.GetSecret(cnf.GuardianConf.CustomerAESKey); err == nil {
				cnf.GuardianConf.CustomerAESKey = p
			} else {
				return returnError(fmt.Errorf("AES key for customer [%s] not defined: %v",
					customer, err))
			}
		}

		if r.GuardianTls != nil {
			cnf.GuardianConf.GuardianTls = r.GuardianTls
		}

		if len(r.GuardianAddress) > 0 {
			cnf.GuardianConf.GuardianAddress = r.GuardianAddress
		}

	}

	if confGuardianAddress {
		if len(cnf.GuardianConf.GuardianAddress) == 0 {
			if ipsIP, err := net.LookupIP(customer + ".guardian.local"); err != nil {
				return returnError(fmt.Errorf("Can not resolve DNS name %q: %v", customer+".guardian.local", err))
			} else {
				cnf.GuardianConf.GuardianAddress = make([]string, len(ipsIP))
				for k, ip := range ipsIP {
					cnf.GuardianConf.GuardianAddress[k] = ip.String()
				}
			}
		} else {
			for k := range cnf.GuardianConf.GuardianAddress {
				if cnf.GuardianConf.GuardianAddress[k] == "localhost" {
					cnf.GuardianConf.GuardianAddress[k] = "docker.for.mac.host.internal"
				}
			}
		}
	}

	return &cnf, nil

}
