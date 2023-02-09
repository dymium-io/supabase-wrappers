//
// Copyright (c) 2022 Dymium, Inc. All rights reserved.
// written by igor@dymium.io
//
package ahandlers

import (
	"dymium.com/dymium/authentication"
	"dymium.com/dymium/common"
	"dymium.com/dymium/types"
	"dymium.com/dymium/log"
	"encoding/json"
	"io/ioutil"
	"net/http"
	"fmt"
)
type contextKey int
const authenticatedSchemaKey contextKey = 0
const authenticatedEmailKey contextKey = 1
const authenticatedGroupsKey contextKey = 2
const authenticatedOrgKey contextKey = 3
const authenticatedRolesKey contextKey = 4
const authenticatedSessionKey contextKey = 5

func AuthMiddleware(h http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// do stuff
		token := common.TokenFromHTTPRequest(r)
		err := authentication.ValidateAdminToken(token)

		if err != nil {
			log.Errorf("Auth Error: %s", err.Error())
			status := types.OperationStatus{"AuthError", err.Error()}
			js, err := json.Marshal(status)
			
			if err != nil {
				http.Error(w, err.Error(), http.StatusInternalServerError)
				return
			}
			w.Header().Set("Cache-Control", common.Nocache)
			w.Header().Set("Content-Type", "application/json")
			w.Write(js)
			return
		}

		h.ServeHTTP(w, r)
	})
}

func Commonheaders(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Cache-Control", common.Cachedirective)
	w.Header().Set("x-content-type-options", "nosniff")
	w.Header().Set("strict-transport-security", "max-age=31536000")
}

func CreateNewCustomer(w http.ResponseWriter, r *http.Request) {
	status := types.OperationStatus{"OK", ""}

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()
	t := types.Customer{}
	err := json.Unmarshal(body, &t)
	
	js, err := json.Marshal(status)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	err = authentication.CreateNewCustomer(t)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}

func GetCustomers(w http.ResponseWriter, r *http.Request)  {
	customers, err := authentication.GetCustomers()
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return		
	}

	js, err := json.Marshal(customers)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}

func GetGlobalUsage(w http.ResponseWriter, r *http.Request)  {
	usage, err := authentication.GetGlobalUsage()
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return		
	}

	js, err := json.Marshal(usage)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}

func DeleteCustomer(w http.ResponseWriter, r *http.Request) {
	status := types.OperationStatus{"OK", ""}

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()
	t := types.DeleteCustomer{}
	err := json.Unmarshal(body, &t)

	fmt.Printf("in DeleteCustomer %v\n", t)
	
	js, err := json.Marshal(status)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	err = authentication.DeleteCustomer(t.Id, t.Schema)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}

func UpdateCustomer(w http.ResponseWriter, r *http.Request) {
	status := types.OperationStatus{"OK", ""}

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()
	t := types.Customer{}
	err := json.Unmarshal(body, &t)
	
	js, err := json.Marshal(status)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	err = authentication.UpdateCustomer(t)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}