// this file is automatically generated.
// !!! DO NOT EDIT !!!

package types





type Customer struct {
   Id *string `json:"id"`
   Name string `json:"name"`
   Orgid string `json:"orgid"`
   Schema string `json:"schema"`
   Domain string `json:"domain"`
   Admingroup string `json:"admingroup"`
}

type DeleteCustomer struct {
   Id string `json:"id"`
   Schema string `json:"schema"`
}

type GlobalUsage struct {
   Customers string `json:"customers"`
}
