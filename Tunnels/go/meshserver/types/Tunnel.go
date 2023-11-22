// this file is automatically generated.
// !!! DO NOT EDIT !!!

package types



type ConnectorStatus string
const (
  CS_Provisioned ConnectorStatus = "provisioned"
  CS_Configured ConnectorStatus = "configured"
)

type TunnelStatus string
const (
  TS_Provisioned TunnelStatus = "provisioned"
  TS_Configured TunnelStatus = "configured"
  TS_Active TunnelStatus = "active"
)

type AddConnectorRequest struct {
   Id *string `json:"id"`
   Name string `json:"name"`
   Accesskey string `json:"accesskey"`
   Secret string `json:"secret"`
   Tunnels []Tunnel `json:"tunnels"`
}

type AuthorizationCodeRequest struct {
   Customerid string `json:"customerid"`
   Code string `json:"code"`
}

type AuthorizationCodeResponse struct {
   Token string `json:"token"`
   Name string `json:"name"`
   Groups []string `json:"groups"`
}

type CSRResponse struct {
   Version string `json:"version"`
   Certificate string `json:"certificate"`
}

type CertificateRequest struct {
   Csr string `json:"csr"`
}

type CertificateRequestWithSecret struct {
   Customer string `json:"customer"`
   Key string `json:"key"`
   Secret string `json:"secret"`
   Csr string `json:"csr"`
}

type Connector struct {
   Id string `json:"id"`
   Name string `json:"name"`
   Accesskey *string `json:"accesskey"`
   Secret *string `json:"secret"`
   Status *TunnelStatus `json:"status"`
   Tunnels []Tunnel `json:"tunnels"`
}

type CustomerIDRequest struct {
   Customerid string `json:"customerid"`
}

type CustomerIDResponse struct {
   LoginURL string `json:"loginURL"`
   Lbaddress string `json:"lbaddress"`
   Lbport int `json:"lbport"`
   ProtocolVersion string `json:"protocolVersion"`
   ClientMajorVersion string `json:"clientMajorVersion"`
   ClientMinorVersion string `json:"clientMinorVersion"`
}

type GetKeySecret struct {
   Accesskey string `json:"accesskey"`
   Secret string `json:"secret"`
}

type MachineCSRResponse struct {
   Version string `json:"version"`
   Jwt string `json:"jwt"`
   Certificate string `json:"certificate"`
}

type RequestUpdate struct {
   ProtocolVersion string `json:"protocolVersion"`
   ClientMajorVersion string `json:"clientMajorVersion"`
   ClientMinorVersion string `json:"clientMinorVersion"`
}

type Tunnel struct {
   Id *string `json:"id"`
   Name string `json:"name"`
   Address string `json:"address"`
   Port string `json:"port"`
   Localport *string `json:"localport"`
   Status *ConnectorStatus `json:"status"`
}
