import Alert from 'react-bootstrap/Alert'
import * as types from '@dymium/common/Types/Common'
import * as internal from '@dymium/common/Types/Internal'
import * as http from '@dymium/common/Api/Http'


export function getTokenProperty(prop) {
    let token = sessionStorage.getItem("Session")
    if (token === "" || token === null)
        return undefined

    let sp = token.split('.')
    if(sp.length !== 3)
        return undefined
    let b64 = sp[1]
    while(b64.length % 4 !== 0) {
        b64 += "="
    }
    let claims = atob(b64)
    let j = JSON.parse(claims)
    return j[prop]
}
export function isInstaller() {
    let roles = getTokenProperty("roles")
    if(roles === undefined)
        return false
    for(let role of roles) {
        if(role === "installer")
            return true
    }
    return false
}
export const databaseTypes = {}
export const databasePorts = {}

for(let index = 0; index < types.ConnectionType_as_strings.length; index++) {
    let key =  types.ConnectionType_as_strings[index]
    let pp = types.ConnectionPortsType_as_strings[index]
    let p = pp.split("_")
    databaseTypes[key] = types.humanReadableConnectionType(key as types.ConnectionType)
    databasePorts[key] = parseInt(p[1])
}


export function getDatascopes(setSpinner, setAlert, setDatascopes, onSuccess)  {
    setSpinner(true)
    http.sendToServer("GET", "/api/getdatascopes",
      null, "",
      resp => {

        resp.json().then(js => {
           if(js.status !== "OK") {
            setAlert(
                <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                    Error retrieving datascopes: {js.errormessage} { }
                </Alert>
            )
           } else {
              setDatascopes(js.records)
              onSuccess(js.records)
           }
           setTimeout( () => setSpinner(false), 500)
        }).catch((error) => {
            setSpinner(false)
            setAlert(
                <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                    Error retrieving datascopes {error.message}
                </Alert>
            )            
        })
      },
      resp => {
        console.log("on error")
        setSpinner(false)
        resp != null && resp.text().then(t=>
        setAlert(
            <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                Error retrieving datascopes: {t}
            </Alert>)
        )        
      },
      error => {
        console.log("on exception: " + error)
        setSpinner(false)
        setAlert(
            <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                Error retrieving datascopes {error.message}
            </Alert>
        )           
      })
  }
