import Alert from 'react-bootstrap/Alert'
import * as types from '@dymium/common/Types/Common'
import * as internal from '@dymium/common/Types/Internal'
import * as http from '@dymium/common/Api/Http'


export function getTokenProperty(prop) {
    let token = sessionStorage.getItem("Session")
    if (token === "" || token === null)
        return undefined

    let sp = token.split('.')
    let b64 = sp[1]
    while(b64.length % 4 !== 0) {
        b64 += "="
    }
    let claims = atob(b64)
    let j = JSON.parse(claims)
    return j[prop]
}

export const databaseTypes = {
    PostgreSQL: "PostgreSQL",
    MySQL: "MySQL",
    MariaDB: "MariaDB",
    SqlServer: "SQL Server",
    OracleDB: "Oracle DB",
}
export const databasePorts = {
    PostgreSQL: 5432,
    MySQL: 3306,
    MariaDB: 3306,
    SqlServer: 1433,
    OracleDB: 1521,
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
        setAlert(
            <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                Error retrieving datascopes
            </Alert>
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
export const PII_civilian = {
    not_applicable: "N/A",
    full_name: "Full Name",
    home_address: "Home address",
    email: "Email address",
    ssn: "Social Security Number",
    passport: "Passport number",
    driver_license: "Driver license",
    credit_card: "Credit card",
    dob: "Date of birth",
    phone: "Telephone number",
    owned_property: "Owned property",
    vin: "Vehicle Identification Number",
    login: "Login details",
    serial: "Processor or device serial number",
    mac: "MAC address",
    ip_address: "IP address",
    device_id: "Device ID",
    cookie: "Cookie",
    citizenship: "Citizenship",
    visa: "Visa or immigration status",
    ethnicity: "Ethnic background",
    religion: "Religious affiliation",
    sexual: "Sexual orientation",
    criminal: "Criminal history",
    medical: "Medical information",
    authentication: "Authentication information",
    first_name: "First name",
    last_name: "Last name",
    middle_name: "Middle name",
    country: "Country",
    state: "State",
    city: "City",
    zip: "Zipcode",
    gender: "Gender",
    race: "Race",
    age: "Age",
    non_specific_age: "Non-specific age range",
    job_position: "Job position",
    company: "Company",
    work_place: "Workplace",
    work_address: "Work Address",
    business_entity: "Business entity Id",
    contact_info: "Contact Info",
    address: "Address"
}