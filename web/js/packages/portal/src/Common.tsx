import Alert from 'react-bootstrap/Alert'
import * as types from '@dymium/common/Types/Common'
let appliedColors = `
:root{
--primary-color: rgb(255, 158, 24);
--primary-color-text: rgb(31, 31, 32);
--primary-color-2: rgb(222, 139, 22);
--primary-color-2-text: #eee;
--primary-color-3: rgb(220, 158, 140); 
--primary-color-3-text: black;
--primary-pale: rgb(183, 114, 17);
--primary-pale-text: #eeeeee;
--secondary-color: rgb(6, 33, 76); 
}
`
function insertStyleSheet(colors) {

    var style = document.createElement('style');
    style.type = 'text/css';

    style.innerHTML = colors;

    document.getElementsByTagName('head')[0].appendChild(style);
}

let cssInserted = false
export function customizeStyleSheet() {
    if (cssInserted)
        return
    cssInserted = true
    insertStyleSheet(appliedColors)
}
export function getTokenProperty(prop) {
    let token = sessionStorage.getItem("Session")
    if (token === "" || token === null)
        return undefined

    let sp = token.split('.')
    let claims = atob(sp[1])
    let j = JSON.parse(claims)
    return j[prop]
}

export const databaseTypes = {
    postgres: "PostgreSQL",
    mysql: "MySQL",
    mariadb: "MariaDB",
    sqlserver: "SQL Server",
    oracle: "Oracle DB",
}
export const databasePorts = {
    postgres: 5432,
    mysql: 3306,
    mariadb: 3306,
    sqlserver: 1433,
    oracle: 1521,
}

export function sendToServer(method: string, url: string,
    headers, body: string, onsuccess, onfailure, onexception) {
    let token = window.sessionStorage.getItem("Session");

    if (token === null) {
        onfailure(null)
        return;
    }
    let _headers = {
        Authorization: "Bearer " + token,
        Cache: "no-cache",
    }
    if (headers != null) {
        _headers = {
            ..._headers,
            ...headers
        }
    }
    let params: any = {
        method,
        headers: _headers
    }
    if (body != "") {
        params = { ...params, body: body }
    }
    fetch(url, params
    ).then(response => {
        if (!response.ok) {
            onfailure(response)
        } else {
            onsuccess(response)
        }
    }).catch(onexception)
}


export function getConnections(setSpinner, setConns, setAlert, onSuccess) {
    setSpinner(true)
    setConns([])
    sendToServer("GET", "/api/getconnections",
        null, "",
        resp => {

            resp.json().then(_js => {
                let js = types.ConnectionResponse.fromJson(_js)
                if (js.status !== "OK") {
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Error retrieving connections: {js.errormessage} { }
                        </Alert>
                    )
                    setTimeout(() => setSpinner(false), 500)
                    return
                }
                let cc = js.data.map(x => {
                    return {
                        id: x.id,
                        credid: x.credid,
                        dbtype: x.dbtype,
                        name: x.name,
                        dbname: x.dbname,
                        address: x.address,
                        port: x.port,
                        description: x.description,
                        usetls: x.useTLS,

                    }
                })

                setConns(cc)
                if(onSuccess != undefined) {
                    onSuccess()
                }
            })

            setTimeout(() => setSpinner(false), 500)
        },
        resp => {
            console.log("on error")
            setSpinner(false)
        },
        error => {
            console.log("on exception: " + error)
            setSpinner(false)
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