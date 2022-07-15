import React, { useEffect, useState, useRef } from 'react';
import Tabs from 'react-bootstrap/Tabs'
import Tab from 'react-bootstrap/Tab'
import Form from 'react-bootstrap/Form'
import Button from 'react-bootstrap/Button'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import InputGroup from 'react-bootstrap/InputGroup'
import Card from 'react-bootstrap/Card'
import Offcanvas from '@dymium/common/Components/Offcanvas'
import { Typeahead } from 'react-bootstrap-typeahead';
import Modal from 'react-bootstrap/Modal'
import Alert from 'react-bootstrap/Alert'
import BootstrapTable from 'react-bootstrap-table-next';
import cloneDeep from 'lodash/cloneDeep';
import Spinner from '@dymium/common/Components/Spinner'
import * as com from '../Common'
import * as types from '@dymium/common/Types/Internal'
import * as http from '../Api/Http'

const PIIs = Object.values(com.PII_civilian)

const Actions = [
    "Allow",
    "Block",
    "Redact",
    "Obfuscate",
    "Smart Redact"
]

interface Rule {
    regexp: string
    detection: string
    action: string
}
const rules = {

}
const PrefillUnclassified = {
    name: "Unclassified",
    rules: [

        {
            regexp: "homeaddress",
            detection: "Home address",
            action: "Allow"
        },
        {
            regexp: "email",
            detection: "Email address",
            action: "Allow"
        },
        {
            regexp: "(SSN|ssn|nationalid|national_id)",
            detection: "Social Security Number",
            action: "Allow"
        },
        {
            regexp: "(businessentityid|business_entity_id)",
            detection: "Business entity Id",
            action: "Allow"
        },
        {
            regexp: "(passport|Passport number|Passport_number)",
            detection: "Passport number",
            action: "Allow"
        },
        {
            regexp: "(Driver license|Driver_license|Driverlicense)",
            detection: "Driver license",
            action: "Allow"
        },
        {
            regexp: "(Credit card|Credit_card,Creditcard)",
            detection: "Credit card",
            action: "Allow"
        },
        {
            regexp: "(dob|dateofbirth|date_of_birth)",
            detection: "Date of birth",
            action: "Allow"
        },
        {
            regexp: "(phone|phonenumber|phone_number)",
            detection: "Telephone number",
            action: "Allow"
        },
        {
            regexp: "(login|signin|sign_in|username|user_name)",
            detection: "Login details",
            action: "Allow"
        },
        {
            regexp: "(serial|serialnumber|serial_number)",
            detection: "Processor or device serial number",
            action: "Allow"
        },
        {
            regexp: "(MAC_address|MACaddress)",
            detection: "MAC address",
            action: "Allow"
        },
        {
            regexp: "^(ip|ip_address|ipaddress)$",
            detection: "IP address",
            action: "Allow"
        },
        {
            regexp: "(device_id|deviceid)",
            detection: "Device ID",
            action: "Allow"
        },
        {
            regexp: "(cookie)",
            detection: "Cookie",
            action: "Allow"
        },
        {
            regexp: "(Citizenship|nationality)",
            detection: "Citizenship",
            action: "Allow"
        },
        {
            regexp: "(visa|immigration)",
            detection: "Visa or immigration status",
            action: "Allow"
        },
        {
            regexp: "(Ethnic)",
            detection: "Ethnic background",
            action: "Allow"
        },
        {
            regexp: "(Religi)",
            detection: "Religious affiliation",
            action: "Allow"
        },
        {
            regexp: "(sexual)",
            detection: "Sexual orientation",
            action: "Allow"
        },
        {
            regexp: "(Criminal)",
            detection: "Criminal history",
            action: "Allow"
        },
        {
            regexp: "(Medical)",
            detection: "Medical information",
            action: "Allow"
        },
        {
            regexp: "(authentication)",
            detection: "Authentication information",
            action: "Allow"
        },
        {
            regexp: "(First name|Firstname|First_name)",
            detection: "First name",
            action: "Allow"
        },
        {
            regexp: "(Middle name|Middlename|Middle_name)",
            detection: "Middle name",
            action: "Allow"
        },
        {
            regexp: "(last name|lastname|last_name)",
            detection: "Last name",
            action: "Allow"
        },
        {
            regexp: "(Country)",
            detection: "Country",
            action: "Allow"
        },
        {
            regexp: "(state)",
            detection: "State",
            action: "Allow"
        },
        {
            regexp: "(City)",
            detection: "City",
            action: "Allow"
        },
        {
            regexp: "(Zipcode|zip_code|zip code|postalcode|postal_code)",
            detection: "Zipcode",
            action: "Allow"
        },
        {
            regexp: "(Gender)",
            detection: "Gender",
            action: "Allow"
        },
        {
            regexp: "(race)",
            detection: "Race",
            action: "Allow"
        },
        {
            regexp: "(age)",
            detection: "Age",
            action: "Allow"
        },
        {
            regexp: "(job)",
            detection: "Job position",
            action: "Allow"
        },
        {
            regexp: "(Company)",
            detection: "Company",
            action: "Allow"
        },
        {
            regexp: "(work_place|workplace)",
            detection: "Work place",
            action: "Allow"
        },
        {
            regexp: "(Work_Address|WorkAddress)",
            detection: "Work Address",
            action: "Allow"
        },
        {
            regexp: "name",
            detection: "Full Name",
            action: "Allow"
        },
        {
            regexp: "(vin|VehicleIdentificationNumber|Vehicle_Identification_Number)",
            detection: "Vehicle Identification Number",
            action: "Allow"
        },
        {
            regexp: "(address)",
            detection: "Address",
            action: "Allow"
        },
        {
            regexp: "(contact)",
            detection: "Contact Info",
            action: "Allow"
        },
        {
            regexp: "",
            detection: "N/A",
            action: "Allow"
        },
    ]
}


const Confidential = {
    name: "Confidential",
    rules: [
        {
            regexp: "homeaddress",
            detection: "Home address",
            action: "Obfuscate"
        },
        {
            regexp: "(SSN|ssn|nationalid|national_id)",
            detection: "Social Security Number",
            action: "Obfuscate"
        },
        {
            regexp: "email",
            detection: "Email address",
            action: "Obfuscate"
        },
        {
            regexp: "(businessentityid|business_entity_id)",
            detection: "Business entity Id",
            action: "Obfuscate"
        },
        {
            regexp: "(passport|Passport number|Passport_number)",
            detection: "Passport number",
            action: "Obfuscate"
        },
        {
            regexp: "(Driver license|Driver_license|Driverlicense)",
            detection: "Driver license",
            action: "Obfuscate"
        },
        {
            regexp: "(Credit card|Credit_card,Creditcard)",
            detection: "Credit card",
            action: "Obfuscate"
        },
        {
            regexp: "(dob|dateofbirth|date_of_birth)",
            detection: "Date of birth",
            action: "Obfuscate"
        },
        {
            regexp: "(phone|phonenumber|phone_number)",
            detection: "Telephone number",
            action: "Obfuscate"
        },

        {
            regexp: "(login|signin|sign_in|username|user_name)",
            detection: "Login details",
            action: "Block"
        },
        {
            regexp: "(serial|serialnumber|serial_number)",
            detection: "Processor or device serial number",
            action: "Allow"
        },
        {
            regexp: "(MAC_address|MACaddress)",
            detection: "MAC address",
            action: "Allow"
        },
        {
            regexp: "^(ip|ip_address|ipaddress)$",
            detection: "IP address",
            action: "Obfuscate"
        },
        {
            regexp: "(device_id|deviceid)",
            detection: "Device ID",
            action: "Obfuscate"
        },
        {
            regexp: "(cookie)",
            detection: "Cookie",
            action: "Redact"
        },
        {
            regexp: "(Citizenship|nationality)",
            detection: "Citizenship",
            action: "Redact"
        },
        {
            regexp: "(visa|immigration)",
            detection: "Visa or immigration status",
            action: "Redact"
        },
        {
            regexp: "(Ethnic)",
            detection: "Ethnic background",
            action: "Redact"
        },
        {
            regexp: "(Religi)",
            detection: "Religious affiliation",
            action: "Redact"
        },
        {
            regexp: "(sexual)",
            detection: "Sexual orientation",
            action: "Redact"
        },
        {
            regexp: "(Criminal)",
            detection: "Criminal history",
            action: "Redact"
        },
        {
            regexp: "(Medical)",
            detection: "Medical information",
            action: "Smart Redact"
        },
        {
            regexp: "(authentication)",
            detection: "Authentication information",
            action: "Redact"
        },
        {
            regexp: "(First name|Firstname|First_name)",
            detection: "First name",
            action: "Obfuscate"
        },
        {
            regexp: "(Middle name|Middlename|Middle_name)",
            detection: "Middle name",
            action: "Obfuscate"
        },
        {
            regexp: "(last name|lastname|last_name)",
            detection: "Last name",
            action: "Obfuscate"
        },
        {
            regexp: "(Country)",
            detection: "Country",
            action: "Obfuscate"
        },
        {
            regexp: "(state)",
            detection: "State",
            action: "Obfuscate"
        },
        {
            regexp: "(City)",
            detection: "City",
            action: "Obfuscate"
        },
        {
            regexp: "(Zipcode|zip_code|zip code|postalcode|postal_code)",
            detection: "Zipcode",
            action: "Obfuscate"
        },
        {
            regexp: "(Gender)",
            detection: "Gender",
            action: "Obfuscate"
        },
        {
            regexp: "(race)",
            detection: "Race",
            action: "Obfuscate"
        },
        {
            regexp: "(age)",
            detection: "Age",
            action: "Obfuscate"
        },
        {
            regexp: "(job)",
            detection: "Job position",
            action: "Obfuscate"
        },
        {
            regexp: "(Company)",
            detection: "Company",
            action: "Obfuscate"
        },
        {
            regexp: "(work_place|workplace)",
            detection: "Work place",
            action: "Obfuscate"
        },
        {
            regexp: "(Work_Address|WorkAddress)",
            detection: "Work Address",
            action: "Obfuscate"
        },
        {
            regexp: "name",
            detection: "Full Name",
            action: "Obfuscate"
        },
        {
            regexp: "(vin|VehicleIdentificationNumber|Vehicle_Identification_Number)",
            detection: "Vehicle Identification Number",
            action: "Obfuscate"
        },
        {
            regexp: "(address)",
            detection: "Address",
            action: "Obfuscate"
        },
        {
            regexp: "(contact)",
            detection: "Contact Info",
            action: "Obfuscate"
        },

        {
            regexp: "",
            detection: "N/A",
            action: "Allow"
        },
    ]
}
const Secret = {
    name: "Secret",
    rules: [


        {
            regexp: "homeaddress",
            detection: "Home address",
            action: "Redact"
        },
        {
            regexp: "email",
            detection: "Email address",
            action: "Redact"
        },
        {
            regexp: "(SSN|ssn|nationalid|national_id)",
            detection: "Social Security Number",
            action: "Redact"
        },
        {
            regexp: "(businessentityid|business_entity_id)",
            detection: "Business entity Id",
            action: "Redact"
        },
        {
            regexp: "(passport|Passport number|Passport_number)",
            detection: "Passport number",
            action: "Redact"
        },
        {
            regexp: "(Driver license|Driver_license|Driverlicense)",
            detection: "Driver license",
            action: "Redact"
        },
        {
            regexp: "(Credit card|Credit_card,Creditcard)",
            detection: "Credit card",
            action: "Redact"
        },
        {
            regexp: "(dob|dateofbirth|date_of_birth)",
            detection: "Date of birth",
            action: "Redact"
        },
        {
            regexp: "(phone|phonenumber|phone_number)",
            detection: "Telephone number",
            action: "Redact"
        },

        {
            regexp: "(login|signin|sign_in|username|user_name)",
            detection: "Login details",
            action: "Block"
        },
        {
            regexp: "(serial|serialnumber|serial_number)",
            detection: "Processor or device serial number",
            action: "Allow"
        },
        {
            regexp: "(MAC_address|MACaddress)",
            detection: "MAC address",
            action: "Obfuscate"
        },
        {
            regexp: "^(ip|ip_address|ipaddress)$",
            detection: "IP address",
            action: "Obfuscate"
        },
        {
            regexp: "(device_id|deviceid)",
            detection: "Device ID",
            action: "Obfuscate"
        },
        {
            regexp: "(cookie)",
            detection: "Cookie",
            action: "Redact"
        },
        {
            regexp: "(Citizenship|nationality)",
            detection: "Citizenship",
            action: "Redact"
        },
        {
            regexp: "(visa|immigration)",
            detection: "Visa or immigration status",
            action: "Redact"
        },
        {
            regexp: "(Ethnic)",
            detection: "Ethnic background",
            action: "Redact"
        },
        {
            regexp: "(Religi)",
            detection: "Religious affiliation",
            action: "Redact"
        },
        {
            regexp: "(sexual)",
            detection: "Sexual orientation",
            action: "Redact"
        },
        {
            regexp: "(Criminal)",
            detection: "Criminal history",
            action: "Redact"
        },
        {
            regexp: "(Medical)",
            detection: "Medical information",
            action: "Smart Redact"
        },
        {
            regexp: "(authentication)",
            detection: "Authentication information",
            action: "Redact"
        },
        {
            regexp: "(First name|Firstname|First_name)",
            detection: "First name",
            action: "Redact"
        },
        {
            regexp: "(Middle name|Middlename|Middle_name)",
            detection: "Middle name",
            action: "Obfuscate"
        },

        {
            regexp: "(last name|lastname|last_name)",
            detection: "Last name",
            action: "Redact"
        },
        {
            regexp: "(Country)",
            detection: "Country",
            action: "Redact"
        },
        {
            regexp: "(state)",
            detection: "State",
            action: "Redact"
        },
        {
            regexp: "(City)",
            detection: "City",
            action: "Redact"
        },
        {
            regexp: "(Zipcode|zip_code|zip code|postalcode|postal_code)",
            detection: "Zipcode",
            action: "Redact"
        },
        {
            regexp: "(Gender)",
            detection: "Gender",
            action: "Redact"
        },
        {
            regexp: "(race)",
            detection: "Race",
            action: "Redact"
        },
        {
            regexp: "(age)",
            detection: "Age",
            action: "Redact"
        },
        {
            regexp: "(job)",
            detection: "Job position",
            action: "Redact"
        },
        {
            regexp: "(Company)",
            detection: "Company",
            action: "Redact"
        },
        {
            regexp: "(work_place|workplace)",
            detection: "Work place",
            action: "Redact"
        },
        {
            regexp: "(Work_Address|WorkAddress)",
            detection: "Work Address",
            action: "Redact"
        },
        {
            regexp: "name",
            detection: "Full Name",
            action: "Redact"
        },
        {
            regexp: "(vin|VehicleIdentificationNumber|Vehicle_Identification_Number)",
            detection: "Vehicle Identification Number",
            action: "Redact"
        },
        {
            regexp: "(address)",
            detection: "Address",
            action: "Redact"
        },
        {
            regexp: "(contact)",
            detection: "Contact Info",
            action: "Redact"
        },

        {
            regexp: "",
            detection: "N/A",
            action: "Allow"
        }
    ]
}
const TopSecret = {
    name: "Top Secret",
    rules: [
        {
            regexp: "homeaddress",
            detection: "Home address",
            action: "Block"
        },
        {
            regexp: "email",
            detection: "Email address",
            action: "Block"
        },
        {
            regexp: "(SSN|ssn|nationalid|national_id)",
            detection: "Social Security Number",
            action: "Block"
        },
        {
            regexp: "(businessentityid|business_entity_id)",
            detection: "Business entity Id",
            action: "Block"
        },
        {
            regexp: "(passport|Passport number|Passport_number)",
            detection: "Passport number",
            action: "Block"
        },
        {
            regexp: "(Driver license|Driver_license|Driverlicense)",
            detection: "Driver license",
            action: "Block"
        },
        {
            regexp: "(Credit card|Credit_card,Creditcard)",
            detection: "Credit card",
            action: "Block"
        },
        {
            regexp: "(dob|dateofbirth|date_of_birth)",
            detection: "Date of birth",
            action: "Block"
        },
        {
            regexp: "(phone|phonenumber|phone_number)",
            detection: "Telephone number",
            action: "Block"
        },

        {
            regexp: "(login|signin|sign_in|username|user_name)",
            detection: "Login details",
            action: "Block"
        },
        {
            regexp: "(serial|serialnumber|serial_number)",
            detection: "Processor or device serial number",
            action: "Block"
        },
        {
            regexp: "(MAC_address|MACaddress)",
            detection: "MAC address",
            action: "Block"
        },
        {
            regexp: "^(ip|ip_address|ipaddress)$",
            detection: "IP address",
            action: "Block"
        },
        {
            regexp: "(device_id|deviceid)",
            detection: "Device ID",
            action: "Block"
        },
        {
            regexp: "(cookie)",
            detection: "Cookie",
            action: "Block"
        },
        {
            regexp: "(Citizenship|nationality)",
            detection: "Citizenship",
            action: "Block"
        },
        {
            regexp: "(visa|immigration)",
            detection: "Visa or immigration status",
            action: "Block"
        },
        {
            regexp: "(Ethnic)",
            detection: "Ethnic background",
            action: "Block"
        },
        {
            regexp: "(Religi)",
            detection: "Religious affiliation",
            action: "Block"
        },
        {
            regexp: "(sexual)",
            detection: "Sexual orientation",
            action: "Block"
        },
        {
            regexp: "(Criminal)",
            detection: "Criminal history",
            action: "Block"
        },
        {
            regexp: "(Medical)",
            detection: "Medical information",
            action: "Block"
        },
        {
            regexp: "(authentication)",
            detection: "Authentication information",
            action: "Block"
        },
        {
            regexp: "(First name|Firstname|First_name)",
            detection: "First name",
            action: "Block"
        },
        {
            regexp: "(Middle name|Middlename|Middle_name)",
            detection: "Middle name",
            action: "Block"
        },

        {
            regexp: "(last name|lastname|last_name)",
            detection: "Last name",
            action: "Block"
        },
        {
            regexp: "(Country)",
            detection: "Country",
            action: "Block"
        },
        {
            regexp: "(state)",
            detection: "State",
            action: "Block"
        },
        {
            regexp: "(City)",
            detection: "City",
            action: "Block"
        },
        {
            regexp: "(Zipcode|zip_code|zip code|postalcode|postal_code)",
            detection: "Zipcode",
            action: "Block"
        },
        {
            regexp: "(Gender)",
            detection: "Gender",
            action: "Block"
        },
        {
            regexp: "(race)",
            detection: "Race",
            action: "Block"
        },
        {
            regexp: "(age)",
            detection: "Age",
            action: "Block"
        },
        {
            regexp: "(job)",
            detection: "Job position",
            action: "Block"
        },
        {
            regexp: "(Company)",
            detection: "Company",
            action: "Block"
        },
        {
            regexp: "(work_place|workplace)",
            detection: "Work place",
            action: "Block"
        },
        {
            regexp: "(Work_Address|WorkAddress)",
            detection: "Work Address",
            action: "Block"
        },
        {
            regexp: "name",
            detection: "Full Name",
            action: "Block"
        },
        {
            regexp: "(vin|VehicleIdentificationNumber|Vehicle_Identification_Number)",
            detection: "Vehicle Identification Number",
            action: "Block"
        },
        {
            regexp: "(address)",
            detection: "Address",
            action: "Block"
        },
        {
            regexp: "(contact)",
            detection: "Contact Info",
            action: "Block"
        },

        {
            regexp: "",
            detection: "N/A",
            action: "Allow"
        },
    ]
}

const Prefills = {
    unclassified: PrefillUnclassified,
    confidential: Confidential,
    secret: Secret,
    topsecret: TopSecret
}
export interface AddTableProps {
    table: types.TableScope,
    connectionId: string,
    onAddTable: (ar: types.TableScope) => void,
    onAlert: (ar: JSX.Element) => void,
    onHide: () => void
}
const AddTable: React.FC<AddTableProps> = (props) => {
    const [validated, setValidated] = useState(false)
    const [database, setDatabase] = useState({})
    const [schema, setSchema] = useState("")
    const [table, setTable] = useState("")
    const [dummy, setDummy] = useState(true)
    const [level, setLevel] = useState("")

    let emptyarray: any[] = []
    const [tablestructure, setTableStructure] = useState(emptyarray)

    let tablestate = useRef(emptyarray)
    if (tablestate.current !== undefined) {
        tablestate.current = tablestructure
    }
    let form = useRef<HTMLFormElement>(null)


    let handleSubmit = event => {
        if (form.current == null) {
            return false
        }

        if (form.current.reportValidity() === false) {
            event.preventDefault();
            setValidated(true)
            setDummy(!dummy)
            return false
        }

        event.preventDefault();
        setValidated(false)
        event.stopPropagation();
        props.onAddTable({ schema, table, tablescope: [...tablestate.current] })
        setTableStructure(emptyarray)
        tablestate.current = emptyarray
        setSchema("")
        setTable("")
        return false
    }
    useEffect(() => {
        if (props.table.connection !== undefined && props.table.connection !== "") {
            setSchema(props.table.schema)
            setTable(props.table.table)
            setTableStructure(cloneDeep(props.table.tablescope))
        } else {
            let body = JSON.stringify({
                ConnectionId: props.connectionId
            })
                http.sendToServer("POST", "/api/queryconnection",
                null, body,
                resp => {
                    resp.json().then(js => {
                        if (js.status !== "OK") {
                            props.onAlert(<Alert variant="danger" onClose={() => props.onAlert(<></>)} dismissible>
                                {js.errormessage}
                            </Alert>)
                            props.onHide()
                            //setSpinner(false)
                            return
                        }
                        
                        setDatabase(js.database)
                        if (props.table.schema !== undefined && props.table.table !== undefined) {
                            setSchema(props.table.schema)
                            setTable(props.table.table)
                        }
                        //setSpinner(false)

                    }).catch((error) => {
                        console.log("exception: ", error.message)
                    })
                },
                resp => {
                    console.log("on error")
                    //setSpinner(false)
                },
                error => {
                    console.log("on exception: ", error.message)
                    //setSpinner(false)

                })
        }

    }, [])


    let getOptions = () => {
        if (database["schemas"] === undefined) {
            return []
        }
        let schemas = database["schemas"].filter(x => !x.isSystem).map(x => {
            return x.name
        })
        return schemas
    }
    let selectSchema = (schema: any) => {
        if (schema[0] === undefined) {
            setSchema("")
        } else {
            setSchema(schema[0].toString())
        }
        setTable("")
    }
    let selectTable = (table: any) => {
        if (table.length === 0) {
            setTable("")
            return
        }
        setTable(table[0].toString())
    }
    useEffect(() => {
        if (props.table.connection === undefined || props.table.connection === "") {
            initTableSchema()
        }
    }, [table])
    let getTables = () => {
        if (database["schemas"] === undefined)
            return []
        let schemas = database["schemas"]
        let tables: any[] = []
        schemas.map(x => {
            if (x.name == schema) {
                tables = x.tables
            }
        })
        return tables.filter(x => !x.isSystem).map(x => {
            return x.name
        })
    }
    let selectPII = (rowIndex) => {
        return event => {
            tablestate.current[rowIndex].semantics = event[0]
            setTableStructure(tablestate.current)
        }
    }

    let selectAction = (rowIndex) => {
        return event => {
            tablestate.current[rowIndex].action = event[0]
            setTableStructure(tablestate.current)
        }
    }

    let schemacolumns = [
        {
            dataField: 'position',
            text: 'position',
            hidden: true,
        },
        {
            dataField: 'name',
            text: 'Column',
            classes: 'overflow-hidden'
        },
        {
            dataField: 'typ',
            text: 'Type:',
        },
        {
            dataField: 'semantics',
            text: 'PII',
            formatter: (cell, row, rowIndex, formatExtraData) => {
                let pattern = "^(" + PIIs.join("|") + ")$"
                return <Typeahead
                    id={"semantics" + rowIndex}
                    inputProps={{ required: true, pattern, id:"semantics" + rowIndex }}
                    key={"semantics" + rowIndex + validated}
                    onChange={selectPII(rowIndex)} size="sm"
                    options={PIIs}
                    defaultSelected={row.semantics !== undefined && row.semantics !== "" ? [row.semantics] : []}
                    placeholder="Data type..."
                    clearButton
                />
            }
        },
        {
            dataField: 'action',
            text: 'Action',
            formatter: (cell, row, rowIndex, formatExtraData) => {
                let pattern = "^(" + Actions.join("|") + ")$"
                return <Typeahead
                    id={"action" + rowIndex}
                    inputProps={{
                        required: true,
                        pattern, id: "action" + rowIndex
                    }}
                    key={"action" + rowIndex + validated}
                    onChange={selectAction(rowIndex)} size="sm"
                    options={Actions}
                    defaultSelected={row.action !== undefined && row.action !== "" ? [row.action] : []}
                    clearButton
                    placeholder="Access..."
                />
            }
        }
    ]


    let showTableSchema = () => {
        let schemas = database["schemas"]
        if (schemas === undefined)
            return []
        let tbls: any[] = []
        schemas.map(x => {
            if (x.name == schema) {
                tbls = x.tables
            }
        })
        let t

        tbls.map(x => {
            if (x.name === table)
                t = x.columns
        })

        if (t === undefined)
            return []

        let retval = t.map(x => {

            return { position: x.position, name: x.name, typ: x.typ, semantics: x.semantics != null ? x.semantics : "", reference: x.reference, action: "", dflt: x["default"], isnullable: x.isNullable }
        })

        return retval
    }
    let initTableSchema = () => {
        let s = showTableSchema()
        setTableStructure(s)
    }
    let onPrefill = (e) => {
        setLevel(e.target.value)
    }
    let prefills = () => {
        let ret: any[] = []

        Object.keys(Prefills).forEach(key => {
            ret.push(<option data-testid={key} key={key} value={key} >{Prefills[key].name}</option>)
        }
        )
        return ret
    }
    let ActionByName = (predo, name: string) => {
        let action = "N/A"
        for (let i = 0; i < predo.rules.length; i++) {
            let re = new RegExp(predo.rules[i].regexp, "i")
            if (name.match(re) != null) {

                return [predo.rules[i].action, predo.rules[i].detection]
            }
        }
        return []
    }
    let applyPrefill = () => {
        if (level === "")
            return
        let newtablestructure = cloneDeep(tablestructure)

        let predo = Prefills[level]
        for (let i = 0; i < newtablestructure.length; i++) {
            let table = newtablestructure[i]

            let [action, semantics] = ActionByName(predo, newtablestructure[i].name)
            newtablestructure[i].action = action
            newtablestructure[i].semantics = semantics

        }

        setTableStructure(newtablestructure)
    }
    
    return <div>

        <Form onSubmit={handleSubmit} ref={form} noValidate validated={validated}>

            <Row>
                <Col xs="auto">
                    <Form.Group className="mb-3" controlId="schemaname">
                        <Form.Label>Schema Name:</Form.Label>
                        <Typeahead id="schemaname" inputProps={{id: "schemaname"}}
                            onChange={selectSchema} size="sm"
                            defaultSelected={props.table.schema != undefined ? [props.table.schema] : []}
                            options={getOptions()}
                            clearButton
                            placeholder="Choose schema..."
                            disabled={props.table.connection !== undefined && props.table.connection !== ""}
                        />

                        <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                        <Form.Control.Feedback type="invalid" >
                            Client side name for Dymium database
                        </Form.Control.Feedback>
                    </Form.Group>
                </Col>
                <Col>
                    {schema !== "" && schema != undefined && table !== "" && table != undefined && tablestructure != [] &&
                        <Form.Group className="mb-3" controlId="connection" >
                            <Form.Label >Select Detect and Prefill Level</Form.Label>
                            <InputGroup>
                                <Form.Control as="select" size="sm" role="combobox" data-testid="seclevel"
                                    onChange={onPrefill}
                                >
                                    <option key="xx" value="">...</option>

                                    {prefills()}
                                </Form.Control>
                                <Button data-testid="fill-security" onClick={applyPrefill} variant="dymium" disabled={level === ""} className="mr-1" style={{ marginTop: '0.0em' }} size="sm"><i className="fa-solid fa-table mr-2"></i>Fill</Button>
                            </InputGroup>
                        </Form.Group>
                    }
                </Col>
            </Row>
            {schema !== "" && schema != undefined &&
                <Row>
                    <Col xs="auto">
                        <Form.Group className="mb-3" controlId="dbname">
                            <Form.Label>Table Name:</Form.Label>
                            <Typeahead id="tables" inputProps={{id: "tables"}} onChange={selectTable} size="sm"
                                clearButton
                                options={getTables()}
                                defaultOpen={false}
                                labelKey="Table"
                                placeholder="Choose table..."
                                selected={table != undefined ? [table] : []}
                                disabled={props.table.connection !== undefined && props.table.connection !== ""}
                            />

                            <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                            <Form.Control.Feedback type="invalid" >
                                Client side name for Dymium database
                            </Form.Control.Feedback>
                        </Form.Group>
                    </Col>
                    <Col>

                    </Col>
                </Row>
            }
            {schema !== "" && schema != undefined && table !== "" && table != undefined && tablestructure != [] &&
                <>
                    <BootstrapTable id="schematable"
                        condensed
                        striped bordered={false}
                        bootstrap4
                        keyField='name'
                        data={tablestructure}
                        columns={schemacolumns}
                    />


                    <Button data-testid="apply-structure" variant="dymium" size="sm" className="mt-4" type="submit">
                        Apply
                    </Button>
                </>

            }
        </Form>


    </div>
}

export default AddTable