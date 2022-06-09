import React, { useEffect, useState, useRef } from 'react';
import Tabs from 'react-bootstrap/Tabs'
import Tab from 'react-bootstrap/Tab'
import Form from 'react-bootstrap/Form'
import Button from 'react-bootstrap/Button'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'
import Offcanvas from '@dymium/common/Components/Offcanvas'
import { Typeahead } from 'react-bootstrap-typeahead';
import Modal from 'react-bootstrap/Modal'
import Alert from 'react-bootstrap/Alert'
import BootstrapTable from 'react-bootstrap-table-next';
import cloneDeep from 'lodash/cloneDeep';
import Spinner from '@dymium/common/Components/Spinner'
import * as com from '../Common'

/*
const testJSON = {
    name: "My test database",
    schemas: [
        {
            name: "global",
            tables: [
                {
                    name: "customers",
                    columns: [
                        {
                            name: "id",
                            position: 1,
                            typ: "varchar 60"
                        },
                        {
                            name: "name",
                            position: 2,
                            typ: "varchar 60"
                        },
                        {
                            name: "org",
                            position: 3,
                            typ: "varchar 60"
                        },
                        {
                            name: "domain",
                            position: 4,
                            typ: "varchar 128"
                        },
                    ]
                },
                {
                    name: "billing",
                    columns: [
                        {
                            name: "id",
                            position: 1,
                            typ: "varchar 60"
                        },
                        {
                            name: "invoicecount",
                            position: 2,
                            typ: "integer"
                        },
                        {
                            name: "balance",
                            position: 3,
                            typ: "integer"
                        },
                        {
                            name: "duedate",
                            position: 4,
                            typ: "date"
                        },
                    ]
                }
            ]
        }, {
            name: "spoofcorp",
            tables: [
                {
                    name: "users",
                    columns: [
                        {
                            name: "id",
                            position: 1,
                            typ: "varchar 60"
                        },
                        {
                            name: "Firstname",
                            position: 2,
                            typ: "varchar 60"
                        },
                        {
                            name: "Secondname",
                            position: 3,
                            typ: "varchar 60"
                        },
                        {
                            name: "email",
                            position: 4,
                            typ: "varchar 60"
                        },
                        {
                            name: "username",
                            position: 5,
                            typ: "varchar 128"
                        },
                        {
                            name: "password",
                            position: 6,
                            typ: "varchar 128"
                        },
                    ]
                }, {
                    name: "projects",
                    columns: [
                        {
                            name: "id",
                            position: 1,
                            typ: "varchar 60"
                        },
                        {
                            name: "name",
                            position: 2,
                            typ: "varchar 60"
                        },
                        {
                            name: "userid",
                            position: 3,
                            typ: "varchar 60"
                        },
                        {
                            name: "description",
                            position: 4,
                            typ: "varchar 600"
                        },

                    ]
                }
            ]

        }
    ]
}
*/
const PIIs = [
    "N/A",
    "Address",
    "Email",
    "SSN",
    "Member ID",
    "Name"
]
const Actions = [
    "Allow",
    "Block",
    "Full Redact",
    "Obfuscate",
    "Smart Reduct"
]
export default function AddTable(props) {
    const [validated, setValidated] = useState(false)
    const [database, setDatabase] = useState({})
    const [schema, setSchema] = useState("")
    const [table, setTable] = useState("")
    const [dummy, setDummy] = useState(true)

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
        props.onAddTable({ schema, table, tablescope:[...tablestate.current]})
        setTableStructure(emptyarray)
        tablestate.current = emptyarray
        setSchema("")
        setTable("")
        return false
    }
    useEffect(() => {
       
        if(props.table.connection !== undefined) {
            setSchema(props.table.schema)
            setTable(props.table.table)
            setTableStructure(cloneDeep(props.table.tablescope))
        } else {
            console.log("connectionId", props.connectionId)
            let body = JSON.stringify({
                ConnectionId: props.connectionId
            })
            com.sendToServer("POST", "/api/queryconnection",
            null, body,
            resp => {
                resp.json().then(js => {
                    setDatabase(js)

                    //setSpinner(false)

                }).catch((error) => {

                })
            },
            resp => {
                console.log("on error")
                //setSpinner(false)


            },
            error => {
                console.log("on exception")
                //setSpinner(false)

            })
        }

    }, [])
    useEffect(() => {
        if (props.clear) {
            tablestate.current = emptyarray
            setTableStructure(emptyarray)
        }
    }, [props.clear])

    let getOptions = () => {
        if(database["schemas"] === undefined) {
            return []
        }
        let schemas = database["schemas"].map(x => {
            return x.name
        })
        return schemas
    }
    let selectSchema = schema => {
        setSchema(schema[0])
    }
    let selectTable = table => {
        setTable(table[0])
    }
    useEffect(() => {
        debugger
        if(props.table.connection === undefined) {
            initTableSchema()
        }
    }, [table])
    let getTables = () => {
        if(database["schemas"] === undefined)
            return []
        let schemas = database["schemas"]
        let tables: any[] = []
        schemas.map(x => {
            if (x.name == schema) {
                tables = x.tables
            }
        })
        return tables.map(x => {
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
                    inputProps={{ required: true, pattern }}
                    key={"semantics" + rowIndex + validated}
                    onChange={selectPII(rowIndex)} size="sm"
                    options={PIIs}
                    defaultSelected={row.semantics !== undefined && row.semantics !== ""  ? [row.semantics] : []}
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
                        pattern
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
        if(schemas === undefined)
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

            return { position: x.position, name: x.name, typ: x.typ, semantics: x.semantics != null ? x.semantics : "", action: "" }
        })

        return retval
    }
    let initTableSchema = () => {
        let s = showTableSchema()
        setTableStructure(s)
    }
    return <div>

        <Form onSubmit={handleSubmit} ref={form} noValidate validated={validated}>

            <Row>
                <Col xs="auto">
                    <Form.Group className="mb-3" controlId="dbname">
                        <Form.Label>Schema Name</Form.Label>
                        <Typeahead id="schemas"
                            onChange={selectSchema} size="sm"
                            selected={[schema]}
                            options={getOptions()}

                            placeholder="Choose schema..."
                        />

                        <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                        <Form.Control.Feedback type="invalid" >
                            Client side name for Dymium database
                        </Form.Control.Feedback>
                    </Form.Group>
                </Col>
            </Row>
            {schema !== "" &&
                <Row>
                    <Col xs="auto">
                        <Form.Group className="mb-3" controlId="dbname">
                            <Form.Label>Table Name</Form.Label>
                            <Typeahead id="tables" onChange={selectTable} size="sm"

                                options={getTables()}
                                defaultOpen={false}
                                labelKey="Table"
                                placeholder="Choose table..."
                                selected={[table]}
                            />

                            <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                            <Form.Control.Feedback type="invalid" >
                                Client side name for Dymium database
                            </Form.Control.Feedback>
                        </Form.Group>
                    </Col>
                </Row>
            }
            {schema !== "" && table != "" &&
                <>               
                 <BootstrapTable id="schematable"
                    condensed
                    striped bordered={false}
                    bootstrap4
                    keyField='name'
                    data={tablestructure}
                    columns={schemacolumns}
                />


                    <Button variant="dymium" size="sm" className="mt-4" type="submit">
                        Apply
                    </Button>
                </>

            }
        </Form>


    </div>
}
