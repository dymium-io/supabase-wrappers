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

import {PrefillUnclassified, Confidential, Secret, TopSecret} from "./Detectors"
export const Prefills = {
  unclassified: PrefillUnclassified,
  confidential: Confidential,
  secret: Secret,
  topsecret: TopSecret
}

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

export interface AddTableProps {
    table: types.TableScope,
    connectionId: string,
    currentConnectionType: string,
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
    let correctname = ["MariaDB", "MySQL"].includes(props.currentConnectionType)? "Database" : "Schema" 
    return <div>

        <Form onSubmit={handleSubmit} ref={form} noValidate validated={validated}>

            <Row>
                <Col xs="auto">
                    <Form.Group className="mb-3" controlId="schemaname">
                        <Form.Label>{correctname} Name:</Form.Label>
                        <Typeahead id="schemaname" inputProps={{id: "schemaname"}}
                            onChange={selectSchema} size="sm"
                            defaultSelected={props.table.schema != undefined ? [props.table.schema] : []}
                            options={getOptions()}
                            clearButton
                            placeholder={`Choose ${correctname}...`}
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