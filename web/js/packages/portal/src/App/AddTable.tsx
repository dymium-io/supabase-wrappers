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
import * as ctypes from '@dymium/common/Types/Common'
import * as dba from '@dymium/common/Types/DbAnalyzer'
import * as http from '@dymium/common/Api/Http'

import { PrefillUnclassified, Confidential, Secret, TopSecret } from "./Detectors"
export const DefaultPrefills = {
    unclassified: PrefillUnclassified,
    confidential: Confidential,
    secret: Secret,
    topsecret: TopSecret
}


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

type SubRule = {
    regexp: string,
    semantics: string,
    detection: string,
    action: string
}
type RuleSet = {
    name: string,
    rules: SubRule[]
}

type Formatter = (cell: any, row: any, rowIndex: any, formatExtraData: any) => JSX.Element
type TableColumn = {
    dataField: string;
    text: string;
    hidden?: boolean;
    classes?: string;
    formatter?: any;
}
type PiiPair = {
    id: string;
    label: string;
}
let Id2Label = (x: string) => {
    let opts: ctypes.DataHandling[] = ["allow", "block", "obfuscate", "redact"]
    for (let i = 0; i < opts.length; i++) {
        if (x == opts[i])
            return ctypes.humanReadableDataHandling(opts[i])
    }
    return ""
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
    const [tables, setTables] = useState<dba.Column[]>([])
    const [schema, setSchema] = useState("")
    const [table, setTable] = useState("")
    const [dummy, setDummy] = useState(true)
    const [level, setLevel] = useState("")
    const [spinner, setSpinner] = useState(false)
    const [Prefills, setPrefills] = useState<Object>(DefaultPrefills)
    const [policy, setPolicy] = useState(new ctypes.DataPolicy())
    const [PIIs, setPIIs] = useState<PiiPair[]>([])
    const [tabledef, setTabledef] = useState<TableColumn[]>([])
    let emptyarray: any[] = []
    const [tablestructure, setTableStructure] = useState(emptyarray)


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

        props.onAddTable({ schema, table, tablescope:  tablestructure})
        setTableStructure(emptyarray)
  
        setSchema("")
        setTable("")
        return false
    }

    let createLevel = (policy: ctypes.DataPolicy, index: number) => {
        let r: RuleSet = { name: policy.actions[index].role, rules: [] }

        return r
    }
    let createLevels = (policy: ctypes.DataPolicy) => {
        let Prefills = {}
        for (let i = 0; i < policy.actions.length; i++) {
            let role = policy.actions[i].role
            Prefills[role] = createLevel(policy, i)
            for (let j = 0; j < policy.piisuggestions.length; j++) {

                let rule: SubRule = {
                    detection: policy.piisuggestions[j].detector.name,
                    semantics: policy.piisuggestions[j].detector.id!,
                    regexp: policy.piisuggestions[j].detector.data,
                    action: policy.piisuggestions[j].actions[i].handling
                }
                Prefills[role].rules.push(rule)
            }
            let defaultrule: SubRule = {
                regexp: "",
                semantics: "",
                detection: "N/A",
                action: "Allow"
            }
            Prefills[role].rules.push(defaultrule)
        }
        setPrefills(Prefills)
    }
    let getConnections = () => {
        if (props.table.connection !== undefined && props.table.connection !== "") {
            setSchema(props.table.schema)
            setTable(props.table.table)
 
            setTableStructure(tablestructure => cloneDeep(props.table.tablescope))
        } else {
            let body = JSON.stringify({
                ConnectionId: props.connectionId
            })
            setSpinner(true)
            http.sendToServer("POST", "/api/queryconnection",
                null, body,
                resp => {
                    resp.json().then(js => {
                        if (js.status !== "OK") {
                            props.onAlert(<Alert variant="danger" onClose={() => props.onAlert(<></>)} dismissible>
                                {js.errormessage}
                            </Alert>)
                            props.onHide()
                            setSpinner(false)
                            return
                        }

                        setDatabase(js.response.dbInfo)
                        if (props.table.schema !== undefined && props.table.table !== undefined) {
                            setSchema(schema => {
                                setTable(props.table.table)
                                return props.table.schema})
                        }
                        setSpinner(false)


                    }).catch((error) => {
                        console.log("exception: ", error.message)
                        setSpinner(false)
                    })
                },
                resp => {
                    console.log("on error")
                    setSpinner(false)
                    resp != null && resp.text().then(t =>
                        props.onAlert(<Alert variant="danger" onClose={() => props.onAlert(<></>)} dismissible>Query Connection failed: {t}</Alert>
                        ))
                },
                error => {
                    console.log("on exception: ", error.message)
                    setSpinner(false)

                })
        }        
    }
    let getPolicies = () => {

        //setSpinner(true)
        http.sendToServer("GET", "/api/getpolicies",
            null, "",
            resp => {
                resp.json().then(js => {
                    //setSpinner(false)                    
                    if (js.error === undefined) {
                        let po = ctypes.DataPolicy.fromJson(js)
                        setPolicy(po)
                        createLevels(po)
                        let newPIIs:PiiPair[] = [{id: "", label: "N/A"}].concat(po.piisuggestions.map(x => { 
                                let id = ""
                                if(x.detector.id != null)   
                                    id = x.detector.id
                                return {id, label: x.detector.name}
                            }))
                        setPIIs(PIIs => newPIIs)
                    } else {
                        props.onAlert(<Alert variant="danger" onClose={() => props.onAlert(<></>)} dismissible>
                            Error: {js.error}
                            <br/>
                            Check if you defined any rules and access levels!
                        </Alert>)                        
                        setPrefills(DefaultPrefills)
                    }
               
                }).catch((error) => {

                })
            },
            resp => {

            },
            error => {

            })
    }
    useEffect(() => {
        getPolicies()


    }, [])
    useEffect(() => {
        if(tablestructure.length !== 0)
            setTableStructure(tables)

    }, [tables])
    
    let getSemanticsFromId = (semantics) => {
        if(semantics === '' || semantics == null) return "N/A"

        for (let i = 0; i < policy.piisuggestions.length; i++) {
            if (policy.piisuggestions[i].detector.id === semantics) {
                return policy.piisuggestions[i].detector.name
            }
        }
        return "N/A"
    }

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
    let _selectTable = (table: any) => {
        if (table.length === 0) {
            setTable("")
            return
        }
        setTable(table[0].toString())
    }

    let selectTable = (table: any[] ) => {
        if(table.length === 0 ) {
            setTables(tables => [])
            return
        }
       let b = {
            ConnectionId: props.connectionId,
            Schema: schema,
            Table: table[0]
        }
        if(schema === "" && props.table.schema !== "") {
            b.Schema =  props.table.schema
        }
        let body = JSON.stringify(b)
        setSpinner(true)
        http.sendToServer("POST", "/api/querytable",
            null, body,
            resp => {
                resp.json().then(js => {
                    if (js.status !== "OK") {
                        window.document.dispatchEvent(new Event('reauthenticate'));
                        props.onAlert(<Alert variant="danger" onClose={() => props.onAlert(<></>)} dismissible>
                            {js.errormessage}
                        </Alert>)
                        props.onHide()
                        setSpinner(false)
                        return
                    }
                    setTables(tables => { 
                        setTable(table => {
                            setTableStructure(js.response.tblInfo.columns)
                            return js.response.tblInfo.tblName
                        }) ; 
                        return js.response.tblInfo.columns })
                    

                    
                    /*
                                        setDatabase(js.response.dbInfo)
                                        if (props.table.schema !== undefined && props.table.table !== undefined) {
                                            setSchema(props.table.schema)
                                            setTable(props.table.table)
                                        }
                                        */
                    setSpinner(false)

                }).catch((error) => {
                    console.log("exception: ", error.message)
                    setSpinner(false)
                })
            },
            resp => {
                console.log("on error")
                setSpinner(false)
                resp != null && resp.text().then(t =>
                    props.onAlert(<Alert variant="danger" onClose={() => props.onAlert(<></>)} dismissible>Query Connection failed: {t}</Alert>
                    ))
            },
            error => {
                console.log("on exception: ", error.message)
                setSpinner(false)

            })
    }

    useEffect(() => {
        if(tablestructure.length == 0)
            return
        if(PIIs.length == 0)
            return
            

        let schemacolumns: TableColumn[] = [
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
                text: 'PII:',
                formatter: (cell, row, rowIndex, formatExtraData) => {

                    let pattern = "^(" + PIIs.map(x => x.label).join("|") + ")$"
                    let def = row.semantics !== undefined && row.semantics !== "" ? [{ id: row.semantics, label: getSemanticsFromId(row.semantics) }] : [{id:"", label:"N/A"}]
                    return <Typeahead
                        id={"semantics" + rowIndex}
                        inputProps={{ required: true, pattern, id: "semantics" + rowIndex }}
                        key={"semantics" + rowIndex + validated}
                        onChange={selectPII(rowIndex)} size="sm"
                        options={PIIs}
                        defaultSelected={def}
                        placeholder="Data type..."
                        clearButton
                    />
                }
            },
            {
                dataField: 'action',
                text: 'Action:',
                formatter: (cell, row, rowIndex, formatExtraData) => {
                    let possibleActions = row.possibleActions
                    if (possibleActions == undefined)
                        possibleActions = []
                    let possible: string[] = cloneDeep(possibleActions)
                    // possible.push("allow")
                    let pattern = "^(" + possible.map(x => Id2Label(x)).join("|") + ")$"

                    return <Typeahead
                        id={"action" + rowIndex}
                        inputProps={{
                            required: true,
                            pattern, id: "action" + rowIndex
                        }}
                        key={"action" + rowIndex + validated}
                        onChange={selectAction(rowIndex)} size="sm"
                        options={possible.map(x => {
                            return { id: x, label: Id2Label(x) }
                        })
                        }
                        defaultSelected={row.action !== undefined && row.action !== "" ? [{ id: row.action, label: Id2Label(row.action) }] : []}
                        clearButton
                        placeholder="Access..."
                    />
                }
            }
        ]

        setTabledef(tabledef => cloneDeep(schemacolumns) )
    }, [table, tablestructure])

    useEffect(() => {
        if(PIIs.length !== 0)
           getConnections()
    }, [PIIs])

    useEffect(() => {
        if (props.table.connection === undefined || props.table.connection === "") {
            initTableSchema()
        }
        if(table !== "" && tables.length === 0 && (props.table.tablescope === undefined)) {
            selectTable([table])
        }
    }, [table])
    useEffect(() => {

       if ( schema !== "" && table === "" ) {
        //    setTable(props.table.table)
       }
    }, [schema])
    console.log(">>>", schema, PIIs.length, tablestructure.length, tabledef.length)
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
            setTableStructure(tablestructure => {
                let t = cloneDeep(tablestructure)
                if(event.length === 0 || event[0].id == undefined) {
                    t[rowIndex].semantics = ""
                } else {
                    t[rowIndex].semantics = event[0].id
                }
                return t
            })
            setTabledef(tabledef => cloneDeep(tabledef))
        }
    }

    let selectAction = (rowIndex) => {
        return event => {
            setTableStructure(tablestructure => {
                let t = cloneDeep(tablestructure)
                if(event.length === 0 || event[0].id == undefined) {
                    t[rowIndex].action = ""
                } else {                
                    t[rowIndex].action = event[0].id
                }
                return t
            })
            setTabledef(tabledef => cloneDeep(tabledef))
        }
    }

    let showTableSchema = () => {

        if (tables == null)
            return []

        let retval = tables.map(x => {

            return {
                position: x.position, name: x.name, typ: x.typ, semantics: x.semantics != null ? x.semantics : "",
                reference: x.reference, action: "", dflt: x["default"], isnullable: x.isNullable, possibleActions: x.possibleActions
            }
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
    let ActionByName = (predo, semantics: string) => {
        let action = "N/A"
        for (let i = 0; i < predo.rules.length; i++) {
            
            if (semantics === predo.rules[i].semantics) {

                return [predo.rules[i].action.toLowerCase(), predo.rules[i].id]
            }
        }
        return []
    }
    let sessionGetTablestructure = () => {
        return tablestructure
    }
    let applyPrefill = () => {
        if (level === "")
            return
        let newtablestructure = cloneDeep( sessionGetTablestructure() )

        let predo = Prefills[level]
        
        for (let i = 0; i < newtablestructure.length; i++) {
            let table = newtablestructure[i]

            let [action, semantics] = ActionByName(predo, newtablestructure[i].semantics)
            let possible: string[] = cloneDeep(table.possibleActions)
            //possible.push("allow")

            if (possible.includes(action))
                newtablestructure[i].action = action
            else
                newtablestructure[i].action = possible[possible.length - 1]

        }

        setTableStructure(tablestructure => newtablestructure)
    }
    let correctname = ["MariaDB", "MySQL"].includes(props.currentConnectionType) ? "Database" : "Schema"
   
    return <div>

        <Form onSubmit={handleSubmit} ref={form} noValidate validated={validated}>

            <Row>
                <Col xs="auto" className="mr-0 pr-0">
                    <Form.Group className="mb-3" controlId="schemaname">
                        <Form.Label>{correctname} Name:</Form.Label>
                        <Typeahead id="schemaname" inputProps={{ id: "schemaname" }}
                            onChange={selectSchema} size="sm"
                            defaultSelected={props.table.schema != undefined ? [props.table.schema] : []}
                            options={getOptions()}
                            clearButton
                            data-testid="schemaname"
                            placeholder={`Choose ${correctname}...`}
                            disabled={props.table.connection !== undefined && props.table.connection !== ""|| (props.table.table !== "")}
                        />

                        <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                        <Form.Control.Feedback type="invalid" >
                            Client side name for Dymium database
                        </Form.Control.Feedback>
                    </Form.Group>
                </Col>

                <Col xs="auto" className="ml-0 pl-0"><Spinner show={spinner} style={{ marginTop: '26px', width: '28px' }}></Spinner></Col>

                <Col>
                    {schema !== "" && schema != undefined && table !== "" && table != undefined && tablestructure != [] &&
                        <Form.Group className="mb-3" controlId="connection" >
                            <Form.Label >Select Access Level:</Form.Label>
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
                            <Typeahead id="tables" inputProps={{ id: "tables" }} onChange={_selectTable} size="sm"
                                clearButton
                                options={getTables()}
                                defaultOpen={false}
                                labelKey="Table"
                                placeholder="Choose table..."
                                defaultSelected={table != undefined ? [table] : []}
                                disabled={ (props.table.connection !== undefined && props.table.connection !== "") || (props.table.table !== "")}
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
            {schema !== "" && schema != undefined && table !== "" && table != undefined && 
            PIIs.length != 0 && tablestructure.length !== 0 && tabledef.length !== 0 &&
                <>
                    <BootstrapTable id="schematable"
                        condensed
                        striped bordered={false}
                        bootstrap4
                        keyField='name'
                        data={tablestructure}
                        columns={tabledef}
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