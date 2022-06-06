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
import Spinner from '@dymium/common/Components/Spinner'
import * as com from '../Common'


let remap = {}
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
function AddTable(props) {
    const [validated, setValidated] = useState(false)
    const [database, setDatabase] = useState({})
    const [schema, setSchema] = useState("")
    const [table, setTable] = useState("")

    let form = useRef<HTMLFormElement>(null)

    let testJSON = {
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
                                position: 0,
                                typ: "varchar 60"
                            },
                            {
                                name: "name",
                                position: 1,
                                typ: "varchar 60"
                            },
                            {
                                name: "org",
                                position: 2,
                                typ: "varchar 60"
                            },
                            {
                                name: "domain",
                                position: 3,
                                typ: "varchar 128"
                            },
                        ]
                    },
                    {
                        name: "billing",
                        columns: [
                            {
                                name: "id",
                                position: 0,
                                typ: "varchar 60"
                            },
                            {
                                name: "invoicecount",
                                position: 1,
                                typ: "integer"
                            },
                            {
                                name: "balance",
                                position: 2,
                                typ: "integer"
                            },
                            {
                                name: "duedate",
                                position: 3,
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
                                position: 0,
                                typ: "varchar 60"
                            },
                            {
                                name: "Firstname",
                                position: 1,
                                typ: "varchar 60"
                            },
                            {
                                name: "Secondname",
                                position: 1,
                                typ: "varchar 60"
                            },
                            {
                                name: "email",
                                position: 2,
                                typ: "varchar 60"
                            },
                            {
                                name: "username",
                                position: 3,
                                typ: "varchar 128"
                            },
                            {
                                name: "password",
                                position: 4,
                                typ: "varchar 128"
                            },
                        ]
                    }, {
                        name: "projects",
                        columns: [
                            {
                                name: "id",
                                position: 0,
                                typ: "varchar 60"
                            },
                            {
                                name: "name",
                                position: 1,
                                typ: "varchar 60"
                            },
                            {
                                name: "userid",
                                position: 1,
                                typ: "varchar 60"
                            },
                            {
                                name: "description",
                                position: 2,
                                typ: "varchar 600"
                            },

                        ]
                    }
                ]

            }
        ]
    }

    let handleSubmit = e => {
        return false
    }
    useEffect(() => {
        setDatabase(testJSON)
    
    }, [])
    let getOptions = () => {
        let schemas = testJSON.schemas.map( x => {
            return x.name
        })
        console.log(schemas)
        return schemas
    }    
    let selectSchema = schema => {
       setSchema(schema[0])
    }
    let selectTable = table => {
        setTable(table[0])
     }
    let getTables = () => {
        let schemas = testJSON.schemas
        let tables:any[] = []
        schemas.map( x => {
            if(x.name == schema) {
                tables = x.tables
            }
        })

        return tables.map( x => {
            return x.name
        })
    }    
    let selectPII = (rowIndex) => {
        return event => {
            // TODO
        }
    }
    let selectAction = (rowIndex) => {
        return event => {
            // TODO
        }
    }    
    let schemacolumns =  [
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
                    return <Typeahead 
                    id={"semantics"+rowIndex} onChange={selectPII(rowIndex)} size="sm" 
                       
                    options={PIIs}
                    
                    placeholder="Choose schema..."   
                    />
                },                

            },         
            {
                dataField: 'action',
                text: 'Action',
                formatter: (cell, row, rowIndex, formatExtraData) => {
                    return <Typeahead 
                    id={"action"+rowIndex} onChange={selectAction(rowIndex)} size="sm" 
                       
                    options={Actions}
                    
                    placeholder="Choose action..."   
                    />
                }
            }                       
            /*
            ,
            {
                dataField: 'dbtype',
                text: 'DB Type:',
                formatter: (cell, row, rowIndex, formatExtraData) => {
                    return com.databaseTypes[row["dbtype"]]
                },
                sortValue: (cell, row) => {
                    return com.databaseTypes[row["dbtype"]]
                },
                sort: true
            }, */
        ]
    let showTableSchema = () => {
        let schemas = testJSON.schemas
        let tables:any[] = []
        schemas.map( x => {
            if(x.name == schema) {
                tables = x.tables
            }
        })
        let t 
        
        tables.map(x => {
            if(x.name === table)
                t = x.columns
        }) 

        if(t === undefined)
            return []
        let retval = t.map(x => {
            
            return {position: x.position, name: x.name, typ: x.typ, semantics: x.semantics, action: ""}
        })
        return retval
    }

    return <div>

        <Form onSubmit={handleSubmit} ref={form} noValidate validated={validated}>
        
            <Row>
                <Col xs="auto">
                    <Form.Group className="mb-3" controlId="dbname">
                        <Form.Label>Schema Name</Form.Label>
                        <Typeahead id="schemas" onChange={selectSchema} size="sm" 
                       
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
            { schema !== "" && 
                        <Row>
                        <Col xs="auto">
                            <Form.Group className="mb-3" controlId="dbname">
                                <Form.Label>Table Name</Form.Label>
                                <Typeahead id="tables" onChange={selectTable} size="sm" 

                                options={getTables()} 
                                defaultOpen={false}
                                labelKey="Table"  
                                placeholder="Choose table..."
                    
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
                                <BootstrapTable id="schematable"
                                condensed
                                striped bordered={false}
                                bootstrap4
                                keyField='name'
                                data={showTableSchema()}
                                columns={schemacolumns}
                            />

             
            }
            <Button variant="dymium" size="sm" className="mt-4" type="submit">
                Apply
            </Button>
        </Form>


    </div>
}

function DatascopeForm(props) {
    let empty: any[] = []
    const [databases, setDatabases] = useState(empty)
    const [connections, setConnections] = useState(empty)
    const [selectedConnection, setSelectedConnection] = useState("")
    const [showOffcanvas, setShowOffcanvas] = useState(false)

    let columns = [
        {
            dataField: 'id',
            text: 'id',
            hidden: true,
        },
        {
            dataField: 'name',
            text: 'Name:',
            sort: true,
        },
        {
            dataField: 'dbtype',
            text: 'DB Type:',
            formatter: (cell, row, rowIndex, formatExtraData) => {
                return com.databaseTypes[row["dbtype"]]
            },
            sortValue: (cell, row) => {
                return com.databaseTypes[row["dbtype"]]
            },
            sort: true
        },
        {
            text: 'Prefix',
            dataField: 'prefix',
            isDummyField: true,
            formatter: (cell, row, rowIndex, formatExtraData) => {

                return <Form.Control size="sm" type="text" placeholder="alphanumeric"
                    required
                    pattern=".+"
                    value={props.username}
                    onChange={e => props.setUsername(e.target.value)}
                />
            },
            //formatExtraData: { hoverIdx: this.state.hoverIdx },
            //headerStyle: { width: '50px' },
            //style: { height: '30px' },
            //align: 'center'
        },
        {
            text: 'Tables',
            dataField: 'edit',
            isDummyField: true,
            formatter: (cell, row, rowIndex, formatExtraData) => {

                return <i className="fas fa-table ablue"
                    //onClick={onEdit(row["id"])} 
                    role="button"></i>
            },
            //formatExtraData: { hoverIdx: this.state.hoverIdx },
            headerStyle: { width: '70px' },
            style: { height: '30px' },
            align: 'center'
        },
        {
            text: 'Delete',
            dataField: 'delete',
            isDummyField: true,
            formatter: (cell, row, rowIndex, formatExtraData) => {
                return <i className="fas fa-trash ablue"
                    //onClick={onDelete(row["id"])} 
                    role="button"></i>
            },
            //formatExtraData: { hoverIdx: this.state.hoverIdx },
            headerStyle: { width: '90px' },
            style: { height: '30px' },
            align: 'center'
        }
    ]

    let available = () => {
        let ret: any[] = []
        ret = props.connections.filter(x => !databases.includes(x.name)).map(x =>
            <option key={x.name} value={x.name}>{x.name}</option>)
        return ret
    }
    let onAddConnection = e => {
        if (selectedConnection === "") {
            return
        }

        let db = [...databases, selectedConnection]
        setDatabases(db)
    }
    let getTableData = () => {
        return databases.map(name => {
            let ob = remap[name]

            return { name, id: ob.id, dbtype: ob.dbtype }
        })
    }
    let showConnection = (db) => {
        return <Card key={db.name} id={db.name} className="card mb-3"> <Card.Header><Row><Col xs="auto" style={{ paddingTop: '2px', fontSize: '1.2em' }} className="thickblue">
            <i className="fa-solid fa-database mr-2"></i>
            Connection: {db.name}</Col><Col><Button onClick={e => { setShowOffcanvas(true) }} size="sm" variant="dymium">Add Table</Button></Col><Col xs="auto" className="text-right"><i className="fa fa-trash" aria-hidden="true"></i></Col></Row></Card.Header>
        </Card>
    }
    let showConnections = () => {
        return databases.map(name => {
            let ob = remap[name]

            return showConnection(ob)
        })
    }

    return (
        <>
            <Offcanvas show={showOffcanvas} onClose={(e) => { setShowOffcanvas(false) }}

                title={"Register table"}>

                <AddTable />

            </Offcanvas>

            <Row>
                <Col xs="auto">
                    <Form.Group className="mb-3" controlId="dbname">
                        <Form.Label>Database Name</Form.Label>
                        <Form.Control size="sm" type="text" placeholder="alphanumeric"
                            required
                            pattern=".+"
                            value={props.username}
                            onChange={e => props.setUsername(e.target.value)}
                        />
                        <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                        <Form.Control.Feedback type="invalid" >
                            Client side name for Dymium database
                        </Form.Control.Feedback>
                    </Form.Group>

                </Col>

            </Row>
            <Row>
                <Col xs="auto" className="d-flex" style={{ alignItems: "center" }}>
                    <Form.Group className="mb-3" controlId="connection" >
                        <Form.Label >Available Connections</Form.Label>
                        <Form.Control as="select" size="sm"
                            onChange={e => {
                                console.log(e.target.value)
                                setSelectedConnection(e.target.value)
                            }}
                            value={selectedConnection}
                        >
                            <option value="">...</option>
                            {available()}
                        </Form.Control>

                    </Form.Group>
                    <Form.Group>
                        <Form.Label ></Form.Label>
                        <Button onClick={onAddConnection} variant="dymium" style={{ marginTop: '1.9em' }} size="sm"><i className="fa-solid fa-database mr-2"></i>Add Connection</Button>
                    </Form.Group>
                </Col>
                <Col className="text-left">

                </Col>

            </Row>
            {showConnections()}
            {/*
            <Row>
                <Col >
                    <BootstrapTable id="scaledtable"
                        condensed
                        striped bordered={false}
                        bootstrap4
                        keyField='name'
                        data={getTableData()}
                        columns={columns}
                    />
                </Col>
                <Col></Col>
            </Row>
                        */}
        </>
    )
}


export function AddDatascope() {
    const [validated, setValidated] = useState(false)
    let form = useRef<HTMLFormElement>(null)

    let [conns, setConns] = useState([])
    const [spinner, setSpinner] = useState(false)
    const [alert, setAlert] = useState(<></>)


    let getConnections = () => {
        setSpinner(true)
        setConns([])
        com.sendToServer("GET", "/api/getconnections",
            null, "",
            resp => {

                resp.json().then(js => {

                    let cc = js.map(x => {
                        let ob = {
                            id: x.id,
                            credid: x.credid,
                            dbtype: x.dbtype,
                            name: x.name,

                            address: x.address,
                            port: x.port,
                            description: x.description,
                            usetls: x.useTLS,

                        }
                        remap[x.name] = ob
                        return ob
                    })

                    setConns(cc)

                })

                setSpinner(false)
                console.log("on success")
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
    useEffect(() => {
        getConnections()
    }, [])
    let handleSubmit = event => {
        if (form.current == null) {
            return false
        }
        if (form.current.reportValidity() === false) {
            event.preventDefault();
            setValidated(true)
            //console.log("Form validity false!")
            return false
        }
        event.preventDefault();
        setValidated(false)
        event.stopPropagation();

        //sendConnection()

        return false
    }

    return (
        <div className=" text-left">
            {alert}
            <h5 > Create New Data Scope <Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>
            <div className=" text-left">
                <Form onSubmit={handleSubmit} ref={form} noValidate validated={validated}>
                    <DatascopeForm connections={conns} setAlert={setAlert} />



                    <Button variant="dymium" size="sm" className="mt-4" type="submit">
                        Apply
                    </Button>
                </Form>
            </div>
        </div>
    )
}
export function EditDatascopes() {
    return (
        <>Edit datascopes</>
    )
}

export default function Datascopes() {
    return (
        <Tabs
            //defaultActiveKey={t} 
            id="datascopes"
            //onSelect={(k) => appDispatch( setActiveConnectionTab(k) )}

            unmountOnExit={true} className="mb-3 text-left">
            <Tab eventKey="add" title="Add Data Scope" className="mx-4">
                <AddDatascope />
            </Tab>
            <Tab eventKey="edit" title="Edit Data Scopes" className="mx-4">
                <EditDatascopes />
            </Tab>
        </Tabs>
    )
}