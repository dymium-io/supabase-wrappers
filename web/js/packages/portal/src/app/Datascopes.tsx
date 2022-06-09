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
import cloneDeep from 'lodash/cloneDeep';
import AddTable from './AddTable'
import * as com from '../Common'


let remap = {}

function DatascopeForm(props) {
    let empty: any[] = []
    const [connections, setConnections] = useState(empty)
    const [tables, setTables] = useState({})
    const [selectedConnection, setSelectedConnection] = useState("")
    const [counter, setCounter] = useState(0)
    const editedConnection = useRef("")

    // make sure we don't have a stale closure
    let refs = useRef({})
    refs.current["connections"] = connections
    refs.current["tables"] = tables
    refs.current["setTables"] = setTables
    refs.current["setCounter"] = setCounter

    let onEdit = (connection, schema, table) => {
        return e => {
            let ob = refs.current["tables"][connection]
            Object.keys(ob).forEach( x => {
                if(ob[x].schema === schema && ob[x].table === table) {
                    setSelectedConnection(connection)
                    props.onEditTable(ob[x]) 
                }
            })
        }
    }
    let onDelete = (connection, schema, table)  => {
        return e => {
            let tables = cloneDeep(refs.current["tables"])
            delete tables[connection][schema + "." + table]
            setTables(tables)
            //refs.current["setTables"](tables)
            setCounter(counter + 1)
            setConnections(refs.current["connections"])
        }
    }
    let columns = [
        {
            dataField: 'connection',
            text: 'connection:',
            isDummyField: true,
            hidden: true,
        },
        {
            dataField: 'schema',
            text: 'Schema:',
            isDummyField: true,
            sort: true,
            formatter: (cell, row, rowIndex, formatExtraData) => {

                return row.schema
            }
        },
        {
            dataField: 'table',
            text: 'Table name:',
            isDummyField: true,
            sort: true,
            formatter: (cell, row, rowIndex, formatExtraData) => {

                return row.table
            }
        },
        {
            dataField: 'columns',
            text: '# of columns:',
            isDummyField: true,
            sort: true,
            formatter: (cell, row, rowIndex, formatExtraData) => {

                return row.tablescope.length
            }
        },
        {
            dataField: 'pii',
            text: 'Sensitivity:',
            isDummyField: true,
            sort: true,
            formatter: (cell, row, rowIndex, formatExtraData) => {
                let sense = false
                row.tablescope.forEach(x => {
                    if (x.semantics !== "N/A")
                        sense = true
                })
                if (sense) return <>Contains PII</>
                return <>Not
                    ensitive</>
            }
        },
        {
            dataField: 'disposition',
            text: 'Access:',
            isDummyField: true,
            sort: true,
            formatter: (cell, row, rowIndex, formatExtraData) => {
                let managed = false
                row.tablescope.forEach(x => {
                    if (x.action !== "Allow")
                        managed = true
                })
                if (managed) return <>Managed</>
                return <>Transparent</>
            }
        },
        {
            text: 'Edit',
            dataField: 'edit',
            isDummyField: true,
            formatter: (cell, row, rowIndex, formatExtraData) => {

                return <i className="fas fa-edit ablue" onClick={onEdit(row["connection"], row["schema"], row["table"])} role="button"></i>
            },
            //formatExtraData: { hoverIdx: this.state.hoverIdx },
            headerStyle: { width: '50px' },
            style: { height: '30px' },
            align: 'center'
        },
        {
            text: 'Delete',
            dataField: 'delete',
            isDummyField: true,
            formatter: (cell, row, rowIndex, formatExtraData) => {
                return <i className="fas fa-trash ablue" onClick={onDelete(row["connection"], row["schema"], row["table"])} role="button"></i>
            },
            //formatExtraData: { hoverIdx: this.state.hoverIdx },
            headerStyle: { width: '90px' },
            style: { height: '30px' },
            align: 'center'
        }
    ]

    let addTable = (table) => {
        let _tables = cloneDeep(refs.current["tables"])
        if (_tables[editedConnection.current] === undefined)
            _tables[editedConnection.current] = {}
        table["connection"] = editedConnection.current
        
        _tables[editedConnection.current][table.schema + "." + table.table] = table
        let setTables = refs.current["setTables"]

        setTables(_tables)
        setCounter(counter + 1)
    }
    useEffect( () => {
        console.log("on tables change")
        props.onTableUpdate(tables)
    }, [tables] 
    )
    const addTableRef = useRef(addTable)
    useEffect(() => {
        props.onAddTableRef(addTableRef)
        let _tables = {}
        props.connections.forEach(x => {
            _tables[x.name] = {} // table name: table
        })
        setTables(_tables)
    }, [])

    let available = () => {
        let ret: any[] = []
        ret = props.connections.filter(x => !connections.includes(x.name)).map(x =>
            <option key={x.name} value={x.name}>{x.name}</option>)
        return ret
    }

    let onAddConnection = e => {
        if (selectedConnection === "") {
            return
        }

        let db = [...connections, selectedConnection]
        setConnections(db)
        setSelectedConnection("")
    }

    let displayTables = name => {
        let ret: any[] = []

        if (tables[name] === undefined)
            return ret

        Object.keys(tables[name]).forEach((x: any) => {
            tables[name][x]["connection"] = name
            ret.push(tables[name][x])
        })

        
        return ret
    }

    let showConnection = (db) => {
        if(db.name === undefined)
            return <></>
        let deleteConnection = e => {

            let d = connections.filter(function (value, index, arr) {
                return value !== db.name;
            })

            setConnections(d)
        }
        return <Card key={db.name} id={db.name} className="card mb-3">
            <Card.Header >
                <Row>
                    <Col xs="auto" style={{ paddingTop: '2px', fontSize: '1.2em' }} className="thickblue">
                        <i className="fa-solid fa-database mr-2"></i>
                        Connection: {db.name}</Col>
                    <Col><Button onClick={e => {

                        editedConnection.current = db.name
                        props.AddNewTable(db.id)
                        //props.setShowOffcanvas(true)
                    }} size="sm" variant="dymium"><i className="fa fa-table mr-1" aria-hidden="true"></i>Add Table</Button></Col>
                    <Col xs="auto" className="text-right"><i onClick={deleteConnection} className="fa fa-trash blue trash" aria-hidden="true"></i></Col>
                </Row>
            </Card.Header>
            <Card.Body className="p-0 mx-0">
                {tables[db.name] !== undefined && Object.keys(tables[db.name]).length > 0 &&
                    <BootstrapTable id="scaledtable"
                        condensed
                        striped bordered={false}
                        bootstrap4
                        keyField='table'
                        data={displayTables(db.name)}
                        columns={columns}
                    />

                }
            </Card.Body>
        </Card>
    }
    let showConnections = () => {
        return connections.map(name => {
            if(name === "") {   
                return <></>
            }
            let ob = remap[name]
            return showConnection(ob)
        })
    }

    return (
        <>
            <Row>
                <Col xs="auto">
                    <Form.Group className="mb-3" controlId="dbname">
                        <Form.Label>Database Name</Form.Label>
                        <Form.Control size="sm" type="text" placeholder="alphanumeric"
                            required
                            pattern=".+"
                            value={props.dbname}
                            onChange={e => {
                                console.log(e.target.value)
                                props.onDbname(e.target.value)
                            }}
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

        </>
    )
}


export function AddDatascope(props) {
    const [validated, setValidated] = useState(false)
    let form = useRef<HTMLFormElement>(null)

    let [conns, setConns] = useState([])
    const [spinner, setSpinner] = useState(false)
    const [alert, setAlert] = useState(<></>)
    const [showOffcanvas, setShowOffcanvas] = useState(false)
    const [clear, setClear] = useState(false)
    const [table, setTable] = useState({})
    const [dbname, setDbname] = useState("")
    const [datascope, setDatascope] = useState({})
    const [currentConnectionId, setCurrentConnectionId] = useState("")

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
            return false
        }
        console.log(datascope)
        console.log(dbname)
        event.preventDefault();
        setValidated(false)
        event.stopPropagation();

        //sendConnection()

        return false
    }

    let addTableR: any = useRef(null)

    let onAddTableRef = (theref) => {
        addTableR.current = theref
    }
    let onAddTable = (table) => {
        setShowOffcanvas(false)
        if (addTableR.current !== undefined && addTableR.current.current) {

            addTableR.current.current(table)
        }
    }
    let onEditTable = t => {
        setTable(t)
        setShowOffcanvas(true)
    }
    let addNewTable = id => {
        setCurrentConnectionId(id)
        setTable({})
        setShowOffcanvas(true)
    }
    let onDbname = e => {
        console.log("onDbname: "+e)
    }
    let onTableUpdate = e => {
        console.log("onTableUpdate: "+e)
        setDatascope(e)
    }
    return (
        <div className=" text-left">
            {alert}
            <Offcanvas show={showOffcanvas} onClose={(e) => { setShowOffcanvas(false) }}
                title={table["connection"] === undefined ? "Register table" : "Edit table" }>
                {showOffcanvas &&
                <AddTable clear={clear} onAddTable={onAddTable} table={table} connectionId={currentConnectionId}/>
                }
            </Offcanvas>
            <h5 > Create New Data Scope <Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>
            <div className=" text-left">
                <Form onSubmit={handleSubmit} ref={form} noValidate validated={validated}>
                    <DatascopeForm dbname={dbname} onDbname={setDbname} onTableUpdate={onTableUpdate} onEditTable={onEditTable} AddNewTable={addNewTable} onAddTableRef={onAddTableRef} connections={conns} setAlert={setAlert} setShowOffcanvas={setShowOffcanvas} />

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