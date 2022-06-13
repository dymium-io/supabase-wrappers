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
import DatascopeForm from './DatascopeForm'
import * as types from '@dymium/common/Types/Common'

interface DataScope {
    id: string;
    name: string;
    records?: types.TableLine[];
}

let remap = new types.ConnectionMap();

export default function EditDatascopes() {
    const [spinner, setSpinner] = useState(false)
    let [conns, setConns] = useState<types.Connection[]>([])
    const [alert, setAlert] = useState<any>(<></>)
    const [datascopes, setDatascopes] = useState<DataScope[]>([])
    const [selectedDatascope, setSelectedDatascope] = useState("")
    const [selectedDatascopeDetails, setSelectedDatascopeDetails] = useState<DataScope | null>(null)

    const [initialTables, setInitialTables] = useState<types.TablesMap>({})
    const [validated, setValidated] = useState(false)
    let form = useRef<HTMLFormElement>(null)


    const [showOffcanvas, setShowOffcanvas] = useState<boolean>(false)
    const [table, setTable] = useState<types.TableScope>({ schema: "", table: "" })
    const [dbname, setDbname] = useState("")
    const [datascope, setDatascope] = useState({})
    const [currentConnectionId, setCurrentConnectionId] = useState("")

    let getDatascopes = () => {
        setSpinner(true)
        com.sendToServer("GET", "/api/getdatascopes",
            null, "",
            resp => {

                resp.json().then(js => {
                    setDatascopes(js)
                })

                setSpinner(false)
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

    let getConnections = () => {
        setSpinner(true)
        setConns([])
        setSpinner(true)
        com.sendToServer("GET", "/api/getconnections",
            null, "",
            resp => {

                resp.json().then(js => {

                    let cc: types.Connection[] = js.map(x => {
                        let ob: types.Connection = {
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
                    getDatascopes()
                })
                setSpinner(false)
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
    useEffect(() => {
        if (selectedDatascope === "")
            return
        console.log("selectedDatascope changed to " + selectedDatascope)
        let body = JSON.stringify(
            { id: selectedDatascope }
        )
        setSpinner(true)
        com.sendToServer("POST", "/api/getdatascopedetails",
            null, body,
            resp => {
                resp.json().then(js => {

                    setSelectedDatascopeDetails(js)
                    setDbname(js.name)
                    let ob = {}
                    if (js.records != null) {
                        js.records.forEach(r => {
                            if (ob[r.connection] === undefined) {
                                ob[r.connection] = {}
                            }
                            let key = r.schema + "." + r.table
                            if (ob[r.connection][key] === undefined) {
                                ob[r.connection][key] = {}
                                ob[r.connection][key]["connection"] = r.connection
                                ob[r.connection][key]["schema"] = r.schema
                                ob[r.connection][key]["table"] = r.table
                                ob[r.connection][key]["tablescope"] = []
                            }
                            let line = {}
                            line["id"] = r.id
                            line["connection"] = r.connection
                            line["action"] = r.action
                            line["name"] = r.col
                            line["position"] = r.position
                            line["reference"] = r.reference
                            line["semantics"] = r.semantics
                            line["typ"] = r.typ
                            ob[r.connection][key]["tablescope"].push(line)

                        })
                    }
                    setInitialTables(ob)

                })
                setSpinner(false)
                getConnections()
            },
            resp => {
                console.log("on error")
                setSpinner(false)
            },
            error => {
                console.log("on exception: " + error)
                setSpinner(false)
            })

    }, [selectedDatascope]
    )

    let updateConnection = () => {
        let retarray: types.DatascopeRecord[] = []
        debugger
        Object.keys(datascope).forEach(connection => {
            let conn = datascope[connection]
            Object.keys(conn).forEach(schematable => {
                let st = conn[schematable]
                // connection, schema, table, tablescope[typ, semantics, name, position, reference, action]
                console.log(st)
                st.tablescope.forEach(ts => {
                    let ob: types.DatascopeRecord = {
                        connection: st.connection, schema: st.schema, table: st.table,
                        typ: ts.typ, position: ts.position, reference: ts.reference, action: ts.action,
                        col: ts.name, semantics: ts.semantics
                    }
                    console.log(JSON.stringify(ob))
                    retarray.push(ob)
                })
            })

        })
        // now do send
        setSpinner(true)
        let retob: types.DataScope = { name: dbname, records: retarray }
        let body = JSON.stringify(retob)
        com.sendToServer("POST", "/api/updatedatascope",
            null, body,
            resp => {

                resp.json().then(js => {
                    if (js.Status === "OK") {
                        setAlert(
                            <Alert variant="success" onClose={() => setAlert(<></>)} dismissible>
                                Data scope {dbname} updated successfully!
                            </Alert>
                        )
                        getConnections()
                    } else {
                        setAlert(
                            <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                                Error updating {dbname}:  {js.Text}!
                            </Alert>
                        )
                    }
                })
                setSpinner(false)
            },
            resp => {
                console.log("on error")
                setSpinner(false)
            },
            error => {
                console.log("on exception: " + error)
                setSpinner(false)
            })
        console.log(retarray)
    }
    let addTableR: any = useRef(null)
    let onAddTableRef = (theref) => {
        addTableR.current = theref
    }
    let onAddTable = (table) => {
        setShowOffcanvas(false)
        debugger
        if (addTableR.current !== undefined && addTableR.current.current) {

            addTableR.current.current(table)
        }
    }
    let handleSubmit = event => {
        if (form.current == null) {
            return false
        }
        if (form.current.reportValidity() === false) {
            event.preventDefault();
            setValidated(true)
            return false
        }

        event.preventDefault();
        setValidated(false)
        event.stopPropagation();

        updateConnection()

        return false
    }
    let onTablesMapUpdate = (t: types.TablesMap) => {
        setDatascope(t)
    }

    let onEditTable = (t: types.TableScope) => {
        setTable(t)
        setShowOffcanvas(true)
    }

    let addNewTable = (id: string) => {
        setCurrentConnectionId(id)
        setTable({ schema: "", table: "" })
        setShowOffcanvas(true)
    }
    return (
        <div className=" text-left">
            {alert}
            <Offcanvas show={showOffcanvas} onClose={(e) => { setShowOffcanvas(false) }}
                title={table["connection"] === undefined ? "Register table" : "Edit table"}>
                {showOffcanvas &&
                    <AddTable onAddTable={onAddTable} table={table} connectionId={currentConnectionId} />
                }
            </Offcanvas>
            <h5 > Edit Data Scopes <Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>
            <div className=" text-left">

                <Row> <Col xs="auto">
                    <Form.Group className="mb-3" controlId="connection" >
                        <Form.Label >Available Data Scopes</Form.Label>
                        <Form.Control as="select" size="sm"
                            onChange={e => {

                                setSelectedDatascope(e.target.value)

                            }}
                            value={selectedDatascope}
                        >
                            return <option value="">...</option>
                            {datascopes.map(x => {

                                return <option key={x.id} value={x.id}>{x.name}</option>
                            })
                            }
                        </Form.Control>

                    </Form.Group>
                </Col>
                </Row>

                {selectedDatascope !== "" &&
                    <div className=" text-left">
                        <Form onSubmit={handleSubmit} ref={form} noValidate validated={validated}>
                            <DatascopeForm edit={true} dbname={dbname} onDbname={setDbname}
                                onTablesMapUpdate={onTablesMapUpdate} onEditTable={onEditTable}
                                AddNewTable={addNewTable} onAddTableRef={onAddTableRef} connections={conns}
                                setAlert={setAlert} nameToConnection={remap}
                                initialTables={initialTables}
                            />

                            <Button variant="dymium" size="sm" className="mt-4" type="submit">
                                Apply
                            </Button>
                        </Form>
                    </div>
                }

            </div>
        </div>
    )
}