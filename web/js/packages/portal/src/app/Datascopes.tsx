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
import EditDatascopes from './EditDatascopes'
import DatascopeForm from './DatascopeForm'
import { useAppDispatch, useAppSelector } from './hooks'
import {setActiveDatascopeTab} from '../Slices/menuSlice'

import * as com from '../Common'
import * as types from '@dymium/common/Types/Common'


let remap =  new types.ConnectionMap();

export function AddDatascope(props) {
    const [validated, setValidated] = useState(false)
    let form = useRef<HTMLFormElement>(null)

    let [conns, setConns] = useState<types.Connection[]>([])
    const [spinner, setSpinner] = useState(false)
    const [alert, setAlert] = useState<any>(<></>)
    const [showOffcanvas, setShowOffcanvas] = useState(false)

    const [table, setTable] = useState<types.TableScope>({schema: "", table:""})
    const [dbname, setDbname] = useState<string>("")
    const [datascope, setDatascope] = useState<types.TablesMap>({})
    const [currentConnectionId, setCurrentConnectionId] = useState<string>("")

    let getConnections = () => {
        setSpinner(true)
        setConns([])
        setSpinner(true)
        com.sendToServer("GET", "/api/getconnections",
            null, "",
            resp => {

                resp.json().then(js => {

                    let cc:types.Connection[] = js.map(x => {
                        let ob:types.Connection = {
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

    let sendConnection = () => {
        let retarray:types.DatascopeRecord[] = []

        Object.keys(datascope).forEach( connection => {
            let conn = datascope[connection]
            Object.keys(conn).forEach( schematable => {
                let st = conn[schematable]
                // connection, schema, table, tablescope[typ, semantics, name, position, reference, action]
                console.log(st)
                st.tablescope.forEach( ts => {
                    let ob:types.DatascopeRecord = {connection: st.connection, schema: st.schema, table: st.table, 
                        typ: ts.typ, position: ts.position, reference: ts.reference, action:ts.action, 
                        col: ts.name, semantics: ts.semantics, dflt: ts.dflt, isnullable: ts.isnullable}
                    console.log(JSON.stringify(ob))
                    retarray.push(ob)
                })
            })

        } )
        // now do send
        setSpinner(true)
        let retob: types.DataScope = {name: dbname, records: retarray}
        let body=JSON.stringify( retob )
        com.sendToServer("POST", "/api/savedatascope",
            null, body,
            resp => {

                resp.json().then(js => {
                    if(js.Status === "OK") {
                        setAlert(
                            <Alert variant="success" onClose={() => setAlert(<></>)} dismissible>
                                Data scope {dbname} created successfully!
                            </Alert>
                        )
                    } else {
                        setAlert(
                            <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                                Error creating {dbname}:  {js.Text}!
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

        sendConnection()

        return false
    }

    let addTableR: any = useRef(null)

    let onAddTableRef = (theref) => {
        addTableR.current = theref
    }
    let onAddTable = (table:types.TableScope) => {
        setShowOffcanvas(false)
        if (addTableR.current !== undefined && addTableR.current.current) {
            addTableR.current.current(table)
        }
    }
    let onEditTable = (t:types.TableScope) => {
        setTable(t)
        setShowOffcanvas(true)
    }
    let addNewTable = (id:string) => {
        setCurrentConnectionId(id)
        setTable({schema: "", table:""})
        setShowOffcanvas(true)
    }
    let onDbname = (e:string) => {
        console.log("onDbname: "+e)
        setDbname(e)
    }
    let onTablesMapUpdate = (t: types.TablesMap) => {
        setDatascope(t)
    }
    return (
        <div className=" text-left">
            {alert}
            <Offcanvas show={showOffcanvas} onClose={(e) => { setShowOffcanvas(false) }}
                title={table["connection"] === undefined ? "Register table" : "Edit table" }>
                {showOffcanvas &&
                <AddTable onHide={() => {setShowOffcanvas(false)}} onAlert={setAlert} onAddTable={onAddTable} table={table} connectionId={currentConnectionId}/>
                }
            </Offcanvas>
            <h5 > Create New Data Scope <Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>
            <div className=" text-left">
                <Form onSubmit={handleSubmit} ref={form} noValidate validated={validated}>
                    <DatascopeForm edit={false} dbname={dbname} onDbname={setDbname} onTablesMapUpdate={onTablesMapUpdate} 
                    onEditTable={onEditTable} AddNewTable={addNewTable} 
                    onAddTableRef={onAddTableRef} connections={conns} setAlert={setAlert} 
                    nameToConnection={remap}/>

                    <Button variant="dymium" size="sm" className="mt-4" type="submit">
                        Apply
                    </Button>
                </Form>
            </div>
        </div>
    )
}


export default function Datascopes() {
    const t = useAppSelector((state) => {
        
        return state.reducer.activeDatascopeTab}
        )
    const appDispatch = useAppDispatch()

    return (
        <Tabs
            defaultActiveKey={t} 
            id="datascopes"
            onSelect={(k) => appDispatch( setActiveDatascopeTab(k) )}

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