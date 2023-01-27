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
import AssignGroups from './AssignGroups'
import Groups from './Groups'
import { useAppDispatch, useAppSelector } from './hooks'
import {setActiveDatascopeTab} from '../Slices/menuSlice'

import * as com from '../Common'
import * as internal from '@dymium/common/Types/Internal'
import * as types from '@dymium/common/Types/Common'
import * as capi from '../Api/Connections'
import * as http from '@dymium/common/Api/Http'

let remap =  new internal.ConnectionMap();

export function AddDatascope(props) {
    const [validated, setValidated] = useState(false)
    let form = useRef<HTMLFormElement>(null)

    let [conns, setConns] = useState<internal.Connection[]>([])
    const [spinner, setSpinner] = useState(false)
    const [alert, setAlert] = useState<JSX.Element>(<></>)
    const [showOffcanvas, setShowOffcanvas] = useState(false)

    const [table, setTable] = useState<internal.TableScope>({schema: "", table:""})
    const [dbname, setDbname] = useState<string>("")
    const [datascope, setDatascope] = useState<internal.TablesMap>({})
    const [currentConnectionId, setCurrentConnectionId] = useState<string>("")
    const [currentConnectionType, setCurrentConnectionType] = useState<string>("")
    
    useEffect(() => {
        capi.getConnections(setSpinner, setConns, setAlert, remap, ()=>{})
    }, [])

    let sendConnection = () => {
        let retarray:types.DatascopeRecord[] = []

        Object.keys(datascope).forEach( connection => {
            let conn = datascope[connection]
            Object.keys(conn).forEach( schematable => {
                let st = conn[schematable]
          
                st.tablescope.forEach( ts => {
                    let ob:types.DatascopeRecord = types.DatascopeRecord.fromJson({connection: st.connection, schema: st.schema, table: st.table, 
                        typ: ts.typ, position: ts.position, reference: ts.reference, action:ts.action, 
                        col: ts.name, semantics: ts.semantics, dflt: ts.dflt, isnullable: ts.isnullable})
                    retarray.push(ob)
                })
            })

        } )
        // now do send
        setSpinner(true)
        let retob: types.Datascope = types.Datascope.fromJson({name: dbname, records: retarray})
        let body= retob.toJson()
        http.sendToServer("POST", "/api/savedatascope",
            null, body,
            resp => {

                resp.json().then(js => {
                    if(js.status === "OK") {
                        setAlert(
                            <Alert variant="success" onClose={() => setAlert(<></>)} dismissible>
                                Ghost Database {dbname} created successfully!
                            </Alert>
                        )
                    } else {
                        setAlert(
                            <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                                Error creating {dbname}:  {js.errormessage}!
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
    let onAddTable = (table:internal.TableScope) => {
        setShowOffcanvas(false)
        if (addTableR.current !== undefined && addTableR.current.current) {
            addTableR.current.current(table)
        }
    }
    let onEditTable = (t:internal.TableScope) => {
        setTable(t)
        setShowOffcanvas(true)
    }
    let addNewTable = (id:string, dbtype:string, schema?:string, table?:string) => {
        setCurrentConnectionId(id)
        setCurrentConnectionType(dbtype)
        if(schema === undefined || table === undefined)
            setTable({ schema: "", table: "" })
        else 
            setTable({ schema, table})
        setShowOffcanvas(true)
    }
    let onDbname = (e:string) => {
        setDbname(e)
    }
    let onTablesMapUpdate = (t: internal.TablesMap) => {
        setDatascope(t)
    }
    let onDeleteConnection = (c:string) => {
        delete datascope[c]
        setDatascope(datascope)
    }
    return (
        <div className=" text-left">
            {alert}
            <Offcanvas show={showOffcanvas} onClose={(e) => { setShowOffcanvas(false) }}
                title={table["connection"] === undefined ? "Register table" : "Edit table" }>
                {showOffcanvas &&
                <AddTable onHide={() => {setShowOffcanvas(false)}} onAlert={setAlert} onAddTable={onAddTable} table={table} currentConnectionType={currentConnectionType} connectionId={currentConnectionId}/>
                }
            </Offcanvas>
            <h5 > Create New Ghost Database <Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>
            <div className=" text-left">
                <Form onSubmit={handleSubmit} ref={form} noValidate validated={validated}>
                    <DatascopeForm edit={false} dbname={dbname} onDbname={setDbname} onTablesMapUpdate={onTablesMapUpdate} 
                    onEditTable={onEditTable} AddNewTable={addNewTable} 
                    onDeleteConnection={onDeleteConnection}
                    onAddTableRef={onAddTableRef} connections={conns} setAlert={setAlert} 
                    nameToConnection={remap}/>

                    <Button data-testid="apply-datascope" variant="dymium" size="sm" className="mt-4" type="submit">
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
            <Tab eventKey="add" title="Add" className="mx-4">
                <AddDatascope />
            </Tab>
            <Tab eventKey="edit" title="Edit Ghost Databases" className="mx-4">
                <EditDatascopes />
            </Tab>
            <Tab eventKey="groups" title="Assign Groups" className="mx-4">
                <AssignGroups />
            </Tab>       
        </Tabs>
    )
}