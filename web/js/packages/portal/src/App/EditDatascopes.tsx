import React, { useEffect, useState, useRef } from 'react';
import Form from 'react-bootstrap/Form'
import Button from 'react-bootstrap/Button'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Offcanvas from '@dymium/common/Components/Offcanvas'
import Alert from 'react-bootstrap/Alert'
import Spinner from '@dymium/common/Components/Spinner'
import Modal from 'react-bootstrap/Modal'
import BootstrapTable from 'react-bootstrap-table-next';
import paginationFactory from 'react-bootstrap-table2-paginator';
import ToolkitProvider, { Search } from 'react-bootstrap-table2-toolkit/dist/react-bootstrap-table2-toolkit';
import AddTable from './AddTable'
import { useAppDispatch, useAppSelector } from './hooks'
import { setSelectedDatascopeDefault } from '../Slices/menuSlice'
import * as com from '../Common'
import DatascopeForm from './DatascopeForm'
import * as internal from '@dymium/common/Types/Internal'
import * as types from '@dymium/common/Types/Common'
import * as capi from '../Api/Connections'
import * as http from '@dymium/common/Api/Http'

const { SearchBar, ClearSearchButton } = Search;

let remap = new internal.ConnectionMap();

export default function EditDatascopes() {
    const [spinner, setSpinner] = useState(false)
    let [conns, setConns] = useState<internal.Connection[]>([])
    const [alert, setAlert] = useState<JSX.Element>(<></>)
    const [showdelete, setShowdelete] = useState(false)
    const [slatedToDelete, setSlatedToDelete] = useState("")
    const [datascopes, setDatascopes] = useState<types.DatascopeIdName[]>([])
    const [selectedDatascope, setSelectedDatascope] = useState("")
    const [selectedDatascopeDetails, setSelectedDatascopeDetails] = useState<types.Datascope | null>(null)

    const [initialTables, setInitialTables] = useState<internal.TablesMap>({})
    const [validated, setValidated] = useState(false)
    let form = useRef<HTMLFormElement>(null)

    let setSDRef = useRef(setSelectedDatascope)

    const [showOffcanvas, setShowOffcanvas] = useState<boolean>(false)
    const [table, setTable] = useState<internal.TableScope>({ schema: "", table: "" })
    const [dbname, setDbname] = useState("")
    const [datascope, setDatascope] = useState({})
    const [currentConnectionId, setCurrentConnectionId] = useState("")
    const [currentConnectionType, setCurrentConnectionType] = useState("")
    const [showOffhelp, setShowOffhelp] = useState(com.isInstaller())

    let t = useAppSelector((state) => {

        return state.reducer.selectedDatascope
    }
    )
    const appDispatch = useAppDispatch()
    let nameById = id => {
        for(let i = 0; i < datascopes.length; i++) {
            if(id === datascopes[i].id)
                return datascopes[i].name
        }
        return ""
    }
    let onEdit = (id) => {
        return e => {
            console.log(id)
            setSDRef.current(id)
      
            appDispatch(setSelectedDatascopeDefault(id))
        }
    }
    let onDelete = (id) => {
        return e => {
            setSlatedToDelete(id)
            setShowdelete(true)
        }
    }
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
            text: 'Edit',
            dataField: 'edit',
            isDummyField: false,
            formatter: (cell, row, rowIndex, formatExtraData) => {
                return <i className="fas fa-edit ablue" onClick={onEdit(row["id"])} role="button"></i>
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
                return <i className="fas fa-trash ablue" onClick={onDelete(row["id"])} role="button"></i>
            },
            //formatExtraData: { hoverIdx: this.state.hoverIdx },
            headerStyle: { width: '90px' },
            style: { height: '30px' },
            align: 'center'
        }
    ]

    let selectRow = {
        mode: 'radio',
        //clickToSelect: true,
        style: { backgroundColor: 'rgba(0, 151, 206, 0.3)' },
        selected: [t],
        onSelect: (row, isSelect, rowIndex, e) => {
            console.log("in onselect", row["id"])
            setSDRef.current(row["id"])
            appDispatch(setSelectedDatascopeDefault(row["id"]))
        },
    };
    let reload = () => { 
        capi.getConnections(setSpinner, setConns, setAlert, remap, () => {

            com.getDatascopes(setSpinner, setAlert, setDatascopes, () => {
                setSelectedDatascope(t)
            })
        })
    }
    useEffect(() => {
        reload()
    }, [])
    useEffect(() => {
        if (selectedDatascope === "" )
            return
        let body = types.DatascopeId.fromJson({ id: selectedDatascope }).toJson()
        setSpinner(true)
        http.sendToServer("POST", "/api/getdatascopedetails",
            null, body,
            resp => {
                resp.json().then(fjs => {
                    let ojs = types.DatascopeInfoStatus.fromJson(fjs)
                    if (ojs.status !== "OK") {
                        setSpinner(false)
                        if(ojs.status !== "sql: no rows in result set")
                            setAlert(
                                <Alert variant="success" onClose={() => setAlert(<></>)} dismissible>
                                    {ojs.errormessage}
                                </Alert>
                            )
                        setSelectedDatascope("")
                        appDispatch(setSelectedDatascopeDefault(""))
                        return
                    }
                    let js = ojs.record
                    if (js == null) {
                        setSpinner(false)
                        setAlert(
                            <Alert variant="success" onClose={() => setAlert(<></>)} dismissible>
                                No record returned
                            </Alert>
                        )
                    }

                    setSelectedDatascopeDetails(js)
                    if (js != null)
                        setDbname(js.name)
                    let ob = {}
                    if (js != null && js.records != null) {
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
                            line["dflt"] = r.dflt
                            line["isnullable"] = r.isnullable
                            line["possibleActions"] = r.possibleActions
                            line["typ"] = r.typ
                            ob[r.connection][key]["tablescope"].push(line)

                        })
                    }
                    setInitialTables(ob)

                })
                setSpinner(false)
                capi.getConnections(setSpinner, setConns, setAlert, remap, () => {
                    com.getDatascopes(setSpinner, setAlert, setDatascopes, () => {
                        setSelectedDatascope(t)
                    })
                })
            },
            resp => {
                console.log("on error")
                setSpinner(false)
                resp != null && resp.text().then(t =>
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            {t}
                        </Alert>
                    )                    
                )
            },
            error => {
                console.log("on exception: " + error)
                setSpinner(false)
            })

    }, [selectedDatascope]
    )

    let updateConnection = () => {
        let retarray: internal.DatascopeRecord[] = []
        Object.keys(datascope).forEach(connection => {
            let conn = datascope[connection]
            Object.keys(conn).forEach(schematable => {
                let st = conn[schematable]
                // connection, schema, table, tablescope[typ, semantics, name, position, reference, action]

                st.tablescope.forEach(ts => {
                    let ob: internal.DatascopeRecord = {
                        connection: st.connection, schema: st.schema, table: st.table,
                        typ: ts.typ, position: ts.position, reference: ts.reference, action: ts.action,
                        col: ts.name, semantics: ts.semantics, dflt: ts.dflt, isnullable: ts.isnullable, 
                        possibleActions: ts.possibleActions
                    }
                    retarray.push(ob)
                })
            })

        })
        // now do send
        setSpinner(true)
        let retob: internal.DataScope = { name: dbname, records: retarray }
        let body = JSON.stringify(retob)
        http.sendToServer("POST", "/api/updatedatascope",
            null, body,
            resp => {

                resp.json().then(js => {
                    if (js.status === "OK") {
                        setAlert(
                            <Alert variant="success" onClose={() => setAlert(<></>)} dismissible>
                                Ghost Database {dbname} updated successfully!
                            </Alert>
                        )
                        capi.getConnections(setSpinner, setConns, setAlert, remap, () => {

                            com.getDatascopes(setSpinner, setAlert, setDatascopes, () => {
                                setSelectedDatascope(t)
                            })
                        })
                    } else {
                        setAlert(
                            <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                                Error updating {dbname}:  {js.errormessage}!
                            </Alert>
                        )
                    }
                })
                setSpinner(false)
            },
            resp => {
                console.log("on error")
                setSpinner(false)
                resp != null && resp.text().then(t =>
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Error updating {dbname}:  {t}
                        </Alert>
                    ))
            },
            error => {
                console.log("on exception: " + error)
                setSpinner(false)
            })

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
    let onTablesMapUpdate = (t: internal.TablesMap) => {
        setDatascope(t)
    }

    let onEditTable = (t: internal.TableScope) => {
        setTable(t)
        setShowOffcanvas(true)
    }

    let addNewTable = (id: string, dbtype:string, schema?: string, table?: string) => {
        setCurrentConnectionId(id)
        setCurrentConnectionType(dbtype)
        if (schema === undefined || table === undefined)
            setTable({ schema: "", table: "" })
        else
            setTable({ schema, table })
        setShowOffcanvas(true)
    }
    let deleteDatascope = () => {
        setShowdelete(false)
        setSpinner(true)
        let retob: types.DatascopeIdName = new types.DatascopeIdName()
        retob.id = slatedToDelete
        retob.name = nameById(slatedToDelete)
        let body = retob.toJson()

        http.sendToServer("POST", "/api/deletedatascope",
            null, body,
            resp => {

                resp.json().then(js => {
                    if (js.status === "OK") {
                        setAlert(
                            <Alert variant="success" onClose={() => setAlert(<></>)} dismissible>
                                Ghost Database {dbname} updated successfully!
                            </Alert>
                        )
                        capi.getConnections(setSpinner, setConns, setAlert, remap, () => {

                            com.getDatascopes(setSpinner, setAlert, setDatascopes, () => {
                                setSelectedDatascope(t)
                            })
                        })
                    } else {
                        setAlert(
                            <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                                Error updating {dbname}:  {js.errormessage}
                            </Alert>
                        )
                    }
                })
                setSpinner(false)
            },
            resp => {
                console.log("on error")
                setSpinner(false)
                resp != null && resp.text().then(t =>
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Error updating {dbname}:  {t}
                        </Alert>
                    )
                )
            },
            error => {
                console.log("on exception: " + error)
                setSpinner(false)
            })

    }
    let onDeleteConnection = (c:string) => {
        delete datascope[c]
        setDatascope(datascope)
    }
    return (
        <div className=" text-left">
            {alert}            
            <Offcanvas show={showOffcanvas} onClose={(e) => { setShowOffcanvas(false) }}
                title={table["connection"] === undefined ? "Register table" : "Edit table"}>
                {showOffcanvas &&
                    <AddTable onHide={() => { setShowOffcanvas(false) }} onAlert={setAlert} onAddTable={onAddTable} table={table} currentConnectionType={currentConnectionType}  connectionId={currentConnectionId} />
                }
            </Offcanvas>
            <Offcanvas modal={false} width={300} show={showOffhelp} onClose={(e) => { setShowOffhelp(false) }}>
                <h5>Editing Ghost Database</h5>
                <div className="mb-3">
                    This page allows to edit or remove a Ghost Database.
                </div>
                <div className="mb-3">
                    The same interface as for adding a Ghost Database is used. You select Data Sources and tables that you want to expose
                </div>
            </Offcanvas>            
            <Modal centered show={showdelete} onHide={() => setShowdelete(false)} data-testid="modal-delete">
                <Modal.Header closeButton>
                    <Modal.Title>Delete Datascope {nameById(slatedToDelete)}?</Modal.Title>
                </Modal.Header>
                <Modal.Body>Are you sure you want to remove the Datascope? This operation is irreversible.</Modal.Body>
                <Modal.Footer>
                    <Button variant="danger"  role="button" id="Delete" data-testid="Delete"
                         aria-label={"Delete"}
                        onClick={() => {
                            deleteDatascope()
                    }
                    }>Delete</Button> <Button variant="dymium" onClick={() => {
                        setShowdelete(false)
                    }}>Cancel</Button>
                </Modal.Footer>
            </Modal>
            <div className=" text-left">
                {/*
                <Row> <Col xs="auto">
                    <Form.Group className="mb-3" controlId="connection" >
                        <Form.Label >Available Ghost Databases</Form.Label>
                        <Form.Control as="select" size="sm"
                            onChange={e => {

                                setSelectedDatascope(e.target.value)
                                appDispatch( setSelectedDatascopeDefault(e.target.value) )

                            }}
                            value={selectedDatascope}
                        >
                            return <option value="">...</option>
                            {datascopes != null && datascopes.map(x => {

                                return <option key={x.id} value={x.id}>{x.name}</option>
                            })
                            }
                        </Form.Control>

                    </Form.Group>
                </Col>
                </Row>
                */}
                <Row>
                    <Col>
                        <ToolkitProvider
                            bootstrap4
                       
                            keyField='id'
                            data={datascopes}
                            columns={columns}
                            search >
                            {
                                props => (
                                    <div className="text-left">
                                   
                                        <div className="d-flex">
                                            <h5 >Edit Ghost Databases  <i onClick={e => { setShowOffhelp(!showOffhelp) }} className="trash fa-solid fa-circle-info mr-1"></i><Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>


                                            <div style={{ marginLeft: "auto" }}>
                                                <SearchBar size="sm" {...props.searchProps} />
                                                <ClearSearchButton {...props.searchProps} />
                                                <i onClick={e=>reload()} className="fa fa-refresh ablue cursor-pointer" style={{position: 'relative', top: '2px'}} aria-hidden="true"></i>

                                            </div>
                                        </div>
                                        <div className="d-block">
                                            <BootstrapTable id="datascopetable"
                                                condensed
                                                keyField='id'
                                                selectRow={selectRow}
                                                striped bootstrap4 bordered={false}
                                                pagination={paginationFactory({
                                                    sizePerPage: 4,
                                                    sizePerPageList: [4, 8, 12, 16]
                                                })}
                                                {...props.baseProps}
                                            />
                                        </div>
                                    </div>
                                )
                            }
                        </ToolkitProvider>
                    </Col>
                </Row>
                {(selectedDatascope !== "" && selectedDatascope !== "xxx") &&
                    <div className=" text-left p-4 mt-4" style={{ backgroundColor: "rgba(255, 255, 255, 0.7)" }}>
                        <h5>{selectedDatascopeDetails?.name}</h5>
                        <Form onSubmit={handleSubmit} ref={form} noValidate validated={validated}>
                            <DatascopeForm edit={true} dbname={dbname} onDbname={setDbname}
                                onDeleteConnection={onDeleteConnection}
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