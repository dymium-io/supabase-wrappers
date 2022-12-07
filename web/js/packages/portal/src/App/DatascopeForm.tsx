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
import * as com from '../Common'
import * as types from '@dymium/common/Types/Common'
import * as internal from '@dymium/common/Types/Internal'

//
// dbname - datascope name
// onDbname - handler for recording dbname change
// onTablesMapUpdate - callback on Table change
// onEditTable - table edit is requested 
// AddNewTable - add new table is requested 
// onAddTableRef - pass ref to addTable function
// connections={conns} 
// setAlert={setAlert} 
// nameToConnection
export interface DatascopeFormProps {
    edit: boolean,
    initialTables?: internal.TablesMap,
    onEditTable: (ar: internal.TableScope) => void,
    onTablesMapUpdate: (ar: internal.TablesMap) => void,
    onAddTableRef: (ar: any) => void,
    connections: internal.Connection[],
    AddNewTable: (ar: string, dbtype: string, schema?: string, table?: string) => void,
    nameToConnection: internal.ConnectionMap,
    dbname: string,
    onDbname: (ar: string) => void,
    setAlert: (ar: any) => void, // not sure how to deal with the type properly here

    //    table: types.TableScope,
    //    connectionId: string,
    //    onAddTable: (ar: types.TableScope) => void,

}
const DatascopeForm: React.FC<DatascopeFormProps> = (props) => {

    let empty: any[] = []
    const [connections, setConnections] = useState<string[]>(empty)
    const [tables, setTables] = useState<internal.TablesMap>({})
    const [selectedConnection, setSelectedConnection] = useState<string>("")
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
            let ob: internal.TableScope = refs.current["tables"][connection]
            Object.keys(ob).forEach(x => {
                if (ob[x].schema === schema && ob[x].table === table) {
                    setSelectedConnection(connection)
                    props.onEditTable(ob[x])
                    editedConnection.current = connection
                }
            })
        }
    }
    let onDelete = (connection: string, schema: string, table: string) => {
        return e => {
            let tables: internal.TablesMap = cloneDeep(refs.current["tables"])
            delete tables[connection][schema + "." + table]
            setTables(tables)
            //refs.current["setTables"](tables)
            setCounter(counter + 1)
            setConnections(refs.current["connections"])
        }
    }
    let columns = [
        {
            dataField: 'id',
            text: 'id:',
            hidden: true,
        },        
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
                return <>Not sensitive</>
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
    useEffect(() => {
        props.onTablesMapUpdate(tables)
    }, [tables]
    )

    useEffect(() => {
        if (props.initialTables !== undefined) {
            let conns: string[] = []
            Object.keys(props.initialTables).forEach(x => {
                conns.push(x)
            })
            setTables(props.initialTables)
            setConnections(conns)
        }
    }, [props.initialTables]
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
            <option data-testid={x.name} key={x.name} value={x.name}>{x.name}</option>)
        return ret
    }

    let onAddConnection = (e: any) => {
        if (selectedConnection === "") {
            return
        }

        let db = [...connections, selectedConnection]
        setConnections(db)
        setSelectedConnection("")
    }

    let displayTables = (name: string) => {
        let ret: any[] = []

        if (tables[name] === undefined)
            return ret

        Object.keys(tables[name]).forEach((x: any) => {
            tables[name][x]["connection"] = name
            tables[name][x]["id"] = tables[name][x]["connection"] + tables[name][x]["schema"] + tables[name][x]["table"]
            ret.push(tables[name][x])
        })
        return ret
    }

    let showConnection = (db: types.ConnectionRecord) => {
        if (db === undefined || db.name === undefined)
            return <></>
        let deleteConnection = e => {
            let d = connections.filter(function (value, index, arr) {
                return value !== db.name;
            })
            setConnections(d)
        }
        let showDependencies = () => {
            // map tables
            if (tables[db.name] === undefined)
                return ""
            let references = {}
            let tbnames = Object.keys(tables[db.name])
            for (let i = 0; i < tbnames.length; i++) {
                let tbl = tables[db.name][tbnames[i]].tablescope
                for (let j = 0; j < tbl.length; j++) {
                    if (tbl[j].reference != null) {
  
                        let rsch = tbl[j].reference.schema
                        let rtbl = tbl[j].reference.table
                        references[rsch + "." + rtbl] = {
                            schema: rsch, table: rtbl,
                            refby: tables[db.name][tbnames[i]].schema + "." + tables[db.name][tbnames[i]].table
                        }
                    }
                }
            }

            //
            let lines: JSX.Element[] = []
            Object.keys(references).forEach((key) => {
                if (tables[db.name][key] !== undefined)
                    return
                let { schema, table, refby } = references[key]
                let r = <div className="m-1">Table {refby} refers to {schema}.{table}. <Button onClick={
                    e => {
                        if (db.id !== null)
                            props.AddNewTable(db.id, props.nameToConnection[db.name].dbtype, schema, table)
                    }

                } size="sm" style={{ marginTop: "-2px" }} variant="dymium">Click to link {schema}.{table}</Button></div>
                lines.push(r)
            })
            return lines
        }
        return <Card key={db.name} id={db.name} className="card mb-3">
            <Card.Header >
                <Row>
                    <Col xs="auto" style={{ paddingTop: '2px', fontSize: '1.2em' }} className="thickblue">
                        <i className="fa-solid fa-database mr-2"></i>
                        Connection: {db.name}</Col>
                    <Col><Button onClick={e => {

                        editedConnection.current = db.name
                        if (db.id !== null) {
                            props.AddNewTable(db.id, props.nameToConnection[db.name].dbtype)
                        }

                    }} size="sm" variant="dymium"><i className="fa fa-table mr-1" aria-hidden="true"></i>Link Table</Button></Col>
                    <Col xs="auto" className="text-right"><i onClick={deleteConnection} title={"Unlink "+ db.name} className="fa fa-unlink blue trash" style={{marginRight: '18px'}} aria-hidden="true"></i></Col>
                </Row>
            </Card.Header>
            <Card.Body className="p-0 mx-0">
                {tables[db.name] !== undefined && Object.keys(tables[db.name]).length > 0 &&
                    <BootstrapTable id="scaledtable"
                        condensed
                        striped bordered={false}
                        bootstrap4
                        keyField='id'
                        data={displayTables(db.name)}
                        columns={columns}
                    />
                }
                {
                    showDependencies()
                }
            </Card.Body>
        </Card>
    }
    let showConnections = () => {
        return connections.map(name => {
            if (name === "") {
                return <></>
            }
            let ob: types.ConnectionRecord = props.nameToConnection[name]
            return showConnection(ob)
        })
    }

    return (
        <>
            {!props.edit &&
                <Row>
                    <Col xs="auto">
                        <Form.Group className="mb-3" controlId="dbname">
                            <Form.Label>Database Name</Form.Label>
                            <Form.Control size="sm" type="text" placeholder="alphanumeric"
                                required
                                pattern=".+"
                                value={props.dbname}
                                onChange={e => {
       
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
            }
            <Row>
                <Col xs="auto" className="d-flex" style={{ alignItems: "center" }}>
                    <Form.Group className="mb-3" controlId="connection" >
                        <Form.Label >Available Data Sources</Form.Label>
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
                        <Button onClick={onAddConnection} variant="dymium" style={{ marginTop: '1.9em' }} size="sm"><i className="fa-solid fa-database mr-2"></i>Link Connection</Button>
                    </Form.Group>
                </Col>
                <Col className="text-left">

                </Col>

            </Row>
            {showConnections()}

        </>
    )
}

export default DatascopeForm