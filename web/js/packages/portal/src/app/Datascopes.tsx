import React, { useEffect, useState, useRef } from 'react';
import Tabs from 'react-bootstrap/Tabs'
import Tab from 'react-bootstrap/Tab'
import Form from 'react-bootstrap/Form'
import Button from 'react-bootstrap/Button'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'
import Offcanvas from '../Components/Offcanvas'
import Modal from 'react-bootstrap/Modal'
import Alert from 'react-bootstrap/Alert'
import BootstrapTable from 'react-bootstrap-table-next';
import Spinner from '../Components/Spinner'
import * as com from '../Common'

let remap = {}
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
            <option >{x.name}</option>)
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
        return <Card className="card mb-3"> <Card.Header><Row><Col xs="auto" style={{ paddingTop: '2px', fontSize: '1.2em' }}>
            <i className="fa-solid fa-database mr-2"></i>
            {db.name}</Col><Col><Button onClick={e=>{setShowOffcanvas(true)}}size="sm" variant="dymium">Add Table</Button></Col><Col xs="auto" className="text-right"><i className="fa fa-trash" aria-hidden="true"></i></Col></Row></Card.Header>
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
            <Offcanvas show={showOffcanvas} onClose={(e)=>{setShowOffcanvas(false)}} {...props}>

                    Some text as placeholder. In real life you can have the elements you
                    have chosen. Like, text, images, lists, etc.
        
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
                        <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                        <Form.Control.Feedback type="invalid" >
                            Select connection
                        </Form.Control.Feedback>
                    </Form.Group>
                    <Button onClick={onAddConnection} variant="dymium" style={{ marginTop: '0.8em' }} size="sm"><i className="fa-solid fa-database mr-2"></i>Add Connection</Button>
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
            <h5 > Create New Datascope <Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>
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
            <Tab eventKey="add" title="Add Datascope" className="mx-4">
                <AddDatascope />
            </Tab>
            <Tab eventKey="edit" title="Edit Datascopes" className="mx-4">
                <EditDatascopes />
            </Tab>
        </Tabs>
    )
}