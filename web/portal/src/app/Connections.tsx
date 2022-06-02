import React, { useEffect, useState, useRef } from 'react';
import Tabs from 'react-bootstrap/Tabs'
import Tab from 'react-bootstrap/Tab'
import Form from 'react-bootstrap/Form'
import Button from 'react-bootstrap/Button'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Modal from 'react-bootstrap/Modal'
import Alert from 'react-bootstrap/Alert'
import BootstrapTable from 'react-bootstrap-table-next';
import paginationFactory from 'react-bootstrap-table2-paginator';
import ToolkitProvider, { Search } from 'react-bootstrap-table2-toolkit/dist/react-bootstrap-table2-toolkit';
import 'react-bootstrap-table-next/dist/react-bootstrap-table2.min.css';

const { SearchBar, ClearSearchButton } = Search;


import { tooltip } from '../Components/Tooltip'
import PasswordField from '../Components/PasswordField'
import * as com from '../Common'
import Spinner from '../Components/Spinner'
import { useInitialize } from '../Utils/CustomHooks'
import { useAppDispatch, useAppSelector } from './hooks'
import {setActiveConnectionTab} from '../Slices/menuSlice'

const databases = Object.keys(com.databaseTypes).map(key => {
    return <option key={key} value={key}>
        {com.databaseTypes[key]}
    </option>
})

function ConnectionForm(props) {
    return (
        <>
            <Row>
                <Col xs="auto">
                    <Form.Group className="mb-3" controlId="dbtype" >
                        <Form.Label >Database type</Form.Label>
                        <Form.Control as="select" required size="sm" value={props.dbtype}
                            onChange={e => props.setDBType(e.target.value)}
                        >
                            <option value="">...</option>
                            {databases}
                        </Form.Control>
                        <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                        <Form.Control.Feedback type="invalid" >
                            Select DB type
                        </Form.Control.Feedback>
                    </Form.Group>
                </Col>
                <Col xs="auto">
                    <Form.Group className="mb-3" controlId="dbname">
                        <Form.Label>{tooltip('Dymium name',
                            <div className="d-block">
                                The name is used to identify the target database from the SQL sent to the Dymium proxy server.
                                For example, instead of
                                <div className='ml-2 my-1'>select * from mytable;</div>
                                you should use:
                                <div className='ml-2 my-1'>select * from name_mytable;</div>
                            </div>
                            , 'auto', '', false)}</Form.Label>
                        <Form.Control size="sm" type="text" placeholder="alphanum, _$^!"
                            required
                            pattern="[a-zA-Z0-9_^$]+"
                            value={props.name}
                            onChange={e => props.setName(e.target.value)}
                        />
                        <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                        <Form.Control.Feedback type="invalid" >
                            Type systemwide unique name to use in SQL
                        </Form.Control.Feedback>
                    </Form.Group>
                </Col>
            </Row>
            <Row>
                <Col xs="auto">
                    <Form.Group className="mb-3" controlId="ipaddress">
                        <Form.Label>Address</Form.Label>
                        <Form.Control size="sm" type="text" placeholder="DB IP address or host name"
                            required
                            pattern="^[a-zA-Z0-9._]+$"
                            value={props.address}
                            onChange={e => props.setAddress(e.target.value)}
                        />
                        <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                        <Form.Control.Feedback type="invalid" >
                            Ender DB address for Dymium
                        </Form.Control.Feedback>
                    </Form.Group>
                </Col>
                <Col xs="auto">
                    <Form.Group className="mb-3" controlId="portnumber">
                        <Form.Label>Port</Form.Label>
                        <Form.Control size="sm" type="number"
                            required
                            pattern=".+"
                            placeholder="DB port number"
                            value={props.port}
                            onChange={e => props.setPort(e.target.value)}
                        />
                        <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                        <Form.Control.Feedback type="invalid" >
                            Select DB port for Dymium
                        </Form.Control.Feedback>
                    </Form.Group>
                </Col>
                <Col xs="auto" style={{ display: 'flex', alignItems: 'bottom' }}>
                    <Form.Group className="mb-3" controlId="usetls">
                        <Form.Label>&nbsp;</Form.Label>
                        <Form.Check
                            style={{ marginTop: '0.2em' }}
                            type="checkbox"
                            label="Use TLS"
                            id="usetls"
                            checked={props.useTLS}
                            onChange={e => props.setUseTLS(e.target.checked)}
                        />
                    </Form.Group>
                </Col>
            </Row>
            {props.context === "edit" &&
                <Row>
                    <Col>
                        <Form.Group className="mb-1" controlId="usetls">

                            <Form.Check
                                style={{ marginTop: '0.2em' }}
                                type="checkbox"
                                label="Change credentials"
                                id="changecred"
                                defaultChecked={props.cred}
                                onChange={e => {
        
                                    props.setPassword("")
                                    props.setUsername("")                                    
                                    props.setCred(e.target.checked)
                                }}
                            />
                        </Form.Group>
                    </Col>
                </Row>
            }
            {props.cred &&
                <Row>
                    <Col xs="auto">
                        <Form.Group className="mb-3" controlId="dbusername">
                            <Form.Label>Username</Form.Label>
                            <Form.Control size="sm" type="text" placeholder="DB username"
                                required
                                pattern=".+"
                                value={props.username}
                                onChange={e => props.setUsername(e.target.value)}
                            />
                            <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                            <Form.Control.Feedback type="invalid" >
                                Admin name for DB
                            </Form.Control.Feedback>
                        </Form.Group>

                    </Col>
                    <Col xs="auto">
                        <Form.Group className="mb-3" controlId="dbpassword">
                            <Form.Label>Password</Form.Label>
                            <PasswordField type="password"
                                required
                                placeholder="DB password"
                                pattern=".+"
                                validfeedback="Looks good!"
                                invalidfeedback="Admin password"
                                value={props.password}
                                className="w-12em"
                                onChange={e => props.setPassword(e.target.value)}
                                size="sm" />
                        </Form.Group>
                    </Col>
                </Row>
            }
            <Row>
                <Col xs="auto">
                    <Form.Group className="mb-3" controlId="description">
                        <Form.Label>Description</Form.Label>
                        <Form.Control as="textarea" rows={3} style={{ width: '35em' }}
                            required
                            placeholder="Please put in the description of this connection"
                            onChange={e => props.setDescription(e.target.value)}
                            value={props.description}
                        />
                        <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                        <Form.Control.Feedback type="invalid" >
                            Please put in some description
                        </Form.Control.Feedback>
                    </Form.Group>
                </Col>
            </Row>
        </>
    )
}
function AddConnection() {
    const [validated, setValidated] = useState(false)
    let form = useRef<HTMLFormElement>(null)

    const [name, setName] = useState("")
    const [dbtype, setDBType] = useState("")
    const [address, setAddress] = useState("")
    const [port, setPort] = useState("")
    const [useTLS, setUseTLS] = useState(false)
    const [username, setUsername] = useState("")
    const [password, setPassword] = useState("")
    const [description, setDescription] = useState("")
    const [spinner, setSpinner] = useState(false)
    const [alert, setAlert] = useState(<></>)

    let sendConnection = () => {
        setSpinner(true)
        let body = JSON.stringify({ name, dbtype, address, port: parseInt(port), useTLS, username, password, description })
        com.sendToServer("POST", "/api/createnewconnection",
            null, body,
            resp => {
                resp.json().then(js => {
                    if (js.Status == "OK") {
                        console.log("on success")
                        setName("")
                        setDBType("")
                        setAddress("")
                        setPort("")
                        setUseTLS(false)
                        setUsername("")
                        setPassword("")
                        setDescription("")

                        setAlert(
                            <Alert variant="success" onClose={() => setAlert(<></>)} dismissible>
                                Connection {name} created successfully!
                            </Alert>
                        )
                    } else {
                        console.log("on error");
                        setAlert(
                            < Alert variant="danger" onClose={() => setAlert(<></>)} dismissible >
                                Error: {js.Text} !
                            </Alert >)
                    }
                    setSpinner(false)

                }).catch((error) => {

                })
            },
            resp => {
                console.log("on error")
                setSpinner(false)
                setAlert(
                    <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        Error creating connection.
                    </Alert>
                )

            },
            error => {
                console.log("on exception")
                setSpinner(false)
                setAlert(
                    <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        Error creating connection: {error.message} { }
                    </Alert>
                )
            })
    }

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

        sendConnection()

        return false
    }

    return (
        <div className=" text-left">
            {alert}
            <h5 > Create New Connection <Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>

            <div className=" text-left">
                <Form onSubmit={handleSubmit} ref={form} noValidate validated={validated}>
                    <ConnectionForm
                        dbtype={dbtype}
                        setDBType={setDBType}
                        name={name}
                        setName={setName}
                        address={address}
                        setAddress={setAddress}
                        port={port}
                        setPort={setPort}
                        useTLS={useTLS}
                        setUseTLS={setUseTLS}
                        username={username}
                        setUsername={setUsername}
                        password={password}
                        setPassword={setPassword}
                        description={description}
                        setDescription={setDescription}
                        context="add"
                        cred={true}
                    />

                    <Button variant="dymium" className="mt-4" type="submit">
                        Apply
                    </Button>
                </Form>
            </div>
        </div>
    )
}

function EditConnections(props) {
    let [conns, setConns] = useState([])
    const [spinner, setSpinner] = useState(false)
    const [showdelete, setShowdelete] = useState(false)
    const [selectedId, setSelectedId] = useState(0)
    const [showedit, setShowedit] = useState(false)

    const [name, setName] = useState("")
    const [dbtype, setDBType] = useState("")
    const [address, setAddress] = useState("")
    const [port, setPort] = useState("")
    const [useTLS, setUseTLS] = useState(false)
    const [username, setUsername] = useState("")
    const [password, setPassword] = useState("")
    const [description, setDescription] = useState("")
    const [validated, setValidated] = useState(false)
    const [cred, setCred] = useState(false)
    const [alert, setAlert] = useState(<></>)
    let form = useRef<HTMLFormElement>(null)

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
        setValidated(true)
        event.stopPropagation();
        setShowedit(false)
        updateConnection()

        return false

    }
    let onEdit = id => {
        return e => {
            setSelectedId(id)
            let conn = null
            conns.forEach(c => {

                if (c['id'] === id)
                    conn = c
            })
            if (conn != null) {

                setName(conn["name"])
                setDBType(conn["dbtype"])
                setAddress(conn["address"])
                setPort(conn["port"])
                setUsername(conn["username"])
                setPassword("")
                setCred(false)
                setUseTLS(conn["usetls"])
                setDescription(conn["description"])
            }
            setValidated(false)
            setShowedit(true)
        }
    }
    let onDelete = id => {
        return e => {
            setSelectedId(id)
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
            dataField: 'credid',
            text: 'credid',
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
            dataField: 'address',
            text: 'Address:',
            sort: true
        },

        {
            dataField: 'port',
            text: 'Port:',
            headerStyle: { width: '100px' },
            sort: true
        },
        {
            dataField: 'description',
            text: 'Description:',
            sort: true
        }, {
            dataField: 'usetls',
            text: 'Use TLS',
            formatter: (cell, row, rowIndex, formatExtraData) => {

                if (row.usetls)
                    return <i className="fa-solid fa-check blue"></i>
                else
                    return <></>
            },
            headerStyle: { width: '130px' },
            sort: true,
            align: 'center'
        },

        {
            text: 'Edit',
            dataField: 'edit',
            isDummyField: true,
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
    let getConnections = () => {
        setSpinner(true)
        setConns([])
        com.sendToServer("GET", "/api/getconnections",
            null, "",
            resp => {

                resp.json().then(js => {

                    let cc = js.map(x => {
                        return {
                            id: x.id,
                            credid: x.credid,
                            dbtype: x.dbtype,
                            name: x.name,

                            address: x.address,
                            port: x.port,
                            description: x.description,
                            usetls: x.useTLS,

                        }
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

    let updateConnection = () => {
        let body = {
            Id: selectedId,
            Name: name,
            DbType: dbtype,
            Address: address,
            Port: port,
            UseTLS: useTLS,
            Description: description
        }
        if (cred) {
            body["Username"] = username
            body["Password"] = password
        }
        setSpinner(true)

        com.sendToServer("POST", "/api/updateconnection",
            "", JSON.stringify(body),
            resp => {

                setSpinner(false)

                console.log("on success")
                setShowedit(false)
                getConnections()
            },
            resp => {
                console.log("on error")
                setSpinner(false)
                setShowedit(false)
                getConnections()
            },
            error => {
                console.log("on exception: " + error)
                setSpinner(false)
                setShowedit(false)
                getConnections()
            })
    }

    let deleteConnection = () => {
        let body = {
            Id: selectedId,
        }

        setSpinner(true)
        com.sendToServer("POST", "/api/deleteconnection",
            "", JSON.stringify(body),
            resp => {
                resp.json().then(js => {
                    debugger
                    console.log("json: " + js)
                    if (js.Status === "OK") {
                        setAlert(
                            <Alert variant="success" onClose={() => setAlert(<></>)} dismissible>
                                Connection {name} deleted successfully!
                            </Alert>
                        )
                    } else {
                        setAlert(
                            <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                                Error deleting connection {name}: {js.Text}
                            </Alert>
                        )
                    }
                    setSpinner(false)
                    console.log("on success")
                    setShowdelete(false)
                    getConnections()
                }).catch(error => {
                    console.log("Error: " + error.message)
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Error deleting connection {name}: {error.message}
                        </Alert>
                    )
                    setSpinner(false)
                    console.log("on exception")
                    setShowdelete(false)
                    getConnections()
                })
            },
            resp => {
                console.log("on error")
                setAlert(
                    <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        Error deleting connection {name}
                    </Alert>
                )
                setSpinner(false)
                setShowdelete(false)
                getConnections()
            },
            error => {
                console.log("on exception: " + error)
                setAlert(
                    <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        Error deleting connection {name}: {error.message}
                    </Alert>
                )
                setSpinner(false)
                setShowdelete(false)
                getConnections()
            })
    }

    useEffect(() => {
        getConnections()
    }, [])
    let connectionName = () => {
        let ret = ""
        conns.forEach(c => {

            if (c['id'] === selectedId)
                ret = c['name']
        })
        return ret
    }
    return (

        <div className=" text-left">

            <Modal centered show={showdelete} onHide={() => setShowdelete(false)} >
                <Modal.Header closeButton>
                    <Modal.Title>Delete connection {connectionName()}?</Modal.Title>
                </Modal.Header>
                <Modal.Body>Are you sure you want to remove the connection? This operation is irreversible.</Modal.Body>
                <Modal.Footer>
                    <Button variant="danger" onClick={() => {
                        deleteConnection()
                        //setShowdelete(false)
                    }
                    }>Delete</Button> <Button variant="dymium" onClick={() => {
                        setShowdelete(false)
                    }}>Cancel</Button>
                </Modal.Footer>
            </Modal>

            <Modal size="lg" show={showedit} onHide={() => setShowedit(false)} >
                <Form onSubmit={handleSubmit} ref={form} noValidate validated={validated}>
                    <Modal.Header closeButton>
                        <Modal.Title>Edit connection {connectionName()}</Modal.Title>
                    </Modal.Header>
                    <Modal.Body>
                        <ConnectionForm
                            dbtype={dbtype}
                            setDBType={setDBType}
                            name={name}
                            setName={setName}
                            address={address}
                            setAddress={setAddress}
                            port={port}
                            setPort={setPort}
                            useTLS={useTLS}
                            setUseTLS={setUseTLS}
                            username={username}
                            setUsername={setUsername}
                            password={password}
                            setPassword={setPassword}
                            description={description}
                            setDescription={setDescription}
                            context="edit"
                            cred={cred}
                            setCred={setCred}
                        />



                    </Modal.Body>
                    <Modal.Footer>
                        <Button variant="dymium" type="submit" onClick={() => {
                            
                        }
                        }>Apply</Button> <Button variant="dymium" onClick={() => setShowedit(false)}>Cancel</Button>
                    </Modal.Footer>
                </Form>
            </Modal>

            {conns !== [] &&
                <div id="tablecontainer" style={{ width: '90%' }} className="text-center">
                    <ToolkitProvider
                        bootstrap4
                        keyField='name'
                        data={conns}
                        columns={columns}
                        search >
                        {
                            props => (
                                <div className="text-left">
                                    {alert}
                                    <div className="d-flex">
                                        <h5 >Edit Connections  <Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>


                                        <div style={{ marginLeft: "auto" }}>
                                            <SearchBar size="sm" {...props.searchProps} />
                                            <ClearSearchButton {...props.searchProps} />
                                        </div>
                                    </div>
                                    <div className="d-block">
                                        <BootstrapTable id="scaledtable"
                                            condensed
                                            striped bootstrap4 bordered={false}
                                            pagination={paginationFactory()}
                                            {...props.baseProps}
                                        />
                                    </div>
                                </div>
                            )
                        }
                    </ToolkitProvider>
                </div>}
        </div>
    )
}
function Connections() {
    const t = useAppSelector((state) => {
        
        return state.reducer.activeConnectionTab}
        )
    const appDispatch = useAppDispatch()
    

    return (
        <Tabs defaultActiveKey={t} id="connections" 
        onSelect={(k) => appDispatch( setActiveConnectionTab(k) )}

        unmountOnExit={true} className="mb-3 text-left">
            <Tab eventKey="add" title="Add Connection" className="mx-4">
                <AddConnection />
            </Tab>
            <Tab eventKey="edit" title="Connections" className="mx-4">
                <EditConnections />
            </Tab>
        </Tabs>
    )
}

export default Connections;