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
import Toggle from 'react-toggle'

const { SearchBar, ClearSearchButton } = Search;


import { tooltip } from '@dymium/common/Components/Tooltip'
import PasswordField from '@dymium/common/Components/PasswordField'
import * as com from '../Common'
import * as types from '@dymium/common/Types/Common'
import * as capi from '../Api/Connections'
import * as http from '../Api/Http'
import * as tun from '@dymium/common/Types/Tunnel'
import Spinner from '@dymium/common/Components/Spinner'
import { useInitialize } from '../Utils/CustomHooks'
import { useAppDispatch, useAppSelector } from './hooks'
import { setActiveConnectionTab } from '../Slices/menuSlice'

const databases = Object.keys(com.databaseTypes).map(key => {
    return <option key={key} value={key}>
        {com.databaseTypes[key]}
    </option>
})

function ConnectionForm(props) {
    let [conns, setConns] = useState<tun.Connector[]>([])
    const [alert, setAlert] = useState<JSX.Element>(<></>)
    let getConnectors = () => {

        http.sendToServer("GET", "/api/getconnectors",
            null, "",
            resp => {
                resp.json().then(js => {
                    if (js.errormessage !== undefined) {
                        setAlert(
                            <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                                {js.errormessage}
                            </Alert>
                        )
                        return
                    }
                    setConns(js)
                }).catch((error) => {
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Invalid server response.
                        </Alert>
                    )
                })
            },
            resp => {
                setAlert(
                    <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        Error retrieving access key.
                    </Alert>
                )
            },
            error => {
                setAlert(
                    <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        Error retrieving access key: {error.message} { }
                    </Alert>
                )
            })
    }
    useEffect(() => {
        getConnectors()
    }, [])
    let getConnectorOptions = () => {

        let out: any[] = [<option value="">...</option>]
        conns.forEach((x: any) => {
            out.push(
                <option key={x.id} value={x.id}>{x.name}</option>
            )
        })
        return out
    }
    let getTunnelOptions = () => {
        let out: any[] = [<option value="">...</option>]
        if (props.connector === "")
            return out
        // find the connector 
        let c = undefined
        for (let i = 0; i < conns.length; i++) {

            if (conns[i]["id"] === props.connectorid) {

                conns[i].tunnels.forEach(x => {
                    if (x.id != null)
                        out.push(
                            <option key={x.id} value={x.id}>{x.name}</option>
                        )
                })
                return out
                break
            }
        }
        if (c === undefined) return out //

        return out
    }
    return (
        <>
            {alert}
            <Row>
                <Col xs="auto">
                    <Form.Group className="mb-3" controlId="dbtype" >
                        <Form.Label >Database type</Form.Label>
                        <Form.Control as="select" required size="sm" value={props.dbtype}
                            onChange={e => {
                                let key = e.target.value
                                if (key !== "")
                                    props.setPort(com.databasePorts[key])
                                else
                                    props.setPort("")
                                props.setDBType(key)
                            }}
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
                    <Form.Group className="mb-3" controlId="dname">
                        <Form.Label>{tooltip('Dymium name',
                            <div className="d-block">
                                The name is used to identify the target data source in the Ghost Database - virtual database with controlled access.

                            </div>
                            , 'auto', '', false)}</Form.Label>
                        <Form.Control size="sm" type="text" placeholder="alpha_num, small caps"
                            required
                            pattern="[a-z0-9_]+"
                            value={props.name}
                            onChange={e => props.setName(e.target.value)}
                        />
                        <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                        <Form.Control.Feedback type="invalid" >
                            Type systemwide unique name to use in SQL
                        </Form.Control.Feedback>
                    </Form.Group>
                </Col>
                <Col xs="auto">
                    {props.dbtype !== 'MySQL' && props.dbtype !== 'MariaDB' &&
                        <Form.Group className="mb-3" controlId="dbname">
                            <Form.Label>{tooltip('Database name',
                                <div className="d-block">
                                    The database name used as the backend connection parameter.

                                </div>
                                , 'auto', '', false)}</Form.Label>
                            <Form.Control size="sm" type="text" placeholder="Alpha_Num"
                                required
                                pattern=".+"
                                value={props.dbname}
                                onChange={e => props.setDbName(e.target.value)}
                            />
                            <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                            <Form.Control.Feedback type="invalid" >
                                Type systemwide unique name to use in SQL
                            </Form.Control.Feedback>
                        </Form.Group>
                    }
                </Col>
            </Row>
            {conns.length > 0 &&
                < Row >
                <Col className="mt-1" style={{ paddingLeft: '1.5em' }}>
                    <Toggle className="yellowtoggle"
                        id='conn-status'
                        checked={props.usesconnector}
                        onChange={(e) => {

                            props.setUsesconnector(e.target.checked)
                        }}



                    />
                    <label className="form-check-label" style={{ marginLeft: '0.5em', position: 'relative', top: '-0.38em' }} htmlFor='conn-status'>Use Connector instead of direct addressing</label>
                </Col>
            </Row>
            }
<Row>
    {props.usesconnector ?
        <>
            <Col xs="auto">
                <Form.Group className="mb-3" controlId="connector" >
                    <Form.Label >Connector</Form.Label>
                    <Form.Control as="select" required size="sm"
                        value={props.connectorid}
                        onChange={e => {

                            let key = e.target.value
                            if (key !== "")
                                props.setConnectorid(key)
                            else
                                props.setConnectorid("")

                        }}
                    >

                        {getConnectorOptions()}
                    </Form.Control>
                    <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                    <Form.Control.Feedback type="invalid" >
                        Select Connector
                    </Form.Control.Feedback>
                </Form.Group>
            </Col>
            <Col xs="auto">
                <Form.Group className="mb-3" controlId="tunnrl" >
                    <Form.Label >Connector</Form.Label>
                    <Form.Control as="select" required size="sm"
                        value={props.tunnelid}
                        onChange={e => {

                            let key = e.target.value
                            if (key !== "")
                                props.setTunnelid(key)
                            else
                                props.setTunnelid("")

                        }}
                    >

                        {getTunnelOptions()}
                    </Form.Control>
                    <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                    <Form.Control.Feedback type="invalid" >
                        Select Connector
                    </Form.Control.Feedback>
                </Form.Group>
            </Col>
        </>
        :
        <>
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
        </>
    }
    <Col xs="auto" style={{ display: 'flex', alignItems: 'bottom' }}>
        <div style={{ marginTop: '1.8em' }}>
            <Toggle className="yellowtoggle"
                id='tls-status'
                checked={props.useTLS}
                onChange={e => props.setUseTLS(e.target.checked)}
            />
            <label className="form-check-label" style={{ marginLeft: '0.5em', position: 'relative', top: '-0.38em' }} htmlFor='tls-status'>Use TLS</label>
        </div>
    </Col>
</Row>
{
    props.context === "edit" &&
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
{
    props.cred &&
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
    <Col xs>
        <Form.Group className="mb-3" controlId="description">
            <Form.Label>Description</Form.Label>
            <Form.Control as="textarea" rows={3} style={{ width: '100%' }}
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
export function AddConnection() {
    const [validated, setValidated] = useState(false)
    let form = useRef<HTMLFormElement>(null)

    const [name, setName] = useState("")
    const [dbname, setDbName] = useState("")
    const [dbtype, setDBType] = useState("")
    const [address, setAddress] = useState("")
    const [port, setPort] = useState("")
    const [useTLS, setUseTLS] = useState(false)
    const [username, setUsername] = useState("")
    const [password, setPassword] = useState("")
    const [description, setDescription] = useState("")
    const [spinner, setSpinner] = useState(false)
    const [alert, setAlert] = useState<JSX.Element>(<></>)

    const [connectorid, setConnectorid] = useState("")
    const [connectorName, setConnectorName] = useState("")
    const [tunnelid, setTunnelid] = useState("")
    const [tunnelName, setTunnelName] = useState("")
    const [usesconnector, setUsesconnector] = useState(false)

    let sendConnection = () => {
        setSpinner(true)
        let body = JSON.stringify({
            name, dbtype, address, port: parseInt(port),
            dbname, useTLS, username, password, description,
            usesconnector: usesconnector, connectorname: connectorName,
            connectorid, tunnelname: tunnelName, tunnelid
        })
        http.sendToServer("POST", "/api/createnewconnection",
            null, body,
            resp => {
                resp.json().then(js => {
                    if (js.status == "OK") {
                        setName("")
                        setDbName("")
                        setDBType("")
                        setAddress("")
                        setPort("")
                        setUseTLS(false)
                        setUsername("")
                        setPassword("")
                        setDescription("")
                        setConnectorid("")
                        setConnectorName("")
                        setTunnelid("")
                        setTunnelName("")
                        setUsesconnector(false)
                        setAlert(
                            <Alert variant="success" onClose={() => setAlert(<></>)} dismissible>
                                Connection {name} created successfully!
                            </Alert>
                        )
                    } else {
                        setAlert(
                            < Alert variant="danger" onClose={() => setAlert(<></>)} dismissible >
                                Error: {js.errormessage} !
                            </Alert >)
                    }
                    setTimeout(() => setSpinner(false), 500)

                }).catch((error) => {

                })
            },
            resp => {
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
            <h5 > Connect to New Data Source <Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>

            <div className=" text-left">
                <Form onSubmit={handleSubmit} ref={form} noValidate validated={validated}>
                    <ConnectionForm
                        dbtype={dbtype}
                        setDBType={setDBType}
                        name={name}
                        setName={setName}
                        dbname={dbname}
                        setDbName={setDbName}
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


                        connectorid={connectorid}
                        setConnectorid={setConnectorid}
                        connectorName={connectorName}
                        setConnectorName={setConnectorName}
                        tunnelid={tunnelid}
                        setTunnelid={setTunnelid}
                        tunnelName={tunnelName}
                        setTunnelName={setTunnelName}
                        usesconnector={usesconnector}
                        setUsesconnector={setUsesconnector}

                        context="add"
                        cred={true}
                    />

                    <Button variant="dymium" size="sm" className="mt-4" type="submit">
                        Apply
                    </Button>
                </Form>
            </div>
        </div>
    )
}

export function EditConnections(props) {
    let [conns, setConns] = useState([])
    const [spinner, setSpinner] = useState(false)
    const [showdelete, setShowdelete] = useState(false)
    const [selectedId, setSelectedId] = useState(0)
    const [showedit, setShowedit] = useState(false)

    const [name, setName] = useState("")
    const [dbname, setDbName] = useState("")
    const [dbtype, setDBType] = useState("")
    const [address, setAddress] = useState("")
    const [port, setPort] = useState("")
    const [useTLS, setUseTLS] = useState(false)
    const [username, setUsername] = useState("")
    const [password, setPassword] = useState("")
    const [description, setDescription] = useState("")
    const [connectorid, setConnectorid] = useState("")
    const [connectorName, setConnectorName] = useState("")
    const [tunnelid, setTunnelid] = useState("")
    const [tunnelName, setTunnelName] = useState("")
    const [usesconnector, setUsesconnector] = useState(false)
    const [validated, setValidated] = useState(false)
    const [cred, setCred] = useState(false)
    const [alert, setAlert] = useState<JSX.Element>(<></>)
    let form = useRef<HTMLFormElement>(null)

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
                setDbName(conn["dbname"])
                setAddress(conn["address"])
                setPort(conn["port"])
                setUsername(conn["username"])
                setPassword("")
                setCred(false)
                setUseTLS(conn["useTLS"])
                setDescription(conn["description"])
                setConnectorid(conn["connectorid"])
                setConnectorName(conn["connectorname"])
                setTunnelid(conn["tunnelid"])
                setTunnelName(conn["tunnelname"])
                setUsesconnector(conn["usesconnector"])

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
            text: 'Target:',
            sort: true,
            formatter: (cell, row, rowIndex, formatExtraData) => {

                if (row.usesconnector) {
                    return <div className="d-flex " ><i className="mr-2 fas fa-diagram-project fa-fw blue" style={{ position: 'relative', top: '0.3em' }}></i>{row.connectorname}/{row.tunnelname}</div>
                }

                return <div className="d-flex"><i className=" mr-2 fa fa-cloud-arrow-up blue" style={{ position: 'relative', top: '0.3em' }}></i>{row.address}:{row.port}</div>
            },
        },
        {
            dataField: 'dbname',
            text: 'Database:',
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

                if (row.useTLS)
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

                return <i className="fas fa-edit ablue" aria-label={"edit" + rowIndex} id={"edit" + rowIndex} onClick={onEdit(row["id"])} role="button"></i>
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
                return <i className="fas fa-trash ablue" aria-label={"delete" + rowIndex} id={"delete" + rowIndex} onClick={onDelete(row["id"])} role="button"></i>
            },
            //formatExtraData: { hoverIdx: this.state.hoverIdx },
            headerStyle: { width: '90px' },
            style: { height: '30px' },
            align: 'center'
        }

    ]

    let updateConnection = () => {
        let body = {
            id: selectedId,
            name: name,
            dbtype: dbtype,
            address: address,
            port: parseInt(port),
            dbname: dbname,
            useTLS: useTLS,
            description: description,
            usesconnector: usesconnector,
            connectorid: connectorid,
            tunnelid: tunnelid
        }
        if (cred) {
            body["username"] = username
            body["password"] = password
        }
        setSpinner(true)

        http.sendToServer("POST", "/api/updateconnection",
            "", JSON.stringify(body),
            resp => {
                resp.json().then(js => {

                    if (js.status === "OK") {
                        setAlert(
                            <Alert variant="success" onClose={() => setAlert(<></>)} dismissible>
                                Connection {name} updated successfully!
                            </Alert>
                        )
                    } else {
                        setAlert(
                            <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                                Error updating connection {name}: {js.errormessage}
                            </Alert>
                        )
                    }
                }).catch(error => {
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Error updating connection {name}
                        </Alert>
                    )
                })

                setSpinner(false)
                setShowedit(false)
                capi.getConnections(setSpinner, setConns, setAlert, undefined, () => { })
            },
            resp => {
                console.log("on error")
                setSpinner(false)
                setShowedit(false)
                capi.getConnections(setSpinner, setConns, setAlert, undefined, () => { })
            },
            error => {
                console.log("on exception: " + error)
                setSpinner(false)
                setShowedit(false)
                capi.getConnections(setSpinner, setConns, setAlert, undefined, () => { })
            })
    }

    let deleteConnection = () => {
        let body = {
            Id: selectedId,
        }

        setSpinner(true)
        http.sendToServer("POST", "/api/deleteconnection",
            "", JSON.stringify(body),
            resp => {
                resp.json().then(js => {
                    if (js.status === "OK") {
                        setAlert(
                            <Alert variant="success" onClose={() => setAlert(<></>)} dismissible>
                                Connection {name} deleted successfully!
                            </Alert>
                        )
                    } else {
                        setAlert(
                            <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                                Error deleting connection {name}: {js.errormessage}
                            </Alert>
                        )
                    }
                    setSpinner(false)
                    setShowdelete(false)
                    capi.getConnections(setSpinner, setConns, setAlert, undefined, () => { })
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
                    capi.getConnections(setSpinner, setConns, setAlert, undefined, () => { })
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
                capi.getConnections(setSpinner, setConns, setAlert, undefined, () => { })
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
                capi.getConnections(setSpinner, setConns, setAlert, undefined, () => { })
            })
    }

    useEffect(() => {
        capi.getConnections(setSpinner, setConns, setAlert, undefined, () => { })
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

            <Modal centered show={showdelete} onHide={() => setShowdelete(false)} data-testid="modal-delete">
                <Modal.Header closeButton>
                    <Modal.Title>Delete connection {connectionName()}?</Modal.Title>
                </Modal.Header>
                <Modal.Body>Are you sure you want to remove the connection? This operation is irreversible.</Modal.Body>
                <Modal.Footer>
                    <Button variant="danger" role="button" id="Delete" data-testid="Delete"
                        aria-label={"Delete"}
                        onClick={() => {
                            deleteConnection()
                        }
                        }>Delete</Button> <Button variant="dymium" onClick={() => {
                            setShowdelete(false)
                        }}>Cancel</Button>
                </Modal.Footer>
            </Modal>

            <Modal size="lg" show={showedit} onHide={() => setShowedit(false)} data-testid="modal-edit">
                <Form onSubmit={handleSubmit} ref={form} noValidate validated={validated}>
                    <Modal.Header closeButton>
                        <Modal.Title>Edit Data Source {connectionName()}</Modal.Title>
                    </Modal.Header>
                    <Modal.Body>
                        <ConnectionForm
                            dbtype={dbtype}
                            setDBType={setDBType}
                            name={name}
                            setName={setName}
                            dbname={dbname}
                            setDbName={setDbName}
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
                            connectorid={connectorid}
                            setConnectorid={setConnectorid}
                            connectorName={connectorName}
                            setConnectorName={setConnectorName}
                            tunnelid={tunnelid}
                            setTunnelid={setTunnelid}
                            tunnelName={tunnelName}
                            setTunnelName={setTunnelName}
                            usesconnector={usesconnector}
                            setUsesconnector={setUsesconnector}
                            context="edit"
                            cred={cred}
                            setCred={setCred}
                        />



                    </Modal.Body>
                    <Modal.Footer>
                        <Button variant="dymium" type="submit" role="button" id="Apply"
                            aria-label={"Apply"}
                            onClick={() => {

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
                                        <h5 >Edit Data Sources  <Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>


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

        return state.reducer.activeConnectionTab
    }
    )
    const appDispatch = useAppDispatch()


    return (
        <Tabs defaultActiveKey={t} id="connections"
            onSelect={(k) => appDispatch(setActiveConnectionTab(k))}

            unmountOnExit={true} className="mb-3 text-left">
            <Tab eventKey="add" title="Add" className="mx-4">
                <AddConnection />
            </Tab>
            <Tab eventKey="edit" title="Data Sources" className="mx-4">
                <EditConnections />
            </Tab>
        </Tabs>
    )
}

export default Connections;