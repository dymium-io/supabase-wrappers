import React, { useEffect, useState, useRef } from 'react';
import Tabs from 'react-bootstrap/Tabs'
import Tab from 'react-bootstrap/Tab'
import Form from 'react-bootstrap/Form'
import Button from 'react-bootstrap/Button'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Modal from 'react-bootstrap/Modal'
import Alert from 'react-bootstrap/Alert'
import { Link } from "react-router-dom";
import { useLocation, useNavigate } from "react-router-dom";
import * as tun from '@dymium/common/Types/Tunnel'
import BootstrapTable from 'react-bootstrap-table-next';
import paginationFactory from 'react-bootstrap-table2-paginator';
import ToolkitProvider, { Search } from 'react-bootstrap-table2-toolkit';
import 'react-bootstrap-table-next/dist/react-bootstrap-table2.min.css';
import Offcanvas from '@dymium/common/Components/Offcanvas'
const { SearchBar, ClearSearchButton } = Search;
import ReactHtmlParser from 'html-react-parser';

import { tooltip } from '@dymium/common/Components/Tooltip'
import PasswordField from '@dymium/common/Components/PasswordField'
import * as com from '../Common'
import * as types from '@dymium/common/Types/Common'
import * as capi from '../Api/Connections'
import * as http from '@dymium/common/Api/Http'
import Spinner from '@dymium/common/Components/Spinner'
import { useInitialize } from '../Utils/CustomHooks'
import { useAppDispatch, useAppSelector } from './hooks'
import { setActiveConnectorTab, setSelectedConnectorDefault } from '../Slices/menuSlice'

const guidance = 'Please save the ID, access key and the secret. They will go into the Connector configuration running in your address space. The secret will only be accessible for 24 hours once the Connector is provisioned but you can always regenerate it.'
const databases = Object.keys(com.databaseTypes).map(key => {
    return <option key={key} value={key}>
        {com.databaseTypes[key]}
    </option>
})

function Downloads(props) {
    const docker = "public.ecr.aws/a9d3u0m7/dymiumconnector:latest"
    let copydocker = e => {
        navigator.clipboard.writeText(docker);
    }
    return <div className=" text-left">

        <h5 > Connector Binary Downloads</h5>
        <div className="viewport">
            <div>For MS Windows:</div>
            <a href="/meshconnector_windows_amd64.zip" download="meshconnector.zip"> <i className="fab fa-windows mr-2" aria-hidden="true"></i>Click to Download MS Windows Connector</a>
            <div>

                <div style={{ display: "flex" }}>
                    Usage: <div className="terminal ml-2">
                        &gt;meshconnector.exe
                    </div>

                </div>
            </div>
        </div>
        <div className="viewport">
            <div>For x64 Linux:</div>
            <a href="/meshconnector_linux_amd64.tgz" download="meshconnector.tar.gz"> <i className="fab fa-linux mr-2" aria-hidden="true"></i>Click to Download Linux Connector</a>
            <div>

                <div style={{ display: "flex" }}>
                    Usage: <div className="terminal ml-2">
                        &gt;./meshconnector
                    </div>

                </div>
            </div>
        </div>

        <div className="viewport">
            <div>For Mac OS X:</div>
            <a href="/meshconnector_darwin_amd64.tgz" download="meshconnector.tar.gz"> <i className="fab fa-apple mr-2" aria-hidden="true"></i>Click to Download Mac OS Connector</a>
            <div>

                <div style={{ display: "flex" }}>
                    Usage: <div className="terminal ml-2">
                        &gt;./meshconnector
                    </div>

                </div>
            </div>
        </div>
        <div className="viewport">
            <div>A Docker container:</div>
            <i className="fab fa-docker thickblue" style={{ marginRight: '0.3em' }} ></i>{docker}<i style={{ marginTop: '0.1em', marginLeft: '0.3em' }} onClick={copydocker} className="fas fa-copy clipbtn"></i>

        </div>
        You run the Connector in the address space of your Data Source. The configuration is passed in the form of environment variables.
        <ContainerHelp />
    </div>

}
function ConnectionForm(props) {
    let regenerate = () => {

        getAccessKey(props.setKey, props.setSecret, props.setSpinner, props.setAlert)
    }
    let displayTunnels = () => {
        let out: JSX.Element[] = []

        for (let i = 0; i < props.tunnel.length; i++) {
            let addTunnel = e => {
                let tunnel = [...props.tunnel]
                tunnel.push({ id: "", name: "", address: "", port: 0 })
                props.setTunnel(tunnel)
            }
            let removeTunnel = e => {
                let tunnel = [...props.tunnel]
                tunnel.splice(i, 1)
                props.setTunnel(tunnel)
            }
            let setName = n => {
                let tunnel = [...props.tunnel]
                tunnel[i].name = n
                props.setTunnel(tunnel)
            }
            let setAddress = n => {
                let tunnel = [...props.tunnel]
                tunnel[i].address = n
                props.setTunnel(tunnel)
            }
            let setPort = n => {
                let tunnel = [...props.tunnel]
                tunnel[i].port = n
                props.setTunnel(tunnel)
            }
            let graphStatus = () => {
                switch(props.tunnel[i].status) {
                    case "active" : return <i className="fa-solid fa-thumbs-up "></i>
                    case "configured": return <i className="fa-solid fa-thumbs-down "></i>
                    default: return <i className="fa-solid fa-gears "></i>

                } 
            }
            out.push(
                <Row key={"tunnel" + i} className={i % 2 ? "palegray" : "palergray"}>
                    <Col >
                        <Form.Group className="mb-3" controlId={"tname" + i}>
                            <Form.Label>Name:</Form.Label>
                            <Form.Control size="sm" type="text" placeholder="Tunnel name"
                                required
                                pattern="[a-zA-Z0-9_ '$#@]+"
                                value={props.tunnel[i].name}
                                onChange={e => setName(e.target.value)}
                            />
                            <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                            <Form.Control.Feedback type="invalid" >
                                Enter Tunnel Name
                            </Form.Control.Feedback>
                        </Form.Group>
                    </Col>
                    <Col xs="auto">
                        <Form.Group className="mb-3" controlId={"ipaddress" + i}>
                            <Form.Label>Address:</Form.Label>
                            <Form.Control size="sm" type="text" placeholder="DB IP address or host name"
                                required
                                pattern="^[a-zA-Z0-9._-]+$"
                                value={props.tunnel[i].address}
                                onChange={e => setAddress(e.target.value)}
                            />
                            <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                            <Form.Control.Feedback type="invalid" >
                                Enter Data Source Address
                            </Form.Control.Feedback>
                        </Form.Group>
                    </Col>
                    <Col xs="auto">
                        <Form.Group className="mb-3" controlId={"portnumber" + i}>
                            <Form.Label>Port:</Form.Label>
                            <Form.Control size="sm" type="number"
                                required
                                pattern=".+"
                                placeholder="DB port number"
                                value={props.tunnel[i].port}
                                onChange={e => setPort(e.target.value)}
                            />
                            <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                            <Form.Control.Feedback type="invalid" >
                                Select Data Source port
                            </Form.Control.Feedback>
                        </Form.Group>
                    </Col>
                    <Col xs="auto">
                        <div>Status:</div>
                        <div style={{ marginTop: '0.3em' }} className="thickblue">{graphStatus()} {tun.humanReadableTunnelStatus(props.tunnel[i].status)}</div>
                    </Col>
                    <Col xs="auto" as="div" className="text-right mx-0 mt-0 px-1 aligh-top">
                        <i hidden={i !== props.tunnel.length - 1} className="far fahover fa-plus-square aligh-top fa-1x mr-1 plusminus" onClick={addTunnel} ></i>
                        <i hidden={i === props.tunnel.length - 1} className="far fahover fa-plus-square aligh-top fa-1x mr-1 plusminus transparent"> </i>

                        <i hidden={i === 0 && props.tunnel.length === 1} className="far fahover fa-minus-square aligh-top fa-1x plusminus" onClick={removeTunnel} ></i>
                        <i hidden={i !== 0 || props.tunnel.length !== 1} className="far fahover fa-minus-square aligh-top fa-1x plusminus transparent" ></i>
                    </Col>

                </Row>

            )

        }
        return out
    }
    let copysecret = () => {
        return e => {
            if (props.secret !== undefined)
                navigator.clipboard.writeText(props.secret);
        }
    }
    let copykey = () => {
        return e => {
            if (props.accesskey !== undefined)
                navigator.clipboard.writeText(props.accesskey);
        }
    }
    let copyid = () => {
        return e => {
            if (props.accesskey !== undefined)
                navigator.clipboard.writeText(props.id);
        }
    }

    return (
        <>
            <Row>
                <Col xs="auto">
                    <Form.Group className="mb-3" controlId="name">
                        <Form.Label>{tooltip('Connector name',
                            <div className="d-block">
                                The name is used to identify the connector and refer to it from the Ghost Database configuration.

                            </div>
                            , 'auto', '', false)}</Form.Label>
                        <Form.Control size="sm" type="text" placeholder="alpha_num"
                            required

                            pattern="[a-zA-Z0-9_ '$#@]+"
                            value={props.name}
                            onChange={e => props.setName(e.target.value)}
                        />
                        <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                        <Form.Control.Feedback type="invalid" >
                            Type connector's name
                        </Form.Control.Feedback>
                    </Form.Group>
                </Col>
                <Col xs="auto" style={{ display: 'flex' }}  >

                    <Form.Group className="mb-3" controlId="key">
                        <Form.Label
                            hidden={props.context === "add" ? true : false}
                        >{tooltip('Access Key',
                            <div className="d-block">
                                Identifier for Access Secret.

                            </div>
                            , 'auto', '', false)}</Form.Label>
                        <Form.Control size="sm" type="text"
                            hidden={props.context === "add" ? true : false}
                            required
                            pattern=".+"
                            readOnly
                            value={props.accesskey}
                            onChange={e => props.setKey(e.target.value)}
                        />
                        <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                        <Form.Control.Feedback type="invalid" >
                            Type systemwide unique name to use in SQL
                        </Form.Control.Feedback>
                    </Form.Group>

                    <i hidden={props.context === "add" ? true : false}
                        onClick={copykey()} style={{ marginTop: '1.75em' }} className="fas fa-copy clipbtn"></i>

                </Col>
                <Col style={{ display: 'flex' }}>

                    <Form.Group className="mb-3" controlId="secret" >
                        <Form.Label hidden={props.context !== "edit" ? true : false}>{tooltip('Secret',
                            <div className="d-block">
                                Access Secret for the Connector
                            </div>
                            , 'auto', '', false)}</Form.Label>
                        <Form.Control size="sm" type="text"
                            required
                            style={{ width: '300px' }}
                            pattern=".+"
                            readOnly
                            hidden={props.context !== "edit" ? true : false}
                            value={props.secret}
                            onChange={e => props.setSecret(e.target.value)}
                        />
                        <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                        <Form.Control.Feedback type="invalid" >
                            Type systemwide unique name to use in SQL
                        </Form.Control.Feedback>
                    </Form.Group>
                    {(props.context === "edit" && props.secret[0] !== '*') &&
                        <i style={{ marginTop: '1.75em' }} onClick={copysecret()} className="fas fa-copy clipbtn"></i>
                    }
                    {props.context === "edit" &&
                        <Button onClick={regenerate} style={{ marginTop: '2em', paddingBottom: '0.1em', paddingTop: '0.0em', height: '2em' }} variant="dymium" className="mx-2" size="sm">Regenerate</Button>
                    }

                </Col>
            </Row>
            {props.context === "edit" &&
                <Row>
                    <Col xs="auto" className="d-flex">
                        <Form.Group className="mb-3" controlId="cid">
                            <Form.Label>Connector ID:</Form.Label>
                            <Form.Control size="sm" type="text" style={{ width: '24em' }}
                                required

                                readOnly
                                value={props.id}

                            />

                        </Form.Group>
                        <i onClick={copyid()} style={{ marginTop: '1.75em' }} className="fas fa-copy clipbtn"></i>

                    </Col>

                </Row>}
            {(props.context === "edit" && props.secret[0] !== '*') &&
                <div style={{ fontStyle: 'italic', fontWeight: 'bold' }}>{guidance}</div>
            }
            <div className="mt-3 ml-1 mb-1">Configure Tunnels to Data Sources:</div>
            <div className="view  mx-3 mb-2">
                {displayTunnels()}
            </div>
        </>
    )
}

let getAccessKey = (setKey: React.Dispatch<React.SetStateAction<string>>,
    setSecret: React.Dispatch<React.SetStateAction<string>>,
    setSpinner: React.Dispatch<React.SetStateAction<boolean>>,
    setAlert: React.Dispatch<React.SetStateAction<JSX.Element>>
) => {
    setSpinner(true)
    http.sendToServer("GET", "/api/getaccesskey",
        null, "",
        resp => {
            resp.json().then(js => {
                setSpinner(false)

                if (js.errormessage !== undefined) {
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            {js.errormessage}
                        </Alert>
                    )
                    return
                }
                setKey(js.accesskey)
                setSecret(js.secret)
            }).catch((error) => {
                setAlert(
                    <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        Invalid server response.
                    </Alert>
                )
            })
        },
        resp => {
            setSpinner(false)
            resp != null && resp.text().then(t =>
                setAlert(
                    <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        Error retrieving access key: {t}
                    </Alert>
                ))

        },
        error => {
            console.log("on exception")
            setSpinner(false)
            setAlert(
                <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                    Error retrieving access key: {error.message} { }
                </Alert>
            )
        })
}

export function AddConnector() {
    const [validated, setValidated] = useState(false)
    let form = useRef<HTMLFormElement>(null)
    const navigate = useNavigate();
    const [name, setName] = useState("")
    const [key, setKey] = useState("")
    const [secret, setSecret] = useState("qweqwew")
    const [tunnel, setTunnel] = useState<tun.Tunnel[]>([tun.Tunnel.fromJson({ id: "", name: "", address: "", port: "" })])
    const [spinner, setSpinner] = useState(false)
    const [alert, setAlert] = useState<JSX.Element>(<></>)
    const [showOffcanvas, setShowOffcanvas] = useState(com.isInstaller())

    useEffect(() => {
        getAccessKey(setKey, setSecret, setSpinner, setAlert)
    }, [])
    const appDispatch = useAppDispatch()
    let sendConnector = () => {
        setSpinner(true)

        let jsbody = new tun.AddConnectorRequest()
        jsbody.name = name
        jsbody.accesskey = key
        jsbody.secret = secret
        jsbody.tunnels = tunnel

        let body = jsbody.toJson()

        http.sendToServer("POST", "/api/createnewconnector",
            null, body,
            resp => {
                resp.json().then(js => {
                    if (js.status == "OK") {
                        setName("")
                        setKey("")
                        setSecret("")
                        setTunnel([tun.Tunnel.fromJson({ id: "", name: "", address: "", port: "" })])

                        appDispatch(setSelectedConnectorDefault(js.token))
                        appDispatch(setActiveConnectorTab("edit"))
                        navigate('/app/connectors/redirect#bookmark')

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
                if (resp != null) {
                    resp.text().then(t => {
                        setAlert(
                            <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                                Error creating connection {t}
                            </Alert>
                        )
                    })
                }

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

        sendConnector()

        return false
    }

    return (
        <div className=" text-left">
            {alert}
            <h5 > Provision New Connector <i onClick={e => { setShowOffcanvas(!showOffcanvas) }} className="trash fa-solid fa-circle-info"></i><Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>
            <Offcanvas modal={false} width={300} show={showOffcanvas} onClose={(e) => { setShowOffcanvas(false) }}>
                <h5>Provisioning New Connector</h5>
                <div className="mb-3">
                    Dymium offers a reverse tunneling technology solution that facilitates easy access to database servers, regardless of their location.
                </div>
                <div className="mb-3">
                    If you have provisioned Private Link from Dymium to your data sources, you can skip this configuration page and go straight to <Link to="/app/connections">Data Sources</Link>.
                </div>
                <div className="mb-3">
                    Create a new connector by specifying name, target address in your address space, and port. Multiple targets can be specified.
                    After clicking Apply, you will be taken to the Edit Connectors tab that will display the parameters for configuring the connector binary.
                </div>
                <div>
                    You  need to download and run the connector client on your network. <Link to="/app/connectors?key=download">Click here to go to Downloads</Link>
                </div>
                <div>

                </div>
            </Offcanvas>
            <div className=" text-left">
                <Form onSubmit={handleSubmit} ref={form} noValidate validated={validated}>
                    <ConnectionForm

                        name={name}
                        setName={setName}
                        accesskey={key}
                        setKey={setKey}
                        secret={secret}
                        setSecret={setSecret}

                        tunnel={tunnel}
                        setTunnel={setTunnel}

                        context="add"
                    />

                    <Button variant="dymium" size="sm" className="mt-4" type="submit">
                        Apply
                    </Button>
                </Form>
            </div>
        </div>
    )
}

function ContainerHelp() {
    return <>

        <div className="mb-3">
            Below is the complete list of necessary variables, in an example bash script.
        </div>
        <div className="mb-3">
            <div className="d-block " style={{ fontFamily: "monospace", fontSize: "0.6em", overflow: "hidden", color: '#33cc33', backgroundColor: 'black' }}>
                <div className=" text-nowrap p-1" >
                    <div >
                        #!/bin/bash
                    </div><div>
                        export LOG_LEVEL=Info
                    </div><div>
                        export PORTAL=https://portal.dymium.io/
                    </div><div>
                        export CONNECTOR=<span style={{ fontStyle: 'italic' }}>your connector id</span>
                    </div><div>
                        export KEY=<span style={{ fontStyle: 'italic' }}>your secret key</span>
                    </div><div>
                        export SECRET=<span style={{ fontStyle: 'italic' }}>your secret</span>
                    </div><div>
                        export CUSTOMER={com.getTokenProperty("schema")}
                    </div><div>
                        export TUNNELSERVER={com.getTokenProperty("schema")}.dymium.io:3009
                    </div><div>
                        ./meshconnector
                    </div>
                </div>
            </div>

            <div className="my-3">
                Please keep in mind that when you edit tunnels and connectors, the connector binary must be restarted with the updated environment variables!
            </div>

        </div>
    </>
}
export function EditConnectors(props) {
    let [conns, setConns] = useState<tun.Connector[]>([])
    const [showedit, setShowedit] = useState(false)
    const [showdelete, setShowdelete] = useState(false)
    const [selectedId, setSelectedId] = useState("")
    const [validated, setValidated] = useState(false)
    const [name, setName] = useState("")
    const [key, setKey] = useState("")
    const [secret, setSecret] = useState("qweqwew")
    const [tunnel, setTunnel] = useState<tun.Tunnel[]>([tun.Tunnel.fromJson({ id: "", name: "", address: "", port: "" })])
    const [spinner, setSpinner] = useState(false)
    const [alert, setAlert] = useState<JSX.Element>(<></>)
    const [showOffcanvas, setShowOffcanvas] = useState(com.isInstaller())

    let rememberedSelection = useAppSelector((state) => {
        return state.reducer.selectedConnector
    }
    )
    //const [selectedConnector, setSelectedConnector] = useState(t)
    //let setSDRef = useRef(setSelectedConnector)
    const appDispatch = useAppDispatch()

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
        setValidated(false)
        event.stopPropagation();
        updateConnector()

        return false
    }
    let selectRow = {
        mode: 'radio',
        //clickToSelect: true,
        style: { backgroundColor: 'rgba(0, 151, 206, 0.3)' },
        selected: [rememberedSelection],
        onSelect: (row, isSelect, rowIndex, e) => {

            // setSDRef.current(row["id"])
            appDispatch(setSelectedConnectorDefault(row["id"]))
        },
    };
    let onEdit = id => {
        return e => {
            // setSDRef.current(id)
            appDispatch(setSelectedConnectorDefault(id))
        }
    }
    let onDelete = id => {
        return e => {
            setSelectedId(id)
            setShowdelete(true)

        }
    }
    let setConnector = () => {
        let connect = getConnector()

        if (connect == null)
            return ""
        let tunnels = connect.tunnels
        setName(connect.name)
        if (connect.accesskey != null)
            setKey(connect.accesskey)
        if (connect.secret != null)
            setSecret(connect.secret)
        setTunnel(connect.tunnels)
    }
    useEffect(() => {
        setConnector()
    }, [rememberedSelection])

    useEffect(() => {
        setConnector()
    }, [conns])
    const defaultSorted = [{
        dataField: 'name',
        order: 'asc'
    }];
    let columns = [
        {
            dataField: 'id',
            text: 'id',
            hidden: true,
            searchable: false
        },
        {
            dataField: 'name',
            text: 'Name:',
            sort: true,
        },
        {
            dataField: 'status',
            text: 'Status:',
            sort: true,
            formatter: (cell, row, rowIndex, formatExtraData) => {
                return <div>{tun.humanReadableConnectorStatus(row["status"])}</div>
            },
            headerStyle: { width: '200px' },
        },
        {
            text: 'Tunnels',
            dataField: 'tunnels',
            isDummyField: true,
            formatter: (cell, row, rowIndex, formatExtraData) => {
                return <div>{row["tunnels"].length}</div>
            },
            //formatExtraData: { hoverIdx: this.state.hoverIdx },
            headerStyle: { width: '100px' },
            style: { height: '30px' },
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

    let updateConnector = () => {
        let connector = getConnector()

        setSpinner(true)
        let jsbody = new tun.AddConnectorRequest()
        if (connector != null)
            jsbody.id = connector.id
        jsbody.name = name
        jsbody.accesskey = key
        jsbody.secret = secret
        jsbody.tunnels = tunnel

        let body = jsbody.toJson()

        http.sendToServer("POST", "/api/updateconnector",
            null, body,
            resp => {
                resp.json().then(js => {
                    if (js.status === "OK") {
                        setAlert(
                            <Alert variant="success" onClose={() => setAlert(<></>)} dismissible>
                                Connector {name} updated successfully! <br/><b>Note</b> that the <b>connector must be restarted</b> to update its configuration.
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
                getConnectors()
            },
            resp => {
                console.log("on error")
                resp != null && resp.text().then(t =>
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Error updating connection {name}: {t}
                        </Alert>
                    )
                )
                setSpinner(false)
                setShowedit(false)
                getConnectors()
            },
            error => {
                console.log("on exception: " + error)
                setSpinner(false)
                setShowedit(false)
                getConnectors()
            })
    }

    let deleteConnector = () => {
        let body = {
            Id: selectedId,
        }
        if (selectedId === rememberedSelection) {
            appDispatch(setSelectedConnectorDefault(""))
        }
        setSpinner(true)
        http.sendToServer("POST", "/api/deleteconnector",
            null, JSON.stringify(body),
            resp => {
                resp.json().then(js => {
                    if (js.status === "OK") {
                        setAlert(
                            <Alert variant="success" onClose={() => setAlert(<></>)} dismissible>
                                Connection deleted successfully!
                            </Alert>
                        )
                    } else {
                        setAlert(
                            <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                                Error deleting connection: {ReactHtmlParser(js.errormessage)}
                            </Alert>
                        )
                    }
                    setSpinner(false)
                    setShowdelete(false)
                    getConnectors()
                }).catch(error => {
                    console.log("Error: " + error.message)
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Error deleting connection: {error.message}
                        </Alert>
                    )
                    setSpinner(false)
                    console.log("on exception")
                    setShowdelete(false)
                    getConnectors()
                })
            },
            resp => {
                console.log("on error")
                resp != null && resp.text().then(t =>
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Error deleting connection: {t}
                        </Alert>
                    ))
                setSpinner(false)
                setShowdelete(false)
                getConnectors()
            },
            error => {
                console.log("on exception: " + error)
                setAlert(
                    <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        Error deleting connection: {error.message}
                    </Alert>
                )
                setSpinner(false)
                setShowdelete(false)
                getConnectors()
            })
    }
    let getConnectors = () => {
        setSpinner(true)
        http.sendToServer("GET", "/api/getconnectors",
            null, "",
            resp => {
                resp.json().then(js => {
                    setSpinner(false)

                    if (js.errormessage !== undefined) {
                        setAlert(
                            <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                                {js.errormessage}
                            </Alert>
                        )
                        return
                    }
                    setConns(js)
                    if (js.length === 0) {
                        setAlert(
                            <Alert variant="warning" onClose={() => setAlert(<></>)} dismissible>
                                Please configure some connectors first!
                            </Alert>
                        )
                    }
                }).catch((error) => {
                    setSpinner(false)
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Invalid server response.
                        </Alert>
                    )
                })
            },
            resp => {
                setSpinner(false)
                resp != null && resp.text().then(t =>
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Error retrieving access key: {t}
                        </Alert>
                    ))

            },
            error => {
                console.log("on exception")
                setSpinner(false)
                setAlert(
                    <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        Error retrieving access key: {error.message}
                    </Alert>
                )
            })
    }
    useEffect(() => {
        getConnectors()
    }, [])
    let connectionName = () => {
        let ret = ""
        conns.forEach(c => {

            if (c['id'] === selectedId)
                ret = c['name']
        })
        return ret
    }
    let getConnector = () => {
        for (let i = 0; i < conns.length; i++) {
            if (conns[i].id === rememberedSelection) {
                return conns[i]
            }
        }
        return null
    }
    let displayConnector = () => {

        return (
            <div className="my-5 p-3" style={{ opacity: '1', border: '1px solid rgb(220,220,240)', backgroundColor: '#FFFFFF' }}>
                <Form onSubmit={handleSubmit} ref={form} noValidate validated={validated}>
                    <ConnectionForm

                        name={name}
                        setName={setName}
                        accesskey={key}
                        setKey={setKey}
                        secret={secret}
                        setSecret={setSecret}
                        setSpinner={setSpinner}

                        tunnel={tunnel}
                        setTunnel={setTunnel}
                        id={rememberedSelection}
                        context="edit"
                    />

                    <Button variant="dymium" size="sm" className="mt-4" type="submit">
                        Apply
                    </Button>
                </Form>
            </div >
        )
    }

    const t = useAppSelector((state) => {

        return state.reducer.activeConnectorTab
    }
    )
    const refb = useRef<HTMLDivElement>(null);

    const history = useNavigate();
    useEffect(() => {

        if (location.hash === '#bookmark') {
            window.history.replaceState("", "Edit Connectors", '/app/connectors');
            setTimeout(() => {
                if (refb.current != null)
                    refb.current.scrollIntoView({ behavior: "smooth", block: "end" });

            }, 300)
        }
    }, [t])

    return (

        <div className=" text-left">
            <Offcanvas modal={false} width={300} show={showOffcanvas} onClose={(e) => { setShowOffcanvas(false) }}>
                <>
                    <h5>Connectors</h5>
                    <div className="mb-3">
                        If you just created a new connector, please save the parameters. The Secret will only be visible in the GUI for a day. 
                    </div>
                    <div className="mb-3">
                        Click on the Downloads tab to get a binary for your platform or a repository for a container. The configuration parameters must be passed to the
                        connector executable via environment variables. 
                    </div>
                    <ContainerHelp />
                </>
            </Offcanvas>

            <Modal  size="lg" centered show={showdelete} onHide={() => setShowdelete(false)} data-testid="modal-delete">
                <Modal.Header closeButton>
                    <Modal.Title>Deprovision connector {connectionName()}?</Modal.Title>
                </Modal.Header>
                <Modal.Body>Are you sure you want to deprovision the connector? This operation is irreversible.</Modal.Body>
                <Modal.Footer>
                    <Button variant="danger" role="button" id="Delete" data-testid="Delete"
                        aria-label={"Delete"}
                        onClick={() => {
                            deleteConnector()
                        }
                        }>Delete</Button> <Button variant="dymium" onClick={() => {
                            setShowdelete(false)
                        }}>Cancel</Button>
                </Modal.Footer>
            </Modal>
            {conns.length == 0 && alert}
            {conns.length > 0 &&
                <div id="tablecontainer" style={{ width: '90%' }} className="text-center">
                    <ToolkitProvider
                        bootstrap4
                        condensed
                        keyField='id'
                        data={conns}
                        columns={columns}


                        search >
                        {
                            props => (
                                <div className="text-left">
                                    {alert}
                                    <div className="d-flex">
                                        <h5 >Review And Manage Connectors  <i onClick={e => { setShowOffcanvas(!showOffcanvas) }} className="trash fa-solid fa-circle-info"></i><Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>


                                        <div style={{ marginLeft: "auto" }}>
                                            <SearchBar size="sm" {...props.searchProps} />
                                            <ClearSearchButton {...props.searchProps} />
                                            <i onClick={e => getConnectors()} className="fa fa-refresh ablue cursor-pointer" style={{ position: 'relative', top: '2px' }} aria-hidden="true"></i>
                                        </div>
                                    </div>
                                    <div className="d-block">
                                        <BootstrapTable id="scaledtable"
                                            condensed
                                            defaultSorted={defaultSorted}
                                            keyField='id'
                                            selectRow={selectRow}
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
            {(location.hash === '#bookmark') && "fafafafa"}
            {
                rememberedSelection !== "" && displayConnector()
            }
            <div id="bookmark" ref={refb}></div>
        </div>
    )
}
function useQuery() {
    const { search } = useLocation();

    return React.useMemo(() => new URLSearchParams(search), [search]);
}
function Connectors() {
    const navigate = useNavigate();
    let t = useAppSelector((state) => {

        return state.reducer.activeConnectorTab
    }
    )

    const appDispatch = useAppDispatch()
    let query = useQuery();

    let tt = query.get("key")
    if (tt !== null) {
        t = tt
    }
    if (t == null) {
        t = "add"
    }
    useEffect(() => {
        if (location.pathname === '/app/connectors/redirect#bookmark') {
            navigate('/app/connectors#bookmark')
        }
        if (query.get("key") != null) {

            appDispatch(setActiveConnectorTab(query.get("key")))
            navigate("/app/connectors")
        }
    }, [t])



    return (
        <Tabs activeKey={t} id="connectors"
            onSelect={(k) => appDispatch(setActiveConnectorTab(k))}

            unmountOnExit={true} className="mb-3 text-left">
            <Tab eventKey="add" title="Add" className="mx-4">
                <AddConnector />
            </Tab>
            <Tab eventKey="edit" title="Connectors" className="mx-4">
                <EditConnectors />
            </Tab>
            <Tab eventKey="download" title="Downloads" className="mx-4">
                <Downloads />
            </Tab>
        </Tabs>
    )
}

export default Connectors;