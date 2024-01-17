import React, { useEffect, useState, useRef, useCallback } from 'react';
import Tabs from 'react-bootstrap/Tabs'
import Tab from 'react-bootstrap/Tab'
import Alert from 'react-bootstrap/Alert'
import Form from 'react-bootstrap/Form'
import Button from 'react-bootstrap/Button'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Multiselect from 'multiselect-react-dropdown';
import Spinner from '@dymium/common/Components/Spinner'
import Offcanvas from '@dymium/common/Components/Offcanvas'
import { useLocation, useNavigate } from "react-router-dom";
import * as com from '../Common'
import * as types from '@dymium/common/Types/Internal'
import * as http from '@dymium/common/Api/Http'
import PasswordField from '@dymium/common/Components/PasswordField'
import { useAppDispatch, useAppSelector } from './hooks'
import { setActiveMachineTab, setSelectedTunnel } from '../Slices/menuSlice'
import BootstrapTable from 'react-bootstrap-table-next';
import paginationFactory from 'react-bootstrap-table2-paginator';
import ToolkitProvider, { Search } from 'react-bootstrap-table2-toolkit';
import Modal from 'react-bootstrap/Modal'

const { SearchBar, ClearSearchButton } = Search;

function MachineTunnelHelp() {
    const [registryid, setRegistryid] = useState("t0k4e6u4")
    useEffect
    (() => {
        http.sendToServer("GET", "/api/getregistryid",
            null, "",
            resp => {
                resp.json().then(js => {
                    setRegistryid(js.id)
                })
            },
            resp => {
            },
            error => {
            })
    }, [])


    return <>

        <div className="mb-3">
            To establish a connection between your database and the Dymium Machine Tunnel Docker container,
            you need map the internal listener port 5432 to the host listener port that will be used by your application.
            <br />
            Steps for Configuration:
            <br />
            Set Environment Variables:
            <br />
            Define the necessary environment variables inside the Docker container.
            These include configurations for log level, portal URL, key, secret, customer name, tunnel server, and the local listener port.
        </div>
        <div className="mb-3">
            <div className="d-block " style={{ fontFamily: "monospace", fontSize: "0.6em", overflow: "hidden", color: '#33cc33', backgroundColor: 'black' }}>
                <div className=" text-nowrap p-1" >
                    <div >
                        docker run \
                    </div><div>
                        -e LOG_LEVEL=Info \
                    </div><div>
                        -e PORTAL=https://portal.dymium.io/ \
                    </div><div>
                        -e KEY=&lt;your_secret_key&gt; \
                    </div><div>
                        -e SECRET=&lt;your_secret&gt; \
                    </div><div>
                        -e CUSTOMER=spoofcorp \
                    </div><div>
                        -e TUNNELSERVER=spoofcorp.dymium.io \
                    </div><div>
                        -p &lt;LOCAL_LISTENER&gt;:5432 \
                    </div><div>
                        public.ecr.aws/{registryid}/dymiummachinetunnel:latest
                    </div>

                </div>
            </div>



        </div>
    </>
}

function AddMachineTunnel() {
    const [spinner, setSpinner] = useState(false)
    const [alert, setAlert] = useState<JSX.Element>(<></>)
    const [showOffcanvas, setShowOffcanvas] = useState(com.isInstaller())
    const [validated, setValidated] = useState(false)
    const [groups, setGroups] = useState<types.Group[]>([])
    const [selectedgroups, setSelectedgroups] = useState<[]>([])
    const [name, setName] = useState("")
    let redstyle = { chips: { background: "rgb(0, 151,206)" }, searchBox: { border: '1px solid red' } }
    let normalstyle = { chips: { background: "rgb(0, 151,206)" } }
    const [multistyle, setMultistyle] = useState(normalstyle)
    const appDispatch = useAppDispatch()
    let form = useRef<HTMLFormElement>(null)
    const navigate = useNavigate();

    let getGroups = () => {
        http.sendToServer("GET", "/api/getmappings",
            null, "",
            resp => {
                resp.json().then(js => {
                    let grps: types.Group[] = []

                    js.records.forEach(x => {
                        for (let i = 0; i < grps.length; i++) {
                            if (x.dymiumgroup === grps[i].name) {
                                return
                            }
                        }
                        grps.push({ id: x.id, name: x.dymiumgroup })
                    })
                    setGroups(grps)
                }).catch((error) => {
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Error retrieving mapping.
                        </Alert>
                    )
                    setSpinner(false)
                })
            },
            resp => {
                setSpinner(false)
                resp != null && resp.text().then(t =>
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Error retrieving mapping: {t}
                        </Alert>
                    ))
                setSpinner(false)
            },
            error => {
                console.log("on exception")
                setSpinner(false)
                setAlert(
                    <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        Error retrieving mapping.
                    </Alert>
                )
                setSpinner(false)
            })
    }
    useEffect(() => {
        getGroups()
    }, [])
    const sendNewMachineTunnel = () => {
        let js = {
            name,
            groups: selectedgroups.map(x => x["id"])
        }
        let body = JSON.stringify(js)
        setSpinner(true)
        http.sendToServer("POST", "/api/addmachinetunnel",
            null, body,
            resp => {
                resp.json().then(js => {
                    setAlert(
                        <Alert variant="success" onClose={() => setAlert(<></>)} dismissible>
                            Machine tunnel created successfully.
                        </Alert>
                    )
                   
                    setTimeout(() => {
                        appDispatch(setSelectedTunnel(js.id))
                        appDispatch(setActiveMachineTab("edit"))
                        setSpinner(false)
                        navigate("/app/machineaccess/redirect#bookmark")
                    }, 1000)
                }).catch((error) => {
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Error adding tunnel {error}.
                        </Alert>
                    )
                    setSpinner(false)
                })
            },
            resp => {
                setSpinner(false)
                resp != null && resp.text().then(t =>
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Error retrieving mapping: {t}
                        </Alert>
                    ))
                setSpinner(false)
            },
            error => {
                console.log("on exception")
                setSpinner(false)
                setAlert(
                    <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        Error retrieving mapping.
                    </Alert>
                )
                setSpinner(false)
            })
    }
    let handleSubmit = event => {
        if (form.current == null) {
            return false
        }
        if (selectedgroups.length === 0) {
            setMultistyle(redstyle)
            let i: HTMLInputElement = document.getElementById("multiselect_input") as HTMLInputElement
            if (selectedgroups.length === 0) {
                i.setCustomValidity("Please select some groups")
            }
        }
        if (form.current.reportValidity() === false) {
            event.preventDefault();
            setValidated(true)
            return false
        }
        event.preventDefault();
        setValidated(false)
        sendNewMachineTunnel()
        return false
    }

    let onSelect = (selectedList, selectedItem) => {
        setSelectedgroups(selectedList)
        setMultistyle(normalstyle)
        let i: HTMLInputElement = document.getElementById("multiselect_input") as HTMLInputElement
        if (i != null) {
            i.setCustomValidity("")
        }
    }

    let onRemove = (selectedList, removedItem) => {
        setSelectedgroups(selectedList)
    }
    return <div className=" text-left">
        {alert}
        <h5 > Provision New Machine Tunnel <i onClick={e => { setShowOffcanvas(!showOffcanvas) }} className="trash fa-solid fa-circle-info"></i><Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>
        <Offcanvas modal={false} width={300} show={showOffcanvas} onClose={(e) => { setShowOffcanvas(false) }}>
            <h5>Provisioning New Machine Tunnel</h5>
            <div className="mb-3">
                Dymium offers a tunneling technology solution that connects a machine to the Dymium service.
            </div>

            <div className="mb-3">
                Create a new tunnel by specifying a unique name, and groups that will define the Ghost Database accessible through the tunnel.
            </div>
            <div>

            </div>
        </Offcanvas>
        <div className=" text-left">
            <Form onSubmit={handleSubmit} ref={form} noValidate validated={validated}>
                <Row>
                    <Col xs="auto">
                        <Form.Group className="mb-3" controlId="name">
                            <Form.Label>Machine Tunnel Name:</Form.Label>
                            <Form.Control style={{ width: '25em' }} required type="text"
                                value={name}
                                pattern="^\S(.*\S)?$"
                                onChange={e => setName(e.target.value)}
                                placeholder="Human readable name" />
                            <Form.Control.Feedback type="invalid">
                                Please provide a valid machine tunnel name.
                            </Form.Control.Feedback>
                        </Form.Group>
                    </Col>
                    <Col></Col>
                </Row>
                <Row>
                    <Col xs="auto">
                        <Form.Group className="mb-3" controlId="groups">
                            <Form.Label>Groups:</Form.Label>
                            <Multiselect
                                id="multiselect"
                                selectedValues={selectedgroups}
                                options={groups}
                                onSelect={onSelect} // Function will trigger on select event
                                onRemove={onRemove}
                                displayValue="name"
                                closeOnSelect={true}
                                showArrow={true}
                                isObject={true}
                                avoidHighlightFirstOption={true}
                                style={multistyle}
                            />
                            <Form.Control.Feedback type="invalid">
                                Please provide a valid group name.
                            </Form.Control.Feedback>
                        </Form.Group>
                    </Col>
                    <Col></Col>
                </Row>
                <Button variant="dymium" size="sm" className="mt-4" type="submit">
                    Apply
                </Button>
            </Form>
        </div>
    </div>
}

function EditMachineTunnels() {
    const [spinner, setSpinner] = useState(false)
    const [alert, setAlert] = useState<JSX.Element>(<></>)
    const [showOffcanvas, setShowOffcanvas] = useState(com.isInstaller())
    const [validated, setValidated] = useState(false)
    const [groups, setGroups] = useState<types.Group[]>([])
    const [selectedgroups, setSelectedgroups] = useState<any[]>([])
    const [sizePerPage, setSizePerPage] = useState(10);
    const [totalSize, setTotalSize] = useState(0);
    const [name, setName] = useState("")
    const [data, setData] = useState([])
    const [page, setPage] = useState(1);
    const [todelete, setToDelete] = useState("")
    const [accesskey, setAccesskey] = useState("")
    const [accesssecret, setAccesssecret] = useState("")
    const [, setSortField] = useState('createdat'); // Default sort column
    const [, setSortOrder] = useState('asc'); // Default sort order
    const [, setSearchText] = useState('');

    const [username, setUsername] = useState("")
    const [password, setPassword] = useState("")

    const [showOffhelp, setShowOffhelp] = useState(false)

    let redstyle = { chips: { background: "rgb(0, 151,206)" }, searchBox: { border: '1px solid red' } }
    let normalstyle = { chips: { background: "rgb(0, 151,206)" } }
    const [multistyle, setMultistyle] = useState(normalstyle)
    const appDispatch = useAppDispatch()

    let rememberedSelection = useAppSelector((state) => {
        return state.reducer.selectedTunnel
    }
    )
    let selectionRef = useRef(rememberedSelection)
    selectionRef.current = rememberedSelection

    let form = useRef<HTMLFormElement>(null)
    let onEdit = useCallback(id => {
        return e => {

            if (selectionRef.current === id) {
                appDispatch(setSelectedTunnel(""))
            } else {
                appDispatch(setSelectedTunnel(id))
            }
        }
    }, [rememberedSelection])
    let onDelete = id => {
        return e => {

            setToDelete(id)
        }
    }
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
            text: 'Access Key',
            dataField: 'accesskey',
            headerStyle: { width: '100px' },
            style: { height: '30px' },
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
    let getGroups = () => {
        http.sendToServer("GET", "/api/getmappings",
            null, "",
            resp => {
                resp.json().then(js => {
                    let grps: types.Group[] = []

                    js.records.forEach(x => {
                        for (let i = 0; i < grps.length; i++) {
                            if (x.dymiumgroup === grps[i].name) {
                                return
                            }
                        }
                        grps.push({ id: x.id, name: x.dymiumgroup })
                    })
                    setGroups(grps)
                }).catch((error) => {
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Error retrieving mapping.
                        </Alert>
                    )
                    setSpinner(false)
                })
            },
            resp => {
                setSpinner(false)
                resp != null && resp.text().then(t =>
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Error retrieving mapping: {t}
                        </Alert>
                    ))
                setSpinner(false)
            },
            error => {
                console.log("on exception")
                setSpinner(false)
                setAlert(
                    <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        Error retrieving mapping.
                    </Alert>
                )
                setSpinner(false)
            })
    }
    const loadTunnels = () => {
        http.sendToServer("GET", "/api/getmachinetunnels",
            null, "",
            resp => {
                resp.json().then(js => {
                    setData(js)
                }).catch((error) => {
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Error retrieving tunnels.
                        </Alert>
                    )
                    setSpinner(false)
                })
            },
            resp => {
                setSpinner(false)
                resp != null && resp.text().then(t =>
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Error retrieving tunnels: {t}
                        </Alert>
                    ))
                setSpinner(false)
            },
            error => {
                console.log("on exception")
                setSpinner(false)
                setAlert(
                    <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        Error retrieving tunnels.
                    </Alert>
                )
                setSpinner(false)
            })
    }
    useEffect(() => {
        loadTunnels()
        getGroups()
    }, [])
    const updateMachineTunnel = () => {
        let js = {
            id: rememberedSelection,
            name,
            groups: selectedgroups.map(x => x["id"])
        }
        let body = JSON.stringify(js)
        setSpinner(true)
        http.sendToServer("POST", "/api/updatemachinetunnel",
            null, body,
            resp => {
                resp.json().then(js => {

                    setSpinner(false)
                    setAlert(
                        <Alert variant="success" onClose={() => setAlert(<></>)} dismissible>
                            Tunnel settings updated.
                        </Alert>
                    )
                    loadTunnels()
                }).catch((error) => {
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Error adding tunnel {error}.
                        </Alert>
                    )
                    setSpinner(false)
                })
            },
            resp => {
                setSpinner(false)
                resp != null && resp.text().then(t =>
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Error retrieving mapping: {t}
                        </Alert>
                    ))
                setSpinner(false)
            },
            error => {
                console.log("on exception")
                setSpinner(false)
                setAlert(
                    <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        Error retrieving mapping.
                    </Alert>
                )
                setSpinner(false)
            })
    }
    const deleteMachineTunnel = (todelete) => {
        let js = {
            id: todelete,
        }
        let body = JSON.stringify(js)
        setSpinner(true)
        http.sendToServer("POST", "/api/deletemachinetunnel",
            null, body,
            resp => {
                resp.json().then(js => {
                    setSpinner(false)
                    loadTunnels()
                    setToDelete("")
                    if (todelete === rememberedSelection) {
                        appDispatch(setSelectedTunnel(""))
                    }
                }).catch((error) => {
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Error deleting tunnel {error}.
                        </Alert>
                    )
                    setSpinner(false)
                })
            },
            resp => {
                setSpinner(false)
                resp != null && resp.text().then(t =>
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Error deleting tunnel: {t}
                        </Alert>
                    ))
                setSpinner(false)
            },
            error => {
                console.log("on exception")
                setSpinner(false)
                setAlert(
                    <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        Error deleting tunnel.
                    </Alert>
                )
                setSpinner(false)
            })
    }
    const calculateSelectedGroups = (groupids) => {
        let grps: any[] = []
        groupids.forEach(g => {
            let t = groups.find(x => x["id"] === g)
            if (t !== undefined) {
                grps.push(t)
            }
        })
        return grps
    }
    let setSelection = (rememberedSelection) => {
        if (rememberedSelection !== "") {
            let t = data.find(x => x["id"] === rememberedSelection)
            if (t !== undefined) {
                setName(t["name"])
                setSelectedgroups(calculateSelectedGroups(t["groups"]))
                setAccesskey(t["accesskey"])
                setAccesssecret(t["secret"])
                setUsername(t["username"])
                setPassword(t["password"])
            }
        }
    }
    useEffect(() => {
        setSelection(rememberedSelection)
    }, [rememberedSelection, data])
    const defaultSorted = [{
        dataField: 'name',
        order: 'asc'
    }];
    let selectRow = {
        mode: 'radio',
        //clickToSelect: true,
        style: { backgroundColor: 'rgba(0, 151, 206, 0.3)' },
        selected: [rememberedSelection],
        onSelect: (row, isSelect, rowIndex, e) => {

            if (rememberedSelection === row["id"]) {
                appDispatch(setSelectedTunnel(""))
            } else {
                appDispatch(setSelectedTunnel(row["id"]))
            }
        },
    };
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
        updateMachineTunnel()

        return false
    }
    let onSelect = (selectedList, selectedItem) => {
        setSelectedgroups(selectedList)
        setMultistyle(normalstyle)
        let i: HTMLInputElement = document.getElementById("multiselect_input") as HTMLInputElement
        if (i != null) {
            i.setCustomValidity("")
        }
    }

    let onRemove = (selectedList, removedItem) => {
        setSelectedgroups(selectedList)
    }
    const handleTableChange = (type, { page, sizePerPage, sortField, sortOrder, searchText }) => {
        setPage(page);
        setSizePerPage(sizePerPage);
        if (sortField !== undefined)
            setSortField(sortField);
        if (sortOrder !== undefined)
            setSortOrder(sortOrder);
        setSearchText(searchText || "");
    };
    const copysecret = e => {
        navigator.clipboard.writeText(accesssecret);
    }
    const copykey = e => {
        navigator.clipboard.writeText(accesskey);
    }
    const copypass  = e => {
        navigator.clipboard.writeText(password);
    }
    const nameFromId = (id) => {
        let t = data.find(x => x["id"] === id)
        if (t !== undefined) {
            return t["name"]
        }
        return ""
    }
    const regenerate = () => {
        let js = {
            id: rememberedSelection,
        }
        let body = JSON.stringify(js)
        setSpinner(true)
        http.sendToServer("POST", "/api/regenmachinetunnel",
            null, body,
            resp => {
                resp.json().then(js => {
                    setSpinner(false)
                    loadTunnels()
                    setToDelete("")
                }).catch((error) => {
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Error regenerating password {error}.
                        </Alert>
                    )
                    setSpinner(false)
                })
            },
            resp => {
                setSpinner(false)
                resp != null && resp.text().then(t =>
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Error regenerating password: {t}
                        </Alert>
                    ))
                setSpinner(false)
            },
            error => {
                setSpinner(false)
                setAlert(
                    <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        Error regenerating password.
                    </Alert>
                )
                setSpinner(false)
            })
    }
    return <>
        <div id="tablecontainer" style={{ width: '100%' }} className="text-left">
            {alert}
            <Modal centered show={todelete !== ""} onHide={() => setToDelete("")} data-testid="modal-delete">
                <Modal.Header closeButton>
                    <Modal.Title>Delete tunnel?</Modal.Title>
                </Modal.Header>
                <Modal.Body>Are you sure you want to remove the tunnel {nameFromId(todelete)}? This operation is irreversible.</Modal.Body>
                <Modal.Footer>
                    <Button variant="danger" role="button" id="Delete" data-testid="Delete"
                        aria-label={"Delete"}
                        onClick={() => {
                            deleteMachineTunnel(todelete)
                        }
                        }>Delete</Button> <Button variant="dymium" onClick={() => {
                            setToDelete("")
                        }}>Cancel</Button>
                </Modal.Footer>
            </Modal>
            <Offcanvas modal={false} width={300} show={showOffcanvas} onClose={(e) => { setShowOffcanvas(false) }}>
                <h5>Launching a Machine Tunnel</h5>
                <div className="mb-3">
                    If you just created a new machine tunnel, please save the parameters. The Password will only be visible in the GUI for a day.
                </div>
                <div className="mb-3">
                    Click on the Downloads tab to get a binary for your platform or a repository for a container. The configuration parameters must be passed to the
                    tunnel executable via environment variables.
                </div>
                <MachineTunnelHelp />
            </Offcanvas>
            <ToolkitProvider
                bootstrap4
                condensed
                keyField='id'
                data={data}
                columns={columns}


                search >
                {
                    props => (
                        <div className="text-left">

                            <div className="d-flex">
                                <h5 >Review And Manage Machine Tunnels  <i onClick={e => { setShowOffcanvas(!showOffcanvas) }} className="trash fa-solid fa-circle-info"></i><Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>


                                <div style={{ marginLeft: "auto" }}>
                                    <SearchBar size="sm" {...props.searchProps} />
                                    <ClearSearchButton {...props.searchProps} />
                                    <i onClick={e => loadTunnels()} className="fa fa-refresh ablue cursor-pointer" style={{ position: 'relative', top: '2px' }} aria-hidden="true"></i>
                                </div>
                            </div>
                            <div className="d-block">
                                <BootstrapTable id="scaledtable"
                                    condensed
                                    defaultSorted={defaultSorted}
                                    keyField='id'
                                    selectRow={selectRow}
                                    striped bootstrap4 bordered={false}
                                    pagination={paginationFactory({ page, sizePerPage, totalSize })}
                                    {...props.baseProps}
                                    onTableChange={handleTableChange}
                                />
                            </div>
                        </div>
                    )
                }
            </ToolkitProvider>
            {rememberedSelection !== "" && <div className="text-left">
                <Form onSubmit={handleSubmit} ref={form} noValidate validated={validated}>
                    <hr />
                    <Row>
                        <Col xs="auto">
                            <Form.Group className="mb-3" controlId="name">
                                <Form.Label>Machine Tunnel Name:</Form.Label>
                                <Form.Control style={{ width: '25em' }} required type="text"
                                    value={name}
                                    size="sm"
                                    pattern="^\S(.*\S)?$"
                                    onChange={e => setName(e.target.value)}
                                    placeholder="Human readable name" />
                                <Form.Control.Feedback type="invalid">
                                    Please provide a valid machine tunnel name.
                                </Form.Control.Feedback>
                            </Form.Group>
                        </Col>

                        <Col xs="auto">

                            <Form.Group className="mb-3" controlId="name">
                                <Form.Label>Access Key:</Form.Label>
                                <span className="d-flex">
                                    <Form.Control style={{ width: '25em' }} required type="text"
                                        value={accesskey}
                                        size="sm"
                                    /><i onClick={copykey} style={{ marginTop: '1px' }} className="fas fa-copy clipbtn"></i>
                                </span>
                                <Form.Control.Feedback type="invalid">
                                    Please provide a valid access key.
                                </Form.Control.Feedback>
                            </Form.Group>

                        </Col>
                        <Col></Col>
                    </Row>
                    <Row>
                        <Col>

                            <Form.Group className="mb-3" controlId="name">
                                <Form.Label>Access Secret:</Form.Label>
                                <span className="d-flex">
                                    <PasswordField type="password"

                                        placeholder="DB password"
                                        pattern=".+"
                                        validfeedback="Looks good!"
                                        invalidfeedback="Admin password"
                                        value={accesssecret}
                                        className="w-40em"

                                        size="sm" /><i onClick={copysecret} style={{ marginTop: '1px' }} className="fas fa-copy clipbtn"></i>
                                </span>
                                <Form.Control.Feedback type="invalid">
                                    Please provide a valid access secret.
                                </Form.Control.Feedback>
                            </Form.Group>
                        </Col>
                    </Row>
                    <Row>
                        <Col xs="auto">

                            <Form.Group className="mb-3" controlId="name">
                                <Form.Label>Ghost Database Username:</Form.Label>
                                <span className="d-flex">
                                    <Form.Control style={{ width: '13em' }} required type="text"
                                        value={username}
                                        size="sm"
                                    /><i onClick={copykey} style={{ marginTop: '1px' }} className="fas fa-copy clipbtn"></i>
                                </span>
                            </Form.Group>
                        </Col>
                        <Col xs="auto">


                            <Form.Group className="mb-3" controlId="name">
                                <Form.Label>Ghost Database Password:</Form.Label>
                                {password === "**********" ? <div>{password}{password}{password}</div>
                                    :
                                    <span className="d-flex">
                                        <PasswordField type="password"

                                            placeholder="DB password"
                                            pattern=".+"
                                            validfeedback="Looks good!"
                                            invalidfeedback="Admin password"
                                            value={password}
                                            className="w-20em"

                                            size="sm" /><i onClick={copypass} style={{ marginTop: '1px' }} className="fas fa-copy clipbtn"></i>
                                    </span>
                                }
                            </Form.Group>
                        </Col>
                        <Col>
                            {password === "**********" &&
                                <Button onClick={regenerate} style={{ position: 'relative', top: '1.9em' }} variant="dymium" size="sm">Regenerate</Button>
                            }
                        </Col>
                    </Row>
                    <Row>
                        <Col xs="auto">
                            <Form.Group className="mb-3" controlId="groups">
                                <Form.Label>Groups:</Form.Label>
                                <Multiselect
                                    id="multiselect"
                                    selectedValues={selectedgroups}
                                    options={groups}
                                    onSelect={onSelect} // Function will trigger on select event
                                    onRemove={onRemove}
                                    displayValue="name"
                                    closeOnSelect={true}
                                    showArrow={true}
                                    isObject={true}
                                    avoidHighlightFirstOption={true}
                                    style={multistyle}
                                />
                                <Form.Control.Feedback type="invalid">
                                    Please provide a valid group name.
                                </Form.Control.Feedback>
                            </Form.Group>
                        </Col>
                        <Col></Col>
                    </Row>
                    <Button variant="dymium" size="sm" className="mt-4" type="submit">
                        Update
                    </Button>
                </Form>
                <div id="bookmark"></div>
            </div>}
        </div>
    </>
}

function MachineTunnelDownloads() {
    const [spinner, setSpinner] = useState(false)
    const [alert, setAlert] = useState<JSX.Element>(<></>)
    const [showOffcanvas, setShowOffcanvas] = useState(com.isInstaller())
    const [docker, setDocker] = useState("public.ecr.aws/t0k4e6u4/dymiummachinetunnel:latest")
    const [registryid, setRegistryid] = useState("t0k4e6u4")
    useEffect(() => {
        http.sendToServer("GET", "/api/getdockers",
            null, "",
            resp => {
                resp.json().then(js => {
                    setDocker(js.machineclient)
                })
            },
            resp => {
            },
            error => {
            })

            http.sendToServer("GET", "/api/getregistryid",
            null, "",
            resp => {
                resp.json().then(js => {
                    setRegistryid(js.id)
                    setDocker("public.ecr.aws/"+js.id+"/dymiummachinetunnel:latest")
                })
            },
            resp => {
            },
            error => {
            })

    }, [])

    let copydocker = e => {
        navigator.clipboard.writeText(docker);
    }
    return <div className=" text-left">
        {alert}
        <h5 > Machine Client Downloads And Use <i onClick={e => { setShowOffcanvas(!showOffcanvas) }} className="trash fa-solid fa-circle-info"></i><Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>
        <Offcanvas modal={false} width={300} show={showOffcanvas} onClose={(e) => { setShowOffcanvas(false) }}>
            <h5>Machine Client Downloads</h5>
            <div className="mb-3">
                Dymium offers a machine to machine tunneling client in the form of docker container.
            </div>
            <div>
                The container is distributed as a docker image, and can be pulled from the Dymium repository.
            </div>
            <div>
                Run is using Docker Desktop on all OSes, or WSL2 on Windows.
            </div>


        </Offcanvas>
        <div className="mb-3">
            Dymium offers a machine to machine tunneling client in the form of docker container.
        </div>

        <div className="viewport">
            <div>A Docker container:</div>
            <i className="fab fa-docker thickblue" style={{ marginRight: '0.3em' }} ></i>{docker}<i style={{ marginTop: '0.1em', marginLeft: '0.3em' }} onClick={copydocker} className="fas fa-copy clipbtn"></i>

        </div>
        <MachineTunnelHelp />
    </div>
}
function useQuery() {
    const { search } = useLocation();

    return React.useMemo(() => new URLSearchParams(search), [search]);
}
export default function MachineAccess() {
    const navigate = useNavigate();
    var t = useAppSelector((state) => {
        return state.reducer.activeMachineTab
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
        if (location.pathname === '/app/machineaccess/redirect') {
            navigate('/app/machineaccess')
        }
        if (query.get("key") != null) {
            navigate("/app/machineaccess")
        }
    }, [t])

    return (
        <Tabs
            activeKey={t} id="machinetunnels"
            onSelect={(k) => appDispatch(setActiveMachineTab(k))}

            unmountOnExit={true} className="mb-3 text-left">
            <Tab eventKey="add" title="Add Machine Tunnel" className="mx-4">
                <AddMachineTunnel />
            </Tab>
            <Tab eventKey="edit" title="Edit Machine Tunnels" className="mx-4">
                <EditMachineTunnels />
            </Tab>
            <Tab eventKey="client" title="Client Downloads and Use" className="mx-4">
                <MachineTunnelDownloads />
            </Tab>
        </Tabs>

    )
}