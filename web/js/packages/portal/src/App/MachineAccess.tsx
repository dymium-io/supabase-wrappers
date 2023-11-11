import React, { useEffect, useState, useRef, useCallback } from 'react';
import Tabs from 'react-bootstrap/Tabs'
import Tab from 'react-bootstrap/Tab'
import Alert from 'react-bootstrap/Alert'
import Form from 'react-bootstrap/Form'
import Button from 'react-bootstrap/Button'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Multiselect from 'multiselect-react-dropdown';
import { Link } from "react-router-dom";
import Spinner from '@dymium/common/Components/Spinner'
import Offcanvas from '@dymium/common/Components/Offcanvas'
import * as com from '../Common'
import * as types from '@dymium/common/Types/Internal'
import * as http from '@dymium/common/Api/Http'
import PasswordField from '@dymium/common/Components/PasswordField'
import { useAppDispatch, useAppSelector } from './hooks'
import { setActiveMachineTab, setSelectedTunnel } from '../Slices/menuSlice'
import BootstrapTable from 'react-bootstrap-table-next';
import paginationFactory from 'react-bootstrap-table2-paginator';
import ToolkitProvider, { Search } from 'react-bootstrap-table2-toolkit';
import { send } from 'process';

const { SearchBar, ClearSearchButton } = Search;

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

    let form = useRef<HTMLFormElement>(null)

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
                    let grps: types.Group[] = []
                    setSpinner(false)

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
                Create a new connector by specifying a unique name, and groups that will define the Ghost Database accessible through the tunnel.
            </div>
            <div>

            </div>
        </Offcanvas>
        <div className=" text-left">
            <Form onSubmit={handleSubmit} ref={form} noValidate validated={validated}>
                <Row>
                    <Col xs="auto">
                        <Form.Group className="mb-3" controlId="name">
                            <Form.Label>Machine Tunnel Name</Form.Label>
                            <Form.Control style={{ width: '25em' }} required type="text"
                                value={name}
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
                            <Form.Label>Groups</Form.Label>
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

    const [accesskey, setAccesskey] = useState("")
    const [accesssecret, setAccesssecret] = useState("")
    const [, setSortField] = useState('createdat'); // Default sort column
    const [, setSortOrder] = useState('asc'); // Default sort order
    const [, setSearchText] = useState('');

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
        //
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
    return <>
        <div id="tablecontainer" style={{ width: '100%' }} className="text-left">
        {alert}
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
                        <Col>
                            <Form.Group className="mb-3" controlId="name">
                                <Form.Label>Machine Tunnel Name</Form.Label>
                                <Form.Control style={{ width: '25em' }} required type="text"
                                    value={name}
                                    size="sm"
                                    onChange={e => setName(e.target.value)}
                                    placeholder="Human readable name" />
                                <Form.Control.Feedback type="invalid">
                                    Please provide a valid machine tunnel name.
                                </Form.Control.Feedback>
                            </Form.Group>
                        </Col>
                    </Row>
                    <Row>
                        <Col>

                            <Form.Group className="mb-3" controlId="name">
                                <Form.Label>Access Key</Form.Label>
                                <span className="d-flex">
                                    <Form.Control style={{ width: '25em' }} required type="text"
                                        value={accesskey}
                                        size="sm"
                                    /><i onClick={copykey} style={{ marginTop: '1px' }} className="fas fa-copy clipbtn"></i>
                                </span>
                                <Form.Control.Feedback type="invalid">
                                    Please provide a valid machine tunnel name.
                                </Form.Control.Feedback>
                            </Form.Group>

                        </Col>
                    </Row>
                    <Row>
                        <Col>

                            <Form.Group className="mb-3" controlId="name">
                                <Form.Label>Access Secret</Form.Label>
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
                                    Please provide a valid machine tunnel name.
                                </Form.Control.Feedback>
                            </Form.Group>
                        </Col>
                    </Row>
                    <Row>
                        <Col xs="auto">
                            <Form.Group className="mb-3" controlId="groups">
                                <Form.Label>Groups</Form.Label>
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
            </div>}
        </div>
    </>
}


export default function MachineAccess() {
    const t = useAppSelector((state) => {

        return state.reducer.activeMachineTab
    }
    )
    const appDispatch = useAppDispatch()

    return (
        <Tabs
            defaultActiveKey={t} id="machinetunnels"
            onSelect={(k) => appDispatch(setActiveMachineTab(k))}

            unmountOnExit={true} className="mb-3 text-left">
            <Tab eventKey="add" title="Add Machine Tunnel" className="mx-4">
                <AddMachineTunnel />
            </Tab>
            <Tab eventKey="edit" title="Edit Machine Tunnels" className="mx-4">
                <EditMachineTunnels />
            </Tab>
        </Tabs>

    )
}