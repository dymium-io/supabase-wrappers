import React, { useEffect, useState, useRef } from 'react';
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
import { useAppDispatch, useAppSelector } from './hooks'
import { setActiveMachineTab } from '../Slices/menuSlice'
import { send } from 'process';


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
    }   , [])
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

        </Tabs>

    )
}