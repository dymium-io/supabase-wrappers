import React, { useEffect, useState, useRef } from 'react';
import Modal from 'react-bootstrap/Modal'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Alert from 'react-bootstrap/Alert'
import Button from 'react-bootstrap/Button'
import Form from 'react-bootstrap/Form'
import Spinner from '@dymium/common/Components/Spinner'
import Offcanvas from '@dymium/common/Components/Offcanvas'
import cloneDeep from 'lodash/cloneDeep'
import BootstrapTable from 'react-bootstrap-table-next';
import Multiselect from 'multiselect-react-dropdown';
import paginationFactory from 'react-bootstrap-table2-paginator';
import ToolkitProvider, { Search } from 'react-bootstrap-table2-toolkit';
import 'react-bootstrap-table-next/dist/react-bootstrap-table2.min.css';

const { SearchBar, ClearSearchButton } = Search;

import * as com from '../Common'
import * as types from '@dymium/common/Types/Internal'
import * as http from '@dymium/common/Api/Http'
import { legacy_createStore } from '@reduxjs/toolkit';
import { getDisplayName } from 'react-bootstrap-typeahead/types/utils';

export default function AssignGroups() {
    const [spinner, setSpinner] = useState(false)
    const [datascopes, setDatascopes] = useState<types.DataScopeInfo[]>([])
    const [validated, setValidated] = useState(false)
    const [id, setId] = useState("")
    const [name, setName] = useState("")
    const [groups, setGroups] = useState<types.Group[]>([])
    const [selectedgroups, setSelectedgroups] = useState<[]>([])
    const [alert, setAlert] = useState<JSX.Element>(<></>)
    const [showOffhelp, setShowOffhelp] = useState(com.isInstaller())

    let form = useRef<HTMLFormElement>(null)
    const [show, setShow] = useState(false)
    let redstyle = { chips: { background: "rgb(0, 151,206)" }, searchBox: { border: '1px solid red' } }
    let normalstyle = { chips: { background: "rgb(0, 151,206)" } }
    const [multistyle, setMultistyle] = useState(normalstyle)

    let refspinner = useRef(spinner)
    refspinner.current = spinner

    let getGroups = (datascopes) => {
        http.sendToServer("GET", "/api/getmappings",
            null, "",
            resp => {
                resp.json().then(js => {
                    let grps: types.Group[] = []
                    
                    js.records.forEach(x => {
                        for(let i = 0; i < grps.length; i++) {
                            if(x.dymiumgroup === grps[i].name) {
                                return
                            }
                        }
                        grps.push({ id: x.id, name: x.dymiumgroup })
                    })
                    setGroups(grps)
                    getMappings(datascopes)
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

    let getMappings = (datascopes) => {
        http.sendToServer("GET", "/api/getgroupsfordatascopes",
            null, "",
            resp => {
                resp.json().then(js => {
                    let newds = cloneDeep(datascopes)

                    let putGroupIntoDatascope = (id, o) => {
                        newds.forEach(x => {
                            if (x.id === id) {
                                if (x.groups === undefined)
                                    x.groups = []
                                x.groups.push(o)
                            }
                        })
                    }
                    for (let i = 0; i < js.length; i++) {
                        let o = { id: js[i].groupid, name: js[i].groupname }
                        putGroupIntoDatascope(js[i].id, o)
                    }

                    setDatascopes(newds)
                    setTimeout(() => setSpinner(false), 500)

                }).catch((error) => {
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Error retrieving group assignments.
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
                            Error retrieving group assignments: {t}.
                        </Alert>
                    ))

            },
            error => {
                setAlert(
                    <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        Error retrieving group assignments.
                    </Alert>
                )
                setSpinner(false)
            })

    }
    let getSpinner = () => {
        return refspinner.current
    }

    let sendGroups = () => {
        setSpinner(true)
        let b: types.Group[] = []
        let name
        datascopes.forEach(x => {
            if (x.id !== id)
                return
            name = x.name
            if (x.groups !== undefined)
                b = x.groups
        })
        let body = {
            name,
            id,
            groups: b
        }
        http.sendToServer("POST", "/api/savegroups",
            null, JSON.stringify(body),
            resp => {
                resp.json().then(js => {

                    if (js.status !== "OK") {
                        setAlert(
                            <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                                Error: {js.errormessage}
                            </Alert>
                        )
                    } else {
                        setAlert(
                            <Alert variant="success" onClose={() => setAlert(<></>)} dismissible>
                                Group assignments saved.
                            </Alert>
                        )
                    }
                    setTimeout(() => setSpinner(false), 500)
                }).catch((error) => {
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Error saving group assignments.
                        </Alert>
                    )
                    setSpinner(false)
                })
            },
            resp => {
                resp != null && resp.text().then(t =>
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Error saving group assignments: {t}
                        </Alert>
                    ))
                setSpinner(false)
            },
            error => {
                setAlert(
                    <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        Error saving group assignments.
                    </Alert>
                )
                setSpinner(false)
            })
    }

    useEffect(() => {
        setSpinner(true)
        com.getDatascopes((x:boolean) => {}, setAlert, setDatascopes, (js) => {
            getGroups(js)
        })
    }, [])

    let onEdit = (id, name, groups) => {
        return e => {
            if(getSpinner()) {
                return false
            }
            if (groups === undefined)
                groups = []
            setSelectedgroups(groups)

            setShow(true)
            setId(id)
            setName(name)
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
            text: 'Ghost Database:',
            headerStyle: { width: '20em' },
            sort: true,
        },
        {
            dataField: 'groups',
            text: 'Groups:',

            formatter: (cell, row, rowIndex, formatExtraData) => {
                let g = row["groups"]
                if (g === undefined) {
                    return <div className="darkred cursor-pointer" onClick={onEdit(row["id"], row["name"], row["groups"])} >Please edit to assign groups and enable user access</div>
                }

                return <div>{g.map(x => x.name).join(", ")}</div>
            },
            sort: true
        },
        {
            text: 'Edit',
            dataField: 'edit',
            isDummyField: true,
            formatter: (cell, row, rowIndex, formatExtraData) => {

                return <i className="fas fa-edit ablue" onClick={onEdit(row["id"], row["name"], row["groups"])} role="button"></i>
            },
            //formatExtraData: { hoverIdx: this.state.hoverIdx },
            headerStyle: { width: '50px' },
            style: { height: '30px' },
            align: 'center'
        },
    ]
    let handleSubmit = event => {
        if (form.current == null) {
            return false
        }
        if (selectedgroups.length === 0) {
            setMultistyle(redstyle)
            let i: HTMLInputElement = document.getElementById("multiselect_input") as HTMLInputElement
            if (i != null) {
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
        event.stopPropagation();
        datascopes.forEach(x => {
            if (x.id === id) {
                x.groups = selectedgroups
            }
        })

        setDatascopes(cloneDeep(datascopes))

        sendGroups()
        setShow(false)
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
    return (
        <div className=" text-left">
             {alert}
            <h5 > Associate groups with Ghost Databases <i onClick={e => { setShowOffhelp(!showOffhelp) }} className="trash fa-solid fa-circle-info mr-1"></i><Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>
            <Offcanvas modal={false} width={300} show={showOffhelp} onClose={(e) => { setShowOffhelp(false) }}>
                <h5>Associate Groups</h5>
                <div className="mb-3">
                    This page allows to associate Dymium groups with the Ghost Databases. Until this association is established, the Ghost Database remains unaccessible.
                </div>
                <div className="mb-3">
                    Select a Ghost Database from the list.
                </div>
                <div className="mb-3">
                    Select one or more groups from the dropout. Hit Apply. The association is done!
                </div>


                <div>

                </div>
            </Offcanvas>

            <Modal centered size="lg" show={show} onHide={() => setShow(false)} >
                <Form onSubmit={handleSubmit} ref={form} noValidate validated={validated}>
                    <Modal.Header closeButton>
                        <Modal.Title>Edit Group Access For {name}</Modal.Title>
                    </Modal.Header>
                    <Modal.Body>
                        <Row>

                            <Col>
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
                            </Col>
                            <Col xs="auto">
                                <Button variant="dymium" type="submit" onClick={() => {

                                }
                                }>Apply</Button>
                                <Button className="mx-3" variant="dymium" onClick={() => setShow(false)}>Cancel</Button>
                            </Col>
                        </Row>
                    </Modal.Body>
                    <Modal.Footer>

                    </Modal.Footer>
                </Form>
            </Modal>
                                
            <BootstrapTable id="scaledtable"
                condensed
                
                striped bootstrap4 bordered={false}
                pagination={paginationFactory()}
                keyField='id'
                data={datascopes}
                columns={columns}
            />
        </div>
    )
}