import React, { useEffect, useState, useRef } from 'react';
import Modal from 'react-bootstrap/Modal'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Alert from 'react-bootstrap/Alert'
import Button from 'react-bootstrap/Button'
import Form from 'react-bootstrap/Form'
import Spinner from '@dymium/common/Components/Spinner'
import cloneDeep from 'lodash/cloneDeep'
import BootstrapTable from 'react-bootstrap-table-next';
import Multiselect from 'multiselect-react-dropdown';
import paginationFactory from 'react-bootstrap-table2-paginator';
import ToolkitProvider, { Search } from 'react-bootstrap-table2-toolkit/dist/react-bootstrap-table2-toolkit';
import 'react-bootstrap-table-next/dist/react-bootstrap-table2.min.css';

const { SearchBar, ClearSearchButton } = Search;

import * as com from '../Common'
import * as types from '@dymium/common/Types/Internal'
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

    let form = useRef<HTMLFormElement>(null)
    const [show, setShow] = useState(false)
    let redstyle = { chips: { background: "rgb(0, 151,206)" }, searchBox: { border: '1px solid red' } }
    let normalstyle = { chips: { background: "rgb(0, 151,206)" } }
    const [multistyle, setMultistyle] = useState(normalstyle)

    let getDatascopes = () => {
        setSpinner(true)
        com.sendToServer("GET", "/api/getdatascopes",
            null, "",
            resp => {
                resp.json().then(js => {
                    setDatascopes(js)
                    getGroups(js)
                })
            },
            resp => {
                setAlert(
                    <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        Error retrieving datascopes.
                    </Alert>
                )
                setSpinner(false)
            },
            error => {
                setAlert(
                    <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        Error retrieving datascopes.
                    </Alert>
                )
                setSpinner(false)
            })
    }
    let getGroups = (datascopes) => {
        com.sendToServer("GET", "/api/getmappings",
            null, "",
            resp => {
                resp.json().then(js => {
                    let grps: types.Group[] = []
                    js.forEach(x => {
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
                setAlert(
                    <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        Error retrieving mapping.
                    </Alert>
                )
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
        com.sendToServer("GET", "/api/getgroupsfordatascopes",
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
                setAlert(
                    <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        Error retrieving group assignments.
                    </Alert>
                )
                setSpinner(false)
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
        com.sendToServer("POST", "/api/savegroups",
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
                setAlert(
                    <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        Error saving group assignments.
                    </Alert>
                )
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
        getDatascopes()
    }, [])

    let onEdit = (id, name, groups) => {
        return e => {
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
        },
        {
            dataField: 'name',
            text: 'Datascope:',
            headerStyle: { width: '20em' },
            sort: true,
        },
        {
            dataField: 'groups',
            text: 'Groups:',

            formatter: (cell, row, rowIndex, formatExtraData) => {
                let g = row["groups"]
                if (g === undefined) {
                    return <div></div>
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
            //console.log("Form validity false!")
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
            <h5 > Associate groups with Data Scopes <Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>
            {alert}
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