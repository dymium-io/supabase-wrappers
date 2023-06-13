import React, { useEffect, useState, useRef, useCallback } from 'react';
import { Navigate } from "react-router-dom";
import Form from 'react-bootstrap/Form'
import Button from 'react-bootstrap/Button'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Tabs from 'react-bootstrap/Tabs'
import Tab from 'react-bootstrap/Tab'
import Modal from 'react-bootstrap/Modal'
import Spinner from '@dymium/common/Components/Spinner'
import Alert from 'react-bootstrap/Alert'
import BootstrapTable from 'react-bootstrap-table-next';
import paginationFactory from 'react-bootstrap-table2-paginator';
import ToolkitProvider, { Search } from 'react-bootstrap-table2-toolkit';
import { useAppDispatch, useAppSelector } from './hooks'
import { setActiveCustomersTab, setActiveCustomer } from '../Slices/menuSlice'

import 'react-bootstrap-table-next/dist/react-bootstrap-table2.min.css';


import * as http from '@dymium/common/Api/Http'
import * as types from '@dymium/common/Types/Common'
import * as admin from '@dymium/common/Types/Admin'
const { SearchBar, ClearSearchButton } = Search;

function CustomerForm(props) {
    return <Form onSubmit={props.handleSubmit} ref={props.form} noValidate validated={props.validated}>
        <Row><Col>
            <Row>
                <Col xs="auto">
                    <Form.Group className="mb-3" controlId="ccname">
                        <Form.Label>Customer name:</Form.Label>
                        <Form.Control size="sm" type="text"
                            required placeholder="anything goes"
                            style={{ width: '40em' }}
                            pattern=".+"
                            disabled={props.edit === true}
                            value={props.name}
                            onChange={e => {
                                props.setName(e.target.value)
                            }}
                        />
                        <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                        <Form.Control.Feedback type="invalid" >
                            Enter customer name
                        </Form.Control.Feedback>
                    </Form.Group>
                </Col>
            </Row>
            <Row>
                <Col xs="auto">
                    <Form.Group className="mb-3" controlId="schemaid" >
                        <Form.Label>Schema name:</Form.Label>
                        <Form.Control size="sm" type="text"
                            required placeholder="small cap alphanum"
                            style={{ width: '40em' }}
                            pattern="[a-zA-Z0-9_]+"
                            value={props.schema}
                            disabled={props.edit === true}
                            onChange={e => {
                                props.setSchema(e.target.value)
                            }}
                        />
                        <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                        <Form.Control.Feedback type="invalid" >
                            Enter schema
                        </Form.Control.Feedback>
                    </Form.Group>
                </Col>
            </Row>
            <Row>
                <Col xs="auto">
                    <Form.Group className="mb-3" controlId="orgid">
                        <Form.Label>Auth0 ord_id:</Form.Label>
                        <Form.Control size="sm" type="text"
                            required placeholder="small cap alphanum"
                            style={{ width: '40em' }}
                            pattern="[-a-zA-Z0-9_]+"
                            value={props.orgid}
                            onChange={e => {
                                props.setOrgid(e.target.value)
                            }}
                        />
                        <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                        <Form.Control.Feedback type="invalid" >
                            Enter orgid
                        </Form.Control.Feedback>
                    </Form.Group>
                </Col>
            </Row>
            <Row>
                <Col xs="auto">
                    <Form.Group className="mb-3" controlId="domainid">
                        <Form.Label>Domain:</Form.Label>
                        <Form.Control size="sm" type="text"
                            required placeholder="small cap alphanum"
                            style={{ width: '40em' }}
                            pattern="[a-z0-9_.]+"
                            value={props.domain}
                            onChange={e => {
                                props.setDomain(e.target.value)
                            }}
                        />
                        <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                        <Form.Control.Feedback type="invalid" >
                            Enter domain
                        </Form.Control.Feedback>
                    </Form.Group>
                </Col>
            </Row>
            <Row>
                <Col xs="auto">
                    <Form.Group className="mb-3" controlId="domainid">
                        <Form.Label>Fallback Admin Group:</Form.Label>
                        <Form.Control size="sm" type="text"
                            required placeholder=""
                            style={{ width: '40em' }}
                            pattern=".+"
                            value={props.group}
                            onChange={e => {
                                props.setGroup(e.target.value)
                            }}
                        />
                        <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                        <Form.Control.Feedback type="invalid" >
                            Enter fallback admin group
                        </Form.Control.Feedback>
                    </Form.Group>
                </Col>
            </Row>
        </Col>
        <Col>
                            <h5>Onboarding a customer</h5>
            <div>
                This form only creates or updates a global database record for a customer, as well as a dedicated database schema.
            </div>
            <div className="mt-3">
                The following steps must be taken in addition:
                <ul>
                    <li>
                        <a href="https://docs.google.com/document/d/1LveztkbSUWUK6CFm08Nrsm5L7upOp7BsGz2NA3DT1kw/edit?usp=sharing" target="terraform">Terraform script executed</a> to bring up all the necessary microservices.
                    </li>
                    <li>
                        <a href="https://docs.google.com/document/d/1yJWNfuAovcdQCAeSm-sThp8MQt5sm3AlK_1TZXqghPM/edit?usp=sharing" target="route53">Hostnames added</a> to Amazons Route53 in the dymium subaccount
                    </li>
                    <li>
                       Auth0 configured either for  <a href="https://docs.google.com/document/d/1CYW7AFfZByBNDZdhWpAJcLD6uNtHfJuaFuXEQaMsl0o/edit?usp=sharing" target="okta">Okta</a> or <a href="https://docs.google.com/document/d/1U1YZ1xm5n89ch0oQKYhiqIAxzlPUmzlIw-uPlU4BlQM/edit?usp=sharing" target="azure">AzureAD</a> integration
                    </li>
                    <li>
                      Group mapping set up for access control in the customer's portal. Unless there is a reason to change group names, just use 1:1 mapping
                    </li>
                    <li>
                      Connectivity to the data sources provisioned either via <a href="" target="route53">PrivateLink </a> or <a href="https://docs.google.com/document/d/1U1YZ1xm5n89ch0oQKYhiqIAxzlPUmzlIw-uPlU4BlQM/edit?usp=sharing" target="route53">Dymium Connector</a>  in the customer's portal.
                      Dymium Connector is easier to set up, and can be used with on-prem installations, as well as less supported clouds.
                    </li>
                    <li>
                        Data Sources created in the customer's portal.
                    </li>      
                    <li>
                       Access levels and Rules defined in the customer's portal.
                    </li>                      
                    <li>
                        Ghost Databases defined in the customer's portal.
                    </li>
                    <li>
                        Groups assigned to Ghost Databases.
                    </li>
                    <li>
                        The customer provided with instruction on the proper use of the tunneling client.
                    </li>
                </ul>
            </div>
        </Col>
        </Row>
        <Button variant="dymium" size="sm" className="mt-4" type="submit">
            Apply
        </Button>
    </Form>
}

function EditCustomers() {
    const [spinner, setSpinner] = useState(false)
    const [customers, setCustomers] = useState<any[]>([])
    const [alert, setAlert] = useState<JSX.Element>(<></>)

    const [validated, setValidated] = useState(false)
    const [name, setName] = useState("")
    const [schema, setSchema] = useState("")
    const [orgid, setOrgid] = useState("")
    const [domain, setDomain] = useState("")
    const [group, setGroup] = useState("")
    const [showdelete, setShowdelete] = useState(false)

    let form = useRef<HTMLFormElement>(null)
    let rememberedSelection = useAppSelector((state) => {
        return state.reducer.activeCustomer
    }
    )

    let fillDetails = useCallback(() => {
        customers.forEach(x => {
            if (x.id === rememberedSelection) {
                setName(x.name)
                setOrgid(x.orgid)
                setDomain(x.domain)
                setSchema(x.schema)
                setGroup(x.admingroup)
            }
        })
    }, [rememberedSelection, customers])


    const appDispatch = useAppDispatch()

    let columns = [
        {
            dataField: 'id',
            text: 'id',
            hidden: true,
        },
        {
            dataField: 'name',
            text: 'Name:',
            headerStyle: { width: '20em' },
            sort: true,
        },
        {
            dataField: 'schema',
            text: 'Schema:',
            headerStyle: { width: '20em' },
            sort: true,
        },
        {
            dataField: 'orgid',
            text: 'Org Id:',
            headerStyle: { width: '20em' },
            sort: true,
        },
        {
            dataField: 'domain',
            text: 'Domain:',
            headerStyle: { width: '20em' },
            sort: true,
        },
        {
            dataField: 'admingroup',
            text: 'Fallback Admin:',
            headerStyle: { width: '20em' },
            sort: true,
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
    let onEdit = id => {
        return e => {
            appDispatch(setActiveCustomer(id))
        }
    }
    let onDelete = id => {
        return e => {
            appDispatch(setActiveCustomer(id))
            setShowdelete(true)
        }
    }
    let GetCustomers = () => {
        setSpinner(true)
        http.sendToServer("GET", "/api/getcustomers",
            null, "",
            resp => {
                resp.json().then(js => {
                    setCustomers(js)
                    if (rememberedSelection !== "") {
                        fillDetails()
                    }
                    setTimeout(() => setSpinner(false), 500)
                }).catch((error) => {
                    setSpinner(false)
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Invalid server response {error.message}
                        </Alert>
                    )
                })
            },
            resp => {
                console.log("on error")
                resp != null && resp.text().then(t =>
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Error getting customers: {t}
                        </Alert>
                    ))
                setSpinner(false)
            },
            error => {
                console.log("on exception: " + error)
                setAlert(
                    <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        Error getting customers {error.message}.
                    </Alert>
                )
                setSpinner(false)
            })
    }
    let UpdateCustomer = () => {
        setSpinner(true)
        let uc = admin.Customer.fromJson({
            id: rememberedSelection,
            name,
            orgid,
            domain,
            schema,
            admingroup: group
        })
        http.sendToServer("POST", "/api/updatecustomer",
            null, uc.toJson(),
            resp => {
                resp.json().then(js => {
                    GetCustomers()
                    setAlert(
                        <Alert variant="success" onClose={() => setAlert(<></>)} dismissible>
                            Customer {customerName()}'s record updated
                        </Alert>
                    )

                }).catch((error) => {
                    setSpinner(false)
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Invalid server response {error.message}
                        </Alert>
                    )
                })
            },
            resp => {
                console.log("on error")
                resp != null && resp.text().then(t =>
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Error updating customer {customerName()} : {t}.
                        </Alert>
                    ))
                setSpinner(false)
            },
            error => {
                console.log("on exception: " + error)
                setAlert(
                    <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        Error getting customers {error.message}.
                    </Alert>
                )
                setSpinner(false)
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
        UpdateCustomer()
        return false
    }


    useEffect(() => {

        GetCustomers()
    }, [rememberedSelection])

    useEffect(() => {
        fillDetails()
    }, [rememberedSelection, fillDetails])

    let selectRow = {
        mode: 'radio',
        //clickToSelect: true,
        style: { backgroundColor: 'rgba(0, 151, 206, 0.3)' },
        selected: [rememberedSelection],
        onSelect: (row, isSelect, rowIndex, e) => {

            appDispatch(setActiveCustomer(row["id"]))
        },
    };
    let deleteCustomer = () => {
        setShowdelete(false)
        setSpinner(true)
        let dc = admin.DeleteCustomer.fromJson({ id: rememberedSelection, schema })
        http.sendToServer("POST", "/api/deletecustomer",
            null, dc.toJson(),
            resp => {

                resp.json().then(js => {
                    GetCustomers()
                    appDispatch(setActiveCustomer(""))
                    setAlert(
                        <Alert variant="success" onClose={() => setAlert(<></>)} dismissible>
                            Customer {customerName()}'s record deleted successfully
                        </Alert>
                    )
                    setTimeout(() => setSpinner(false), 500)

                }).catch((error) => {
                    setSpinner(false)
                    GetCustomers()
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Invalid server response {error.message}
                        </Alert>
                    )
                })
            },
            resp => {
                console.log("on error")
                if (resp != null) {
                    resp.text().then(t => {
                        setAlert(
                            <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                                Error: {t}
                            </Alert>
                        )
                        appDispatch(setActiveCustomer(""))
                        GetCustomers()
                    })
                }
                setSpinner(false)
            },
            error => {
                console.log("on exception: " + error)
                setAlert(
                    <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        Error deleting customer {error.message}
                    </Alert>
                )
                appDispatch(setActiveCustomer(""))
                GetCustomers()
                setSpinner(false)
            })
    }
    let customerName = () => {
        let retname = ""
        customers.forEach(x => {
            if (x.id === rememberedSelection) {
                retname = x.name

            }
        })
        return retname
    }
    return <>
        <Modal centered show={showdelete} onHide={() => setShowdelete(false)} data-testid="modal-delete">
            <Modal.Header closeButton>
                <Modal.Title>Delete customer <b>{customerName()}</b> record?</Modal.Title>
            </Modal.Header>
            <Modal.Body>Are you sure you want to delete the customer <b>{customerName()}</b> record? This operation is irreversible.</Modal.Body>
            <Modal.Footer>
                <Button variant="danger" role="button" id="Delete" data-testid="Delete"
                    aria-label={"Delete"}
                    onClick={() => {
                        deleteCustomer()
                    }
                    }>Delete</Button> <Button variant="dymium" onClick={() => {
                        setShowdelete(false)
                    }}>Cancel</Button>
            </Modal.Footer>
        </Modal>

        <ToolkitProvider
            bootstrap4
            keyField='id'
            data={customers}
            columns={columns}
            search >
            {
                props => (<div className="text-left">
                    {alert}
                    <div className="d-flex">
                        <h5 >Edit Customers  <Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>


                        <div style={{ marginLeft: "auto" }}>
                            <SearchBar size="sm" {...props.searchProps} />
                            <ClearSearchButton {...props.searchProps} />

                        </div>
                    </div>
                    <div className="d-block mb-3 w-100 testtable" style={{ overflow: "scroll" }}>
                        <BootstrapTable id="scaledtable"
                            selectRow={selectRow}
                            condensed
                            keyField='id'
                            striped bootstrap4 bordered={false}
                            pagination={paginationFactory()}
                            {...props.baseProps}
                        />
                    </div>
                </div>
                )
            }
        </ToolkitProvider>
        {rememberedSelection !== "" &&
            <div className="mb-5">
                <CustomerForm
                    edit={true}
                    handleSubmit={handleSubmit} form={form} validated={validated}
                    name={name} setName={setName}
                    schema={schema} setSchema={setSchema}
                    orgid={orgid} setOrgid={setOrgid}
                    domain={domain} setDomain={setDomain}
                    group={group} setGroup={setGroup}

                />
            </div>
        }
    </>
}

function AddCustomer() {
    const [spinner, setSpinner] = useState(false)
    const [validated, setValidated] = useState(false)
    const [name, setName] = useState("")
    const [schema, setSchema] = useState("")
    const [orgid, setOrgid] = useState("")
    const [domain, setDomain] = useState("")
    const [group, setGroup] = useState("")
    const [alert, setAlert] = useState<JSX.Element>(<></>)

    let form = useRef<HTMLFormElement>(null)

    let CreateNewCustomer = () => {
        let body = admin.Customer.fromJson({
            name,
            orgid,
            schema,
            domain,
            admingroup: group
        })

        let sbody = body.toJson()
        setSpinner(true)
        http.sendToServer("POST", "/api/createnewcustomer",
            null, sbody,
            resp => {

                resp.json().then(_js => {
                    let js = types.ConnectionResponse.fromJson(_js)
                    if (js.status !== "OK") {
                        setAlert(
                            <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                                Error retrieving connections: {js.errormessage} { }
                            </Alert>
                        )
                        setTimeout(() => setSpinner(false), 500)
                        return
                    } else {
                        setAlert(
                            <Alert variant="success" onClose={() => setAlert(<></>)} dismissible>
                                Customer {name} created successfully
                            </Alert>
                        )
                        setName("")
                        setDomain("")
                        setOrgid("")
                        setGroup("")
                        setSchema("")
                    }
                    setTimeout(() => setSpinner(false), 500)
                })


            },
            resp => {
                console.log("on error")
                resp != null &&
                    resp.text().then(t =>
                        setAlert(
                            <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                                Error: {t}
                            </Alert>
                        )
                    )
                setSpinner(false)
            },
            error => {
                console.log("on exception: " + error)
                setAlert(
                    <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        {error.message}
                    </Alert>
                )
                setSpinner(false)
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
        CreateNewCustomer()
        return false
    }
    return <>
        <h5 >Add New Customer <Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>
        {alert}
        <CustomerForm
            edit={false}
            handleSubmit={handleSubmit} form={form} validated={validated}
            name={name} setName={setName}
            schema={schema} setSchema={setSchema}
            orgid={orgid} setOrgid={setOrgid}
            domain={domain} setDomain={setDomain}
            group={group} setGroup={setGroup}

        />
    </>
}

function Customers() {
    const t = useAppSelector((state) => {

        return state.reducer.activeCustomersTab
    }
    )

    const appDispatch = useAppDispatch()


    return (
        <Tabs
            activeKey={t}
            id="customers"
            onSelect={(k) => appDispatch(setActiveCustomersTab(k))}
            unmountOnExit={true} className="mb-3 text-left">
            <Tab eventKey="add" title="Add" className="mx-4">
                <AddCustomer />
            </Tab>
            <Tab eventKey="edit" title="Edit" className="mx-4">
                <EditCustomers />
            </Tab>
        </Tabs>
    )
}

export default Customers;