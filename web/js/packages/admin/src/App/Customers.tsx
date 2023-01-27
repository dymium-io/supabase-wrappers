import React, { useEffect, useState, useRef } from 'react';
import { Navigate } from "react-router-dom";
import Form from 'react-bootstrap/Form'
import Button from 'react-bootstrap/Button'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Tabs from 'react-bootstrap/Tabs'
import Tab from 'react-bootstrap/Tab'
import Spinner from '@dymium/common/Components/Spinner'
import Alert from 'react-bootstrap/Alert'
import BootstrapTable from 'react-bootstrap-table-next';
import paginationFactory from 'react-bootstrap-table2-paginator';
import ToolkitProvider, { Search } from 'react-bootstrap-table2-toolkit/dist/react-bootstrap-table2-toolkit';
import 'react-bootstrap-table-next/dist/react-bootstrap-table2.min.css';


import * as http from '@dymium/common/Api/Http'
import * as types from '@dymium/common/Types/Common'
import * as admin from '@dymium/common/Types/Admin'
const { SearchBar, ClearSearchButton } = Search;

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
        dataField: 'orgdomainid',
        text: 'Domain:',
        headerStyle: { width: '20em' },
        sort: true,
    },    
]

function EditCustomers() {
    const [spinner, setSpinner] = useState(false)
    const [customers, setCustomers] = useState<any[]>([])
    const [alert, setAlert] = useState<JSX.Element>(<></>)

    return <>
        <ToolkitProvider
            bootstrap4
            keyField='name'
            data={customers}
            columns={columns}
            search >
            {
                props => ( <div className="text-left">
                        {alert}
                        <div className="d-flex">
                            <h5 >Edit Customers  <Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>


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

    </>
}

function AddCustomer() {
    const [spinner, setSpinner] = useState(false)
    const [validated, setValidated] = useState(false)
    const [customer, setCustomer] = useState("")
    const [schema, setSchema] = useState("")
    const [orgid, setOrgid] = useState("")
    const [domain, setDomain] = useState("")
    const [group, setGroup] = useState("")
    const [alert, setAlert] = useState<JSX.Element>(<></>)

    let form = useRef<HTMLFormElement>(null)

    let CreateNewCustomer = () => {
        let body = admin.Customer.fromJson({
            name: customer,
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
                                Customer {customer} created successfully
                            </Alert>
                        )
                    }
                    setTimeout(() => setSpinner(false), 500)
                })


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
        <Form onSubmit={handleSubmit} ref={form} noValidate validated={validated}>
            <Row>
                <Col xs="auto">
                    <Form.Group className="mb-3" controlId="ccname">
                        <Form.Label>Customer name:</Form.Label>
                        <Form.Control size="sm" type="text"
                            required placeholder="anything goes"
                            style={{ width: '40em' }}
                            pattern=".+"
                            value={customer}
                            onChange={e => {
                                setCustomer(e.target.value)
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
                    <Form.Group className="mb-3" controlId="schemaid">
                        <Form.Label>Schema name:</Form.Label>
                        <Form.Control size="sm" type="text"
                            required placeholder="small cap alphanum"
                            style={{ width: '40em' }}
                            pattern="[a-zA_Z0-9_]+"
                            value={schema}
                            onChange={e => {
                                setSchema(e.target.value)
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
                            pattern="[a-z0-9_]+"
                            value={orgid}
                            onChange={e => {
                                setOrgid(e.target.value)
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
                            value={domain}
                            onChange={e => {
                                setDomain(e.target.value)
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
                            value={group}
                            onChange={e => {
                                setGroup(e.target.value)
                            }}
                        />
                        <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                        <Form.Control.Feedback type="invalid" >
                            Enter fallback admin group
                        </Form.Control.Feedback>
                    </Form.Group>
                </Col>
            </Row>
            <Button variant="dymium" size="sm" className="mt-4" type="submit">
                Apply
            </Button>
        </Form>

    </>
}

function Customers() {


    return (
        <Tabs
            defaultActiveKey="add"
            id="cusomers"

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