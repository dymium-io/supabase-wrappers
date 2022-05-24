import React, { useEffect, useState, useRef } from 'react';
import Tabs from 'react-bootstrap/Tabs'
import Tab from 'react-bootstrap/Tab'
import Form from 'react-bootstrap/Form'
import Button from 'react-bootstrap/Button'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import PasswordField from '../Components/PasswordField'
import * as com from '../Common'

const databases = Object.keys(com.databaseTypes).map(key => {
    return <option key={key} value={key}>
        {com.databaseTypes[key]}
    </option>
})

function AddConnection() {
    const [validated, setValidated] = useState(false)
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

        const data = new FormData(event.target);



        return false
    }

    return (

        <div className=" text-start">
            <h5 > Create New Connection</h5>
            <Form onSubmit={handleSubmit} ref={form} noValidate validated={validated}>
                <Row>
                    <Col xs="auto">
                        <Form.Group className="mb-3" controlId="dbname">
                            <Form.Label>Name</Form.Label>
                            <Form.Control size="sm" type="text" placeholder="Human readable name"
                                required
                                pattern=".+"
                                defaultValue=""
                            />
                            <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                            <Form.Control.Feedback type="invalid" >
                                Type connection name
                            </Form.Control.Feedback>                            
                        </Form.Group>
                    </Col>
                    <Col xs="auto">
                        <Form.Group className="mb-3" controlId="dbtype" >
                            <Form.Label >Database type</Form.Label>
                            <Form.Select required size="sm">
                                <option value="">...</option>
                                {databases}
                            </Form.Select>
                            <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                            <Form.Control.Feedback type="invalid" >
                                Select DB type
                            </Form.Control.Feedback>
                        </Form.Group>
                    </Col>

                </Row>

                <Row>
                    <Col xs="auto">
                        <Form.Group className="mb-3" controlId="ipaddress">
                            <Form.Label>Address</Form.Label>
                            <Form.Control size="sm" type="text" placeholder="Enter IP address or host name"
                                required
                                pattern="^[a-zA-Z0-9._]+$"
                                defaultValue=""
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
                                placeholder="Enter port number"
                                defaultValue=""
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
                            />
                        </Form.Group>
                    </Col>
                </Row>
                <Row>
                    <Col xs="auto">
                        <Form.Group className="mb-3" controlId="dbusername">
                            <Form.Label>Username</Form.Label>
                            <Form.Control size="sm" type="text" placeholder="DB username"
                                required
                                pattern=".+"
                                defaultValue="" />
                        <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                            <Form.Control.Feedback type="invalid" >
                               Admin name for DB
                            </Form.Control.Feedback>                              </Form.Group>
                  
                    </Col>
                    <Col xs="auto">
                        <Form.Group className="mb-3" controlId="dbpassword">
                            <Form.Label>Password</Form.Label>
                            <PasswordField type="password"
                                required
                                defaultValue=""
                                placeholder="DB password"
                                pattern=".+"
                                size="sm" />
                        <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                            <Form.Control.Feedback type="invalid" >
                               Admin password for DB
                            </Form.Control.Feedback>                                    
                        </Form.Group>
                    </Col>
                </Row>
                <Button variant="dymium" className="mt-4" type="submit">
                    Apply
                </Button>
            </Form>
        </div>
    )
}

function EditConnections() {

    return (

        <div>Edit Connections</div>
    )
}

function Connections() {
    return (
        <Tabs defaultActiveKey="add" id="connections" className="mb-3 text-start">
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