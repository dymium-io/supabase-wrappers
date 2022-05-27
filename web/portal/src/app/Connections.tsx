import React, { useEffect, useState, useRef } from 'react';
import Tabs from 'react-bootstrap/Tabs'
import Tab from 'react-bootstrap/Tab'
import Form from 'react-bootstrap/Form'
import Button from 'react-bootstrap/Button'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import BootstrapTable from 'react-bootstrap-table-next';


import paginationFactory from 'react-bootstrap-table2-paginator';

import ToolkitProvider, { Search } from 'react-bootstrap-table2-toolkit';
/*
const { SearchBar, ClearSearchButton } = Search;
*/

import {tooltip} from '../Components/Tooltip'
import PasswordField from '../Components/PasswordField'
import * as com from '../Common'
import {useInitialize} from '../Utils/CustomHooks'
const databases = Object.keys(com.databaseTypes).map(key => {
    return <option key={key} value={key}>
        {com.databaseTypes[key]}
    </option>
})

function AddConnection() {
    const [validated, setValidated] = useState(false)
    let form = useRef<HTMLFormElement>(null)

    const [name, setName] = useState("")
    const [dbtype, setDBType] = useState("")
    const [address, setAddress] = useState("")
    const [port, setPort] = useState("")
    const [useTLS, setUseTLS] = useState(false)
    const [username, setUsername] = useState("")
    const [password, setPassword] = useState("")
    const [description, setDescription] =  useState("")
    let sendConnection = () => { 
        let body = JSON.stringify( {name, dbtype, address, port: parseInt(port), useTLS, username, password, description} )
        com.sendToServer("POST", "/api/createnewconnection", 
            null, body, 
            resp=> {
                console.log("on error")
            }, 
            resp=>{
                console.log("on success")
            }, 
            error=>{
                console.log("on exception")
            }) 
    }

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

        sendConnection()

        return false
    }

    return (
        <div className=" text-left">
            <h5 > Create New Connection</h5>
            <Form onSubmit={handleSubmit} ref={form} noValidate validated={validated}>
                <Row>
                    <Col xs="auto">
                        <Form.Group className="mb-3" controlId="dbtype" >
                            <Form.Label >Database type</Form.Label>
                            <Form.Control as="select"required size="sm" value={dbtype}
                            onChange={e=>setDBType(e.target.value)}
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
                        <Form.Group className="mb-3" controlId="dbname">
                            <Form.Label>{tooltip('Dymium prefix', 
                            <div className="d-block">
                               The prefix is used to identify the target database from the SQL sent to the Dymium proxy server.
                               For example, instead of
                               <div className='ml-2 my-1'>select * from mytable;</div>
                               you should use:
                               <div className='ml-2 my-1'>select * from prefix_mytable;</div>
                            </div>
                            , 'auto', '', false)}</Form.Label>
                            <Form.Control size="sm" type="text" placeholder="alphanum, _$^!"
                                required
                                pattern="[a_zA_Z\_$^]+"
                                value={name}
                                onChange={e=>setName(e.target.value)}
                            />
                            <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                            <Form.Control.Feedback type="invalid" >
                                Type systemwide unique prefix to use in SQL
                            </Form.Control.Feedback>                            
                        </Form.Group>
                    </Col>
                </Row>
                <Row>
                    <Col xs="auto">
                        <Form.Group className="mb-3" controlId="ipaddress">
                            <Form.Label>Address</Form.Label>
                            <Form.Control size="sm" type="text" placeholder="DB IP address or host name"
                                required
                                pattern="^[a-zA-Z0-9._]+$"
                                value={address}
                                onChange={e=>setAddress(e.target.value)}
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
                                value={port}
                                onChange={e=>setPort(e.target.value)}
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
                                checked={useTLS}
                                onChange={e=>setUseTLS(e.target.checked)}
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
                                value={username}
                                onChange={e=>setUsername(e.target.value)}
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
                                value={password}
                                className="w-12em"
                                onChange={e=>setPassword(e.target.value)}                                
                                size="sm" />
                        </Form.Group>
                    </Col>
                </Row>
                <Row>
                    <Col xs="auto">
                        <Form.Group className="mb-3" controlId="dbpassword">
                            <Form.Label>Description</Form.Label>
                            <Form.Control as="textarea" rows={3}  style={{width: '35em'}}
                                required
                                placeholder="Please put in the description of this connection"
                                onChange={e=>setDescription(e.target.value)} 
                                value={description}
                            />
                        <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                            <Form.Control.Feedback type="invalid" >
                               Please put in some description
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
        text: 'Prefix:',
        sort: true,
    },
    {
        dataField: 'dbtype',
        text: 'DB Type:',
        sort: true
    },
    {
        dataField: 'address',
        text: 'Address:',
        sort: true
    },
    {
        dataField: 'port',
        text: 'Port:',
        sort: true
    },   
    {
        dataField: 'usetls',
        text: 'Use TLS',
        sort: true
    },       
    {
        dataField: 'description',
        text: 'Description:',
        sort: true
    },    
]
function EditConnections(props) {
    let [conns, setConns] = useState([])
    let getConnections = () => { 
       
        com.sendToServer("GET", "/api/getconnections", 
            null, "", 
            resp=> {

                resp.json().then(js => {
                    console.log(js)
                    let cc
                    cc = js.map(x => {
                        return {
                            id: x.id,
                            credid: x.credid,
                            name: x.name,
                            dbname: x.dbname,
                            address: x.address,
                            port: x.port,
                            description: x.description,
                        
                        }
                    }
                 
                    )
                    setConns(cc)
                })

                console.log("on success")
            }, 
            resp=>{
                console.log("on error")
            }, 
            error=>{
                console.log("on exception: "+error)
            }) 
    }
    useEffect(() => {
        getConnections()
    }, []) 

    return (

        <div className=" text-left">
            <h5 >Edit Connections</h5>

                                <BootstrapTable id="scaledtable"
                                    striped bootstrap4 bordered={false}
                                    pagination={paginationFactory()}
                                    keyField='prefix'
                                    data={conns}
                                    columns={columns}                                    
                                    {...props.baseProps}
                                />
      

    </div>
    )
 }
function Connections() {
    return (
        <Tabs defaultActiveKey="add" id="connections" unmountOnExit={true} className="mb-3 text-left">
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