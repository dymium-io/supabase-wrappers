import React, { useEffect, useState, useRef } from 'react';
import Tabs from 'react-bootstrap/Tabs'
import Tab from 'react-bootstrap/Tab'
import Form from 'react-bootstrap/Form'
import Button from 'react-bootstrap/Button'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import BootstrapTable from 'react-bootstrap-table-next';
import paginationFactory from 'react-bootstrap-table2-paginator';
import ToolkitProvider, {Search} from 'react-bootstrap-table2-toolkit/dist/react-bootstrap-table2-toolkit';
import 'react-bootstrap-table-next/dist/react-bootstrap-table2.min.css';

const { SearchBar, ClearSearchButton } = Search;


import {tooltip} from '../Components/Tooltip'
import PasswordField from '../Components/PasswordField'
import * as com from '../Common'
import Spinner from '../Components/Spinner'
import {useInitialize} from '../Utils/CustomHooks'
const databases = Object.keys(com.databaseTypes).map(key => {
    return <option key={key} value={key}>
        {com.databaseTypes[key]}
    </option>
})

function SpecifyConnection(props) {

}
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
    const [spinner, setSpinner] =  useState(false)
    let sendConnection = () => { 
        setSpinner(true)
        let body = JSON.stringify( {name, dbtype, address, port: parseInt(port), useTLS, username, password, description} )
        com.sendToServer("POST", "/api/createnewconnection", 
            null, body, 
            resp=> {
                console.log("on error")
                setSpinner(false)
            }, 
            resp=>{
                console.log("on success")
                setSpinner(false)
            }, 
            error=>{
                console.log("on exception")
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
            <h5 > Create New Connection <Spinner show={spinner} style={{width: '28px'}}></Spinner></h5>
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
                            <Form.Label>{tooltip('Dymium name', 
                            <div className="d-block">
                               The name is used to identify the target database from the SQL sent to the Dymium proxy server.
                               For example, instead of
                               <div className='ml-2 my-1'>select * from mytable;</div>
                               you should use:
                               <div className='ml-2 my-1'>select * from name_mytable;</div>
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
                                Type systemwide unique name to use in SQL
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
                        <Form.Group className="mb-3" controlId="description">
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
        text: 'Name:',
        sort: true,
    },
    {
        dataField: 'dbtype',
        text: 'DB Type:',
        formatter: (cell, row, rowIndex, formatExtraData) => { 
            return com.databaseTypes[row["dbtype"]]
        },
        sortValue: (cell, row) => {
            return com.databaseTypes[row["dbtype"]]
         } ,
        sort: true
    },
    {
        dataField: 'address',
        text: 'Address:',
        sort: true
    },
    {
        dataField: 'description',
        text: 'Description:',
        sort: true
    },        
    {
        dataField: 'port',
        text: 'Port:',
        headerStyle: { width: '100px' },
        sort: true
    },   
    {
        dataField: 'usetls',
        text: 'Use TLS',
        formatter: (cell, row, rowIndex, formatExtraData) => { 
      
            if(row.usetls)
                return  <i className="fa-solid fa-check blue"></i>
            else 
                return <></>
        },        
        headerStyle: { width: '130px' },
        sort: true,
        align: 'center'
    },       

    {
        text: 'Edit',
        isDummyField: true,
        formatter: () => { 
            return <i className="fas fa-edit ablue" role="button"></i>
        },
        //formatExtraData: { hoverIdx: this.state.hoverIdx },
        headerStyle: { width: '50px' },
        style: { height: '30px' },
        align: 'center'
      },    
      {
          text: 'Delete',
          isDummyField: true,
          formatter: () => { 
              return <i className="fas fa-trash ablue" role="button"></i>
          },
          //formatExtraData: { hoverIdx: this.state.hoverIdx },
          headerStyle: { width: '90px' },
          style: { height: '30px' },
          align: 'center'
        }

]
function EditConnections(props) {
    let [conns, setConns] = useState([])
    const [spinner, setSpinner] =  useState(false)

    let getConnections = () => { 
       setSpinner(true)
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
                            dbtype: x.dbtype,
                            name: x.name,
                            dbname: x.dbname,
                            address: x.address,
                            port: x.port,
                            description: x.description,
                            usetls: x.useTLS,
                        
                        }
                    }
                 
                    )
                    setConns(cc)
                })
                setSpinner(false)
                console.log("on success")
            }, 
            resp=>{
                console.log("on error")
                setSpinner(false)                
            }, 
            error=>{
                console.log("on exception: "+error)
                setSpinner(false)                
            }) 
    }
    useEffect(() => {
        getConnections()
    }, []) 

    return (

        <div className=" text-left">
           


    <div id="tablecontainer" style={{width: '90%'}} className="text-center">
      <ToolkitProvider
                    bootstrap4
                    keyField='prefix'
                    data={conns}
                    columns={columns}
                    search >
                    {
                        props => (
                            <div className="text-left">
                                <div  className="d-flex">
                                 <h5 >Edit Connections  <Spinner show={spinner} style={{width: '28px'}}></Spinner></h5>
                                <div style={{marginLeft: "auto"}}>
                                <SearchBar size="sm" {...props.searchProps} />
                                <ClearSearchButton {...props.searchProps} />
                                </div>
                                </div>
                                <div className="d-block">
                                <BootstrapTable id="scaledtable"
                                    striped bootstrap4 bordered={false}
                                    pagination={paginationFactory()}
                                    {...props.baseProps}
                                />
                                </div>
                            </div>
                        )
                    }
                </ToolkitProvider>
                </div>
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