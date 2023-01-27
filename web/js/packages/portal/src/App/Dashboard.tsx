import React, { useEffect, useState } from 'react';
import { Navigate } from "react-router-dom";
import Spinner from '@dymium/common/Components/Spinner'
import Tabs from 'react-bootstrap/Tabs'
import Tab from 'react-bootstrap/Tab'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import * as http from '@dymium/common/Api/Http'
import './Dashboard.scss';

function getReadableSizeString(fileSizeInBytes) {
    fileSizeInBytes = parseInt(fileSizeInBytes)
    var i = -1;
    var byteUnits = [' kB', ' MB', ' GB', ' TB', 'PB', 'EB', 'ZB', 'YB'];
    do {
      fileSizeInBytes /= 1024;
      i++;
    } while (fileSizeInBytes > 1024);
  
    return Math.max(fileSizeInBytes, 0.1).toFixed(1) + byteUnits[i];
  }
function Usage() {
    const [spinner, setSpinner] = useState(false)
    const [alert, setAlert] = useState<JSX.Element>(<></>)
    const [connections, setConnections] = useState(0)
    const [datascopes, setDatascopes] = useState(0)
    const [logins, setLogins] = useState(0)
    const [tunnels, setTunnels] = useState(0)
    const [blocked, setBlocked] = useState(0)
    const [obfuscated, setObfuscated] = useState(0)
    const [redacted, setRedacted] = useState(0)
    const [bytesin, setBytesin] = useState("")
    const [bytesout, setBytesout] = useState("")
    const [connectors, setConnectors] = useState(0)
    const [connectortunnels, setConnectortunnels] = useState(0)


    let sendQuery = () => {
        setSpinner(true)
        let body = JSON.stringify({ })
        http.sendToServer("POST", "/api/getusage",
            null, body,
            resp => {
                resp.json().then(js => {
                    setConnections(js.connections)
                    setDatascopes(js.datascopes)
                    setLogins(js.logins)
                    setBlocked(js.blocked)
                    setObfuscated(js.obfuscated)
                    setRedacted(js.redacted)
                    setTunnels(js.tunnels)
                    setBytesin(getReadableSizeString(js.bytesin))
                    setBytesout(getReadableSizeString(js.bytesout))
                    setConnectors(js.connectors)
                    setConnectortunnels(js.connectortunnels)
                    setTimeout( () => setSpinner(false), 500)

                }).catch((error) => {

                })
            },
            resp => {
                setSpinner(false)

            },
            error => {
                console.log("on exception")
                setSpinner(false)
            })
    }
    useEffect(() => {
        sendQuery()
    }, [])
    return (
        <div className="text-left">
            {alert}
            <h5 > System Usage <Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>
            <Row style={{marginTop: '3em'}}> 
                <Col className="card"> 
                    <h6 style={{fontSize: '1.1em'}}><i className="fa-solid fa-link mr-2"></i>Links to data sources:</h6>
                    <div style={{fontSize: '2.0em'}}>{connections}</div>
                </Col>
                <Col className="card"> 
                    <h6 style={{fontSize: '1.1em'}}><i className=" fas fa-diagram-project fa-fw  mr-2"></i>Connectors configured:</h6>
                    <div style={{fontSize: '2.0em'}}>{connectors}, with {connectortunnels} tunnels</div>
                </Col>                
                <Col className="card">
                <h6 style={{fontSize: '1.1em'}}><i className="fa-solid fa-ghost mr-2"></i> Ghost Databases:</h6>
                    <div style={{fontSize: '2.0em'}}>{datascopes}</div>                
                </Col>
                <Col className="card">
                <h6 style={{fontSize: '1.1em'}}><i className="fa-solid fa-right-to-bracket mr-2"></i>Successful logins:</h6>
                    <div style={{fontSize: '2.0em'}}>{logins}</div>                
                </Col>     
                        
            </Row>
            <Row style={{marginTop: '3em'}}> 
                <Col className="card"> 
                    <h6 style={{fontSize: '1.1em'}}><i className="fa-solid fa-columns mr-2"></i>Columns blocked:</h6>
                    <div style={{fontSize: '2.0em'}}>{blocked}</div>
                </Col>
                <Col className="card">
                <h6 style={{fontSize: '1.1em'}}><i className="fa-solid fa-columns mr-2"></i>Columns obfuscated:</h6>
                    <div style={{fontSize: '2.0em'}}>{obfuscated}</div>                
                </Col>
                <Col className="card">
                <h6 style={{fontSize: '1.1em'}}><i className="fa-solid fa-columns mr-2"></i>Columns redacted:</h6>
                    <div style={{fontSize: '2.0em'}}>{redacted}</div>                
                </Col>                
            </Row>            
            <Row style={{marginTop: '3em'}}> 
            <Col className="card">
                <h6 style={{fontSize: '1.1em'}}><i className="fa-solid fa-network-wired mr-2"></i>User Tunnels established:</h6>
                    <div style={{fontSize: '2.0em'}}>{tunnels}</div>                
                </Col>                       
                <Col className="card"> 
                    <h6 style={{fontSize: '1.1em'}}><i className="fa fa-traffic-light mr-2"></i>Traffic out:</h6>
                    <div style={{fontSize: '2.0em'}}>{bytesout}</div>
                </Col>
                <Col className="card">
                <h6 style={{fontSize: '1.1em'}}><i className="fa fa-traffic-light mr-2"></i>Traffic in:</h6>
                    <div style={{fontSize: '2.0em'}}>{bytesin}</div>                
                </Col>
             
            </Row>               
        </div>
    )
}
function Dashboard() {
    return (

        <Tabs
            unmountOnExit={true} className="mb-3 text-left">
            <Tab eventKey="usage" title="Usage" className="mx-4">
                <Usage />
            </Tab>


        </Tabs>

    )
}

export default Dashboard;