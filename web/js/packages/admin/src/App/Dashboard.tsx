
import React, { useEffect, useState } from 'react';
import { Navigate } from "react-router-dom";
import Spinner from '@dymium/common/Components/Spinner'
import Tabs from 'react-bootstrap/Tabs'
import Tab from 'react-bootstrap/Tab'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Offcanvas from '@dymium/common/Components/Offcanvas'
import Alert from 'react-bootstrap/Alert'
import * as http from '@dymium/common/Api/Http'
//import './Dashboard.scss';

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
    const [customers, setCustomers] = useState(0)


    let sendQuery = () => {
        setSpinner(true)
        debugger
        http.sendToServer("GET", "/api/getglobalusage",
            null, "",
            resp => {
                resp.json().then(js => {
                    setCustomers(js.customers)

                    setTimeout( () => setSpinner(false), 500)

                }).catch((error) => {
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        Error getting stats: {error.messsage}
                      </Alert>
                    )
                })
            },
            resp => {
                setSpinner(false)
                resp && resp.text().then(t => {
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        Error getting stats: {t}
                      </Alert>
                    )                    
                })
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
                    <h6 style={{fontSize: '1.1em'}}><i className="fa-solid fa-link mr-2"></i>Customers:</h6>
                    <div style={{fontSize: '2.0em'}}>{customers}</div>
                </Col>

                        
            </Row>
           
        </div>
    )
}

function Dashboard() {

    return (
        <Tabs
            defaultActiveKey="beye"
            id="birdseye"

            unmountOnExit={true} className="mb-3 text-left">
            <Tab eventKey="beye" title="Bird's Eye View" className="mx-4">
                <Usage />
            </Tab>

        </Tabs>
    )
}
export default Dashboard;