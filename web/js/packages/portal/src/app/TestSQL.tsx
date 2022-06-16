import React, { useEffect, useState, useRef } from 'react';
import Tabs from 'react-bootstrap/Tabs'
import Tab from 'react-bootstrap/Tab'
import Form from 'react-bootstrap/Form'
import Button from 'react-bootstrap/Button'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import { Link } from "react-router-dom";
import Alert from 'react-bootstrap/Alert'
import Spinner from '@dymium/common/Components/Spinner'
import * as com from '../Common'
import * as types from '@dymium/common/Types/Common'

 function Test() {
  const [spinner, setSpinner] = useState(false)
  const [alert, setAlert] = useState<JSX.Element>(<></>)
  const [datascopes, setDatascopes] = useState<types.DataScopeInfo[]>([])
  const [selectedDatascope, setSelectedDatascope] = useState("")


  let getDatascopes = () => {
    setSpinner(true)
    com.sendToServer("GET", "/api/getdatascopes",
      null, "",
      resp => {

        resp.json().then(js => {
          setDatascopes(js)
        })

        setSpinner(false)
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

  useEffect(() => {
    getDatascopes()

  }, [])
  return (
    <div className=" text-left">
      {alert}

      <h5 > Run SQL statement against Data Scope virtual database<Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>
      <div className=" text-left">

        <Row>
          <Col xs="auto">
            <Form.Group className="mb-3" controlId="connection" >
              <Form.Label >Select Data Scope</Form.Label>
              <Form.Control as="select" size="sm"
                onChange={e => {

                  setSelectedDatascope(e.target.value)
                  //appDispatch(setSelectedDatascopeDefault(e.target.value))

                }}
                value={selectedDatascope}
              >
                return <option value="">...</option>
                {datascopes.map(x => {

                  return <option key={x.id} value={x.id}>{x.name}</option>
                })
                }
              </Form.Control>

            </Form.Group>
          </Col>
        </Row>
        <Row>
          <Col>
            <Form.Group className="mb-3" controlId="sql">
              <Form.Label>Enter SQL statement</Form.Label>
              <Form.Control as="textarea" rows={3} />
            </Form.Group>
          </Col>
        </Row>
        {selectedDatascope !== "" &&
          <div className=" text-left">

            <Button variant="dymium" size="sm" className="mt-1" type="submit">
              Apply
            </Button>

          </div>
        }


      </div>
    </div>
  )

}


export default function TestSQL() {

  return (
      <Tabs

          unmountOnExit={true} className="mb-3 text-left">
          <Tab eventKey="test" title="Test SQL" className="mx-4">
              <Test />
          </Tab>

     
      </Tabs>
  )
}