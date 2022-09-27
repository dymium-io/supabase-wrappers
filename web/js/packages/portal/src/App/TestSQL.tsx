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
import * as types from '@dymium/common/Types/Internal'
import * as ctypes from '@dymium/common/Types/Common'
import * as http from '../Api/Http'
import { debugPort } from 'process';

function Test() {
  const [spinner, setSpinner] = useState(false)
  const [alert, setAlert] = useState<JSX.Element>(<></>)
  const [datascopes, setDatascopes] = useState<types.DataScopeInfo[]>([])
  const [selectedDatascope, setSelectedDatascope] = useState("")
  const [tables, setTables] = useState<ctypes.DatascopeTable[]>([])
  const [selectedTable, setSelectedTable] = useState<ctypes.DatascopeTable>()

  useEffect(() => {
    com.getDatascopes(setSpinner, setAlert, setDatascopes, () => {

    })

  }, [])
  useEffect(() => {
    if (selectedDatascope !== null) {
      //setSelectedTable()
      getTables()
    }
  }, [selectedDatascope])
  let getTables = () => {
    setSpinner(true)
    let id: ctypes.DatascopeId = new ctypes.DatascopeId();
    id.id = selectedDatascope

    let body: string = id.toJson()
    http.sendToServer("POST", "/api/getdatascopetables",
      null, body,
      resp => {
        resp.json().then(js => {
          if (js.status == "OK") {
            let t: ctypes.DatascopeTable[] = []

            js.tables.forEach((x) => {

              t.push(ctypes.DatascopeTable.fromJson(x))


            })
            setTables(t)



          } else {
            setAlert(
              < Alert variant="danger" onClose={() => setAlert(<></>)} dismissible >
                Error: {js.errormessage} !
              </Alert >)
          }
          setTimeout(() => setSpinner(false), 500)

        }).catch((error) => {

        })
      },
      resp => {
        setSpinner(false)
        setAlert(
          <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
            Error creating connection.
          </Alert>
        )

      },
      error => {
        console.log("on exception")
        setSpinner(false)
        setAlert(
          <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
            Error creating connection: {error.message} { }
          </Alert>
        )
      })
  }

  let getSelect = () => {
    if(selectedTable == null) {
      return
    }
    setSpinner(true)

    //let jj = {database: selectedTable.database, schema: selectedTable.schema, table: selectedTable.table}
    let jj = selectedTable.toJson()
    let body: string = jj 
    http.sendToServer("POST", "/api/getselect",
      null, body,
      resp => {
        resp.json().then(js => {
          if (js.status == "OK") {



          } else {
            setAlert(
              < Alert variant="danger" onClose={() => setAlert(<></>)} dismissible >
                Error: {js.errormessage} !
              </Alert >)
          }
          setTimeout(() => setSpinner(false), 500)

        }).catch((error) => {

        })
      },
      resp => {
        setSpinner(false)
        setAlert(
          <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
            Error executing query.
          </Alert>
        )

      },
      error => {
        console.log("on exception")
        setSpinner(false)
        setAlert(
          <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
            Error executing query: {error.message} { }
          </Alert>
        )
      })
  }

  let handleSubmit = event => {

    getSelect()

    event.preventDefault();
    event.stopPropagation();
    return false
  }
  return (
    <div className=" text-left">
      {alert}

      <h5 > Test data access and transformation<Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>
      <div className=" text-left">
        <Form onSubmit={handleSubmit} noValidate >
          <Row>
            <Col xs="auto">
              <Form.Group className="mb-3" controlId="connection" >
                <Form.Label >Select Data Scope</Form.Label>
                <Form.Control as="select" size="sm"
                  onChange={e => {

                    setSelectedDatascope(e.target.value)
                    //setSelectedTable(null)
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

            <Col xs="auto">
              <div style={{ marginTop: '3px', fontFamily: 'monospace' }}>Select * from</div>

            </Col>
            <Col xs="auto">
              <Form.Group className="mb-3" controlId="connection" >

                <Form.Control as="select" size="sm" style={{ fontFamily: 'monospace' }}
                  onChange={e => {

                    setSelectedTable(tables[parseInt(e.target.value)])
                    //appDispatch(setSelectedDatascopeDefault(e.target.value))

                  }}
                //value={table}
                >
                  return <option value="">...</option>
                  {tables.map((x, i) => {
                    
                    return <option key={i} value={i}>{x.database + '_' + x.schema + '.' + x.table}</option>
                  })
                  }
                </Form.Control>

              </Form.Group>
            </Col>
            <Col xs="auto">
              <div style={{ marginTop: '3px', fontFamily: 'monospace' }}> limit 20;</div>

            </Col>
            <Col>        {selectedDatascope !== "" && selectedTable !== null &&
              <div className=" text-left">

                <Button variant="dymium" size="sm" className="" type="submit">
                  Apply
                </Button>

              </div>
            }</Col>
          </Row>
        </Form>

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