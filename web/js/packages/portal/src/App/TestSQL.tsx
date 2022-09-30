import React, { useEffect, useState, useRef } from 'react';
import Tabs from 'react-bootstrap/Tabs'
import Tab from 'react-bootstrap/Tab'
import Form from 'react-bootstrap/Form'
import Button from 'react-bootstrap/Button'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import BootstrapTable from 'react-bootstrap-table-next';
import paginationFactory from 'react-bootstrap-table2-paginator';
import ToolkitProvider, { ColumnToggle } from 'react-bootstrap-table2-toolkit/dist/react-bootstrap-table2-toolkit';

import Alert from 'react-bootstrap/Alert'
import Spinner from '@dymium/common/Components/Spinner'
import * as com from '../Common'
import * as types from '@dymium/common/Types/Internal'
import * as ctypes from '@dymium/common/Types/Common'
import * as stypes from '@dymium/common/Types/DbSyncCommon'
import * as http from '../Api/Http'
import { debugPort } from 'process';
import * as b64 from '../Utils/Base64'

//const { ToggleList } = ColumnToggle;
//const ToggleList = ColumnToggle.ToggleList
type HeaderCell = {
  dataField: string,
  text: string,
  headerStyle: any,
  formatter: any,
  headerFormatter: any
}
let columns: HeaderCell[] = []


function IsImage(text): string {

  if (text[0] === 'G' && text[1] === 'I' && text[2] === 'F' && text[3] === '8' &&
    text[4] === '9' && text[5] === 'a') {
    console.log("GIF")
    return "gif"
  }
  if (text[0] === 211 && text[1] === 'P' && text[2] === 'N' && text[3] === 'G') {
    console.log("PNG")
    return "png"
  }
  if (text[0] === 0xFF && text[1] === 0xD8) {
    console.log("JPEG")
    return "jpeg"
  }

  return ""
}

function Test() {
  const [spinner, setSpinner] = useState(false)
  const [alert, setAlert] = useState<JSX.Element>(<></>)
  const [datascopes, setDatascopes] = useState<types.DataScopeInfo[]>([])
  const [selectedDatascope, setSelectedDatascope] = useState("")
  const [tables, setTables] = useState<ctypes.DatascopeTable[]>([])
  const [selectedTable, setSelectedTable] = useState<ctypes.DatascopeTable>()
  const [data, setData] = useState<{}[]>([])
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
    let processData = txt => {
      columns = []
      let data: {}[] = []
      let js = stypes.SqlTestResult.fromJson(txt)
      js.columns.forEach(x => {

        let col: HeaderCell = {
          dataField: x,
          text: x,
          headerStyle: { minWidth: '200px' },
          formatter: (cell, row) => {


            let b = cell.substring(1, cell.length)
            if (cell[0] === '1') {
              let bcell = b64.base64DecToArr(b)
              let o
              let m = IsImage(bcell)
              if (m !== "") {

                let src = `data:image/${m};base64,${b}`

                o = <img src={src} width="250px"></img>
              } else
                o = "BINARY"
              return <div style={{
                overflow: 'scroll', minWidth: '200px',
                marginLeft: '3px', marginRight: '3px'
              }}>{o}</div>
            }

            return <div style={{
              overflow: 'scroll', minWidth: '200px',
              marginLeft: '3px', marginRight: '3px'
            }}>{b}</div>
          },
          headerFormatter: (column, colIndex) => {
            return <div style={{
              textOverflow: 'ellipsis', overflow: 'hidden',
              marginLeft: '3px', marginRight: '3px'
            }}>{column.text}</div>
          },
        }

        columns.push(col)
      })
      txt.records.forEach(x => {
        let a = {}
        for (let i = 0; i < columns.length; i++) {

          a[columns[i].dataField] = x[i]
          console.log(x[i])
          let bcell = b64.base64DecToArr(x[i])
          let m = IsImage(bcell)
          console.log(m)
        }
        data.push(a)
      })
      setData(data)

    }
    /*    
        let mock = `{"columns":["id","name","primary_function","contractor","thrust","wingspan","length","height","weight","max_takeoff_weight","fuel_capacity","payload","speed","range_","ceiling","armament"],"records":[["67592d50-bbfe-402a-8781-94c9409033a7","F16","multirole fighter","Lockheed Martin","27000","33","50","16","19700","37500","7000","5x1258 bbmos, 7xPYD9, 1xXGL590, 1x7214surl_tnnxs","1500","2000","50000","xxx"]]}`
        processData( JSON.parse(mock))
        return false
    */

    //let jj = {database: selectedTable.database, schema: selectedTable.schema, table: selectedTable.table}
    if (selectedTable == null || selectedTable == undefined) {
      return
    }
    let jj = selectedTable.toJson()
    setSpinner(true)
    let body: string = jj
    http.sendToServer("POST", "/api/getselect",
      null, body,
      resp => {
        resp.json().then(txt => {

          processData(txt)
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

                    return <option key={i} value={i}>{x.connection + '_' + x.schema + '.' + x.table}</option>
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
        {data.length > 0 &&
          <div id="testtable" className="mb-5 mt-3">
            <ToolkitProvider
              condensed
              striped bootstrap4
              keyField={columns[0].dataField}
              columns={columns}

              id="scaledtable"
              data={data}

              columnToggle
            >
              {
                props => (
                  <div>
                    <ColumnToggle.ToggleList
                      contextual="success"
                      className="list-custom-class"
                      btnClassName="btn-test btn-sm"
                      {...props.columnToggleProps}
                    />
                    <hr />
                    <BootstrapTable
                      {...props.baseProps}
                    />
                  </div>
                )
              }
            </ToolkitProvider>

          </div>
      }

    </div>
    </div >
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