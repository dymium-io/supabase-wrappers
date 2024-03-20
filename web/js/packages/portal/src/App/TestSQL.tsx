import React, { useEffect, useState, useRef } from 'react';
import Tabs from 'react-bootstrap/Tabs'
import Tab from 'react-bootstrap/Tab'
import Form from 'react-bootstrap/Form'
import Button from 'react-bootstrap/Button'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import { Link } from "react-router-dom";
import BootstrapTable from 'react-bootstrap-table-next';
import paginationFactory from 'react-bootstrap-table2-paginator';
import ToolkitProvider, { ColumnToggle } from 'react-bootstrap-table2-toolkit';
import Offcanvas from '@dymium/common/Components/Offcanvas'
import Alert from 'react-bootstrap/Alert'
import Spinner from '@dymium/common/Components/Spinner'
import * as com from '../Common'
import * as types from '@dymium/common/Types/Internal'
import * as ctypes from '@dymium/common/Types/Common'
import * as stypes from '@dymium/common/Types/DbSyncCommon'
import * as http from '@dymium/common/Api/Http'
import { debugPort } from 'process';
import * as b64 from '../Utils/Base64'
import * as hex from '../Utils/Hex'



function getDatascopesForSQL(setSpinner, setAlert, setDatascopes, onSuccess) {
  setSpinner(true)
  http.sendToServer("GET", "/api/getdatascopesfortestsql",
    null, "",
    resp => {

      resp.json().then(js => {
        if (js.status !== "OK") {
          setAlert(
            <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
              Error retrieving datascopes: {js.errormessage} { }
            </Alert>
          )
        } else {
          setDatascopes(js.records)
          onSuccess(js.records)
        }
        setTimeout(() => setSpinner(false), 500)
      }).catch((error) => {
        setSpinner(false)
        setAlert(
          <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
            Error retrieving datascopes {error.message}
          </Alert>
        )
      })
    },
    resp => {
      console.log("on error")
      setSpinner(false)
      resp != null && resp.text().then(t =>
        setAlert(
          <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
            Error retrieving datascopes: {t}
          </Alert>)
      )
    },
    error => {
      console.log("on exception: " + error)
      setSpinner(false)
      setAlert(
        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
          Error retrieving datascopes {error.message}
        </Alert>
      )
    })
}

//const { ToggleList } = ColumnToggle;
//const ToggleList = ColumnToggle.ToggleList
type HeaderCell = {
  dataField: string,
  text: string,
  hidden: boolean,
  headerStyle: any,
  formatter: any,
  headerFormatter: any,
  isDummyField: boolean
}



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

  if (text[0] === '4'.charCodeAt(0) &&
    text[1] === '7'.charCodeAt(0) &&
    text[2] === '4'.charCodeAt(0) &&
    text[3] === '9'.charCodeAt(0) &&
    text[4] === '4'.charCodeAt(0) &&
    text[5] === '6'.charCodeAt(0) &&
    text[6] === '3'.charCodeAt(0) &&
    text[7] === '8'.charCodeAt(0) &&
    text[8] === '3'.charCodeAt(0) &&
    text[9] === '9'.charCodeAt(0) &&
    text[10] === '6'.charCodeAt(0) &&
    text[11] === '1'.charCodeAt(0)) {
    console.log("HEX GIF")
    return "hexgif"
  }
  if (text[0] === 'F'.charCodeAt(0) &&
    text[1] === 'F'.charCodeAt(0) &&
    text[2] === 'D'.charCodeAt(0) &&
    text[3] === '8'.charCodeAt(0)) {
    console.log("HEX JPEG")
    return "hexjpg"
  }
  // 89 50 4E 47
  if (text[0] === '8'.charCodeAt(0) &&
    text[1] === '9'.charCodeAt(0) &&
    text[2] === '5'.charCodeAt(0) &&
    text[3] === '0'.charCodeAt(0) &&
    text[4] === '4'.charCodeAt(0) &&
    text[5] === 'E'.charCodeAt(0) &&
    text[6] === '4'.charCodeAt(0) &&
    text[7] === '7'.charCodeAt(0)


  ) {
    console.log("HEX PNG")
    return "hexpng"
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
  const [txt, setTxt] = useState<stypes.SqlTestResult>(new stypes.SqlTestResult())
  const [showOffhelp, setShowOffhelp] = useState(com.isInstaller())
  const [columns, setColumns] = useState<HeaderCell[]>([])
  const [selectedTableIndex, setSelectedTableIndex] = useState(-1)
  
  useEffect(() => {

    getDatascopesForSQL(setSpinner, setAlert, setDatascopes, () => {

    })

  }, [])
  useEffect(() => {
    if (selectedDatascope !== null) {
      setSelectedTableIndex(-1)
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
        resp != null && resp.text().then(t =>
          setAlert(
            <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
              Error creating connection: {t}
            </Alert>
          ))

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
  useEffect(() => {
    let columns:HeaderCell[] = []
    let data: {}[] = []
    let js = txt
    let counter = 0
    js.columns.forEach(x => {

      let col: HeaderCell = {

        dataField: x,
        text: x,
        hidden: false,
        isDummyField: false,
        headerStyle: { minWidth: '200px' },
        formatter: (cell, row) => {

          if(cell === undefined) {
            return <div></div>
          }

          let b = cell.substring(1, cell.length)
          if (cell[0] === '1') {
            let o
            let bcell = b64.base64DecToArr(b)
            let m = IsImage(bcell)
            if (m !== "") {

              switch (m) {
                case "hexgif": {
                  let h = atob(b)
                  let newb = hex.HexStringToByteArray(h)
                  b = b64.base64EncArr(newb)
                  m = "gif"
                }
                  break
                case "hexpng": {
                  let h = atob(b)
                  let newb = hex.HexStringToByteArray(h)
                  b = b64.base64EncArr(newb)
                  m = "png"
                }
                  break
                case "hexjpg": {
                  let h = atob(b)
                  let newb = hex.HexStringToByteArray(h)
                  b = b64.base64EncArr(newb)
                  m = "jpeg"
                }
                  break
                default:
                  break
              }
              let src = `data:image/${m};base64,${b}`

              o = <img src={src} style={{ maxWidth: "250px" }}></img>
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
    let idcol = {
      dataField: "dymiumid412354",
      hidden: true,
      text: "",
      isDummyField: true,
      headerStyle: { minWidth: '200px' },
      formatter: (cell, row) => {
        return <div></div>
      },
      headerFormatter: (column, colIndex) => {
        return <div></div>
      }
      
    }
    columns.push(idcol)

    txt.records.forEach(x => {
      let a = {}
      for (let i = 0; i < columns.length - 1; i++) {
        a[columns[i].dataField] = x[i]
      }
      data.push(a)
    })
    setData(data)
    setColumns(columns)

  }, [txt])

  let getSelect = () => {
    let processData = (_txt: stypes.SqlTestResult) => {
      if(_txt.columns.length === 0) {
        setAlert(
          <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
            Empty result set returned. Possibly, the table is empty, or all the columns are blocked.
          </Alert>
        )        
        return
      }
      setTxt(_txt)
    }

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
          if(txt["status"] === "AuthError" || txt["status"] === "Error") {
            setAlert(
              <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                Error executing query: {txt["errormessage"]}
              </Alert>
            )
            setSpinner(false)
            return
          }
          let tr = stypes.SqlTestResult.fromJson(txt)

          processData(tr)
          setTimeout(() => setSpinner(false), 500)

        }).catch((error) => {
          setSpinner(false)
          setAlert(
            <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
              Error executing query: {error.message}
            </Alert>
          )

        })
      },
      resp => {
        setSpinner(false)
        resp != null && resp.text().then(t =>
          setAlert(
            <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
              Error executing query: {t}
            </Alert>
          ))

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
    setAlert(<></>)
    event.preventDefault();
    event.stopPropagation();
    return false
  }
  const CustomToggleList = ({
    columns,
    onColumnToggle,
    toggles
  }) => (
    <div className="btn-group btn-test  btn-sm btn-group-toggle btn-group-horizontal" style={{zIndex: "0"}} data-toggle="buttons">
      {
        columns.slice(0, -1)
          .map(column => ({
            ...column,
            toggle: toggles[column.dataField]
          }))
          .map(column => (
            <button
              type="button"
              key={ column.dataField }
              className={ `btn btn-test  btn-sm ${column.toggle ? 'active' : ''}` }
              data-toggle="button"
              aria-pressed={ column.toggle ? 'true' : 'false' }
              onClick={ () => onColumnToggle(column.dataField) }
            >
              { column.text }
            </button>
          ))
      }
    </div>
  );

  return (
    <div className=" text-left">
      {alert}
      <Offcanvas modal={false} width={300} show={showOffhelp} onClose={(e) => { setShowOffhelp(false) }}>
        <h5>Testing SQL</h5>
        <div className="mb-3">
          Here you can run small select queries with a limit of 20 records to test the work of Ghost Database, data access and transformation.
        </div>
        <div className="mb-3">
          Select ghost database, select a table, and hit Apply.
        </div>
        <div className="mb-3">
          This tool also demonstrates the namespace transformation from the original schemas and tables into the Ghost Database.
        </div>


        <div>

        </div>
      </Offcanvas>
      <h5 > Test data access and transformation <i onClick={e => { setShowOffhelp(!showOffhelp) }} className="trash fa-solid fa-circle-info mr-1"></i><Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>
      <div className=" text-left">
        <Form onSubmit={handleSubmit} noValidate >
          <Row>
            <Col xs="auto">
              <Form.Group className="mb-3" controlId="connection" >
                <Form.Label >Select Ghost Database</Form.Label>
                <Form.Control as="select" size="sm"
                  onChange={e => {

                    setSelectedDatascope(e.target.value)
                    //setSelectedTable(null)
                    //appDispatch(setSelectedDatascopeDefault(e.target.value))

                  }}
                  value={selectedDatascope}
                >
                  return <option value="">...</option>
                  {datascopes.sort( (a, b) => {
                    return (a.name > b.name) ? 1 : -1
                  }).map(x => {

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
                    setSelectedTableIndex(parseInt(e.target.value))
                    //appDispatch(setSelectedDatascopeDefault(e.target.value))

                  }}
                  value={selectedTableIndex}
                >
                  return <option key={-1} value={-1}>...</option>
                  {tables.sort( (x,y) => {
                    return (x.connection + '_' + x.schema + '.' + x.table > y.connection + '_' + y.schema + '.' + y.table) ? 1 : -1
                  }).map((x, i) => {

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

                <Button disabled={selectedTableIndex === -1} variant="dymium" size="sm" className="" type="submit">
                  Apply
                </Button>

              </div>
            }</Col>
          </Row>
        </Form>
        {data.length > 0 &&
          <div id="testtable" className="mb-5 mt-3">
            <ToolkitProvider
              columns={columns}
              id="scaledtable"
              data={data}
              keyField="dymiumid412354"
              columnToggle
            >
              {
                props => (
                  <div>
                    <CustomToggleList
                      contextual="success"
                      striped
                      btnClassName="btn-test btn-sm"
                      {...props.columnToggleProps}
                    />
                    <hr />
                    <BootstrapTable
                      keyField="dymiumid412354"
                      condensed
                      striped bootstrap4
                      {...props.baseProps}
                      hiddenColumns={['id']} 
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
    <Tabs id="tabs"

      unmountOnExit={true} className="mb-3 text-left">
      <Tab eventKey="test" id="test" title="Test SQL" className="mx-4">
        <Test />
      </Tab>


    </Tabs>
  )
}