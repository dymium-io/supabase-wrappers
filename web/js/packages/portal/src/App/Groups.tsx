import React, { useEffect, useState, useRef } from 'react';
import Tabs from 'react-bootstrap/Tabs'
import Tab from 'react-bootstrap/Tab'
import Form from 'react-bootstrap/Form'
import Button from 'react-bootstrap/Button'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'
import Offcanvas from '@dymium/common/Components/Offcanvas'
import { Typeahead } from 'react-bootstrap-typeahead';
import Modal from 'react-bootstrap/Modal'
import Alert from 'react-bootstrap/Alert'
import BootstrapTable from 'react-bootstrap-table-next';
import paginationFactory from 'react-bootstrap-table2-paginator';
import ToolkitProvider, { Search } from 'react-bootstrap-table2-toolkit/dist/react-bootstrap-table2-toolkit';
import 'react-bootstrap-table-next/dist/react-bootstrap-table2.min.css';
import Multiselect from 'multiselect-react-dropdown';
import Spinner from '@dymium/common/Components/Spinner'
import cloneDeep from 'lodash/cloneDeep';
import * as com from '../Common'
import * as types from '@dymium/common/Types/Internal'
import * as http from '../Api/Http'
import { useInitialize } from '../Utils/CustomHooks'
import { useAppDispatch, useAppSelector } from './hooks'
import { setActiveGroupsTab } from '../Slices/menuSlice'

const { SearchBar, ClearSearchButton } = Search;

function GroupMapping() {
  const [validated, setValidated] = useState(false)
  let form = useRef<HTMLFormElement>(null)
  const [show, setShow] = useState(false)
  const [showdelete, setShowdelete] = useState(false)
  const [selectedId, setSelectedId] = useState(0)
  const [selectedName, setSelectedName] = useState("")
  const [comments, setComments] = useState("")
  const [directorygroup, setDirectorygroup] = useState("")
  const [dymiumgroup, setDymiumgroup] = useState("")
  const [adminaccess, setAdminaccess] = useState(false)
  const [spinner, setSpinner] = useState(false)
  const [id, setId] = useState("")
  const [alert, setAlert] = useState<JSX.Element>(<></>)
  const [mappings, setMappings] = useState<types.Mapping[]>([])
  let AddMapping = () => {
    setShow(false)
  }
  let getMappings = () => {
    setSpinner(true)
    http.sendToServer("GET", "/api/getmappings",
      null, "",
      resp => {
        resp.json().then(js => {

          setMappings(js.records)
          setSpinner(false)
          setShow(false)
        }).catch((error) => {

        })
      },
      resp => {
        setSpinner(false)
        setAlert(
          <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
            Error retrieving mapping.
          </Alert>
        )
        setShow(false)
      },
      error => {
        console.log("on exception")
        setSpinner(false)
        setAlert(
          <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
            Error retrieving mapping.
          </Alert>
        )
        setShow(false)
      })

  }

  let sendMapping = () => {
    setSpinner(true)
    let body = JSON.stringify({ dymiumgroup, directorygroup, comments, adminaccess })
    http.sendToServer("POST", "/api/createmapping",
      null, body,
      resp => {
        resp.json().then(js => {
          if (js.status == "OK") {

            getMappings()
            setAlert(
              <Alert variant="success" dismissible onClose={() => setAlert(<></>)} >
                Mapping created successfully!
              </Alert>
            )
            setShow(false)
          } else {
            setAlert(
              < Alert variant="danger" onClose={() => setAlert(<></>)} dismissible >
                Error: {js.errormessage} !
              </Alert >)
          }
          setSpinner(false)
          setShow(false)
        }).catch((error) => {
          setSpinner(false)
          setShow(false)
        })
      },
      resp => {
        setSpinner(false)
        setAlert(
          <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
            Error creating mapping.
          </Alert>
        )
        setShow(false)
      },
      error => {
        console.log("on exception")
        setSpinner(false)
        setShow(false)
        setAlert(
          <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
            Error creating mapping.
          </Alert>
        )
      })
  }

  let updateMapping = () => {
    setSpinner(true)
    
    let body = JSON.stringify({ id, dymiumgroup, directorygroup, comments, adminaccess })
    http.sendToServer("POST", "/api/updatemapping",
      null, body,
      resp => {
        resp.json().then(js => {
          if (js.status == "OK") {

            getMappings()
            setAlert(
              <Alert variant="success" dismissible onClose={() => setAlert(<></>)} >
                Mapping updated successfully!
              </Alert>
            )
          } else {
            setAlert(
              < Alert variant="danger" onClose={() => setAlert(<></>)} dismissible >
                Error: {js.errormessage} !
              </Alert >)
          }
          setSpinner(false)
          setShow(false)
        }).catch((error) => {

        })
      },
      resp => {
        setSpinner(false)
        setAlert(
          <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
            Error creating mapping.
          </Alert>
        )
        setShow(false)
      },
      error => {
        console.log("on exception")
        setSpinner(false)
        setShow(false)
        setAlert(
          <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
            Error creating mapping.
          </Alert>
        )
      })
  }

  let deleteMapping = () => {
    setSpinner(true)
    let body = JSON.stringify({ id: selectedId })
    http.sendToServer("POST", "/api/deletemapping",
      null, body,
      resp => {
        resp.json().then(js => {
          if (js.status == "OK") {

            getMappings()
            setAlert(
              <Alert variant="success" dismissible onClose={() => setAlert(<></>)} >
                Mapping deleted successfully!
              </Alert>
            )
          } else {
            setAlert(
              < Alert variant="danger" onClose={() => setAlert(<></>)} dismissible >
                Error deleting mapping: {js.errormessage} !
              </Alert >)
          }
          setSpinner(false)
          setShow(false)
        }).catch((error) => {

        })
      },
      resp => {
        setSpinner(false)
        setAlert(
          <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
            Error deleting mapping.
          </Alert>
        )
        setShow(false)
      },
      error => {
        console.log("on exception")
        setSpinner(false)
        setShow(false)
        setAlert(
          <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
            Error deleting mapping.
          </Alert>
        )
      })
  }

  useEffect(() => {
    getMappings()
  }, [])

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
    if (id === "")
      sendMapping()
    else
      updateMapping()

    setComments("")
    setId("")
    setDymiumgroup("")
    setAdminaccess(false)
    setDirectorygroup("")
    setValidated(false)

    return false
  }
  let onEdit = (id, dymiumgroup, directorygroup, comments, adminaccess) => {

    return e => {

      setDirectorygroup(directorygroup)
      setId(id)
      setComments(comments)
      setDymiumgroup(dymiumgroup)
      setAdminaccess(adminaccess)
      setValidated(false)
      setShow(true)
    }
  }
  let onDelete = (id, dymiumgroup, directorygroup, comments) => {
    return e => {
      setSelectedId(id)
      setSelectedName(directorygroup + " => " + dymiumgroup)
      setShowdelete(true)
    }
  }
  let columns = [
    {
      dataField: 'id',
      text: 'id',
      hidden: true,
    },
    {
      dataField: 'directorygroup',
      text: 'Directory group:',
      sort: true,
    },
    {
      dataField: 'dymiumgroup',
      text: 'Dymium group',
      sort: true,
    },


    {
      dataField: 'comments',
      text: 'Description:',
      //headerStyle: { width: '100%' },
      sort: true
    },
    {
      text: 'Admin Role',
      dataField: 'adminaccess',

      formatter: (cell, row, rowIndex, formatExtraData) => {
        if(row["adminaccess"] ) {
          return <i className="fas fa-check blue" ></i>
        } else {
          return <></>
        }
      },
      //formatExtraData: { hoverIdx: this.state.hoverIdx },
      headerStyle: { width: '140px' },
      style: { height: '30px' },
      align: 'center'
    },    {
      text: 'Edit',
      dataField: 'edit',
      isDummyField: true,
      formatter: (cell, row, rowIndex, formatExtraData) => {

        return <i className="fas fa-edit ablue" onClick={onEdit(row["id"],
          row["dymiumgroup"], row["directorygroup"], row["comments"], row["adminaccess"])} role="button"></i>
      },
      //formatExtraData: { hoverIdx: this.state.hoverIdx },
      headerStyle: { width: '50px' },
      style: { height: '30px' },
      align: 'center'
    },
    {
      text: 'Delete',
      dataField: 'delete',
      isDummyField: true,
      formatter: (cell, row, rowIndex, formatExtraData) => {
        return <i className="fas fa-trash ablue" onClick={onDelete(row["id"],
          row["dymiumgroup"], row["directorygroup"], row["comments"])} role="button"></i>
      },
      //formatExtraData: { hoverIdx: this.state.hoverIdx },
      headerStyle: { width: '90px' },
      style: { height: '30px' },
      align: 'center'
    }
  ]

  return (
    <>
      <h5 > Group mapping from your directory to Dymium<Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>

      <Modal size="lg" centered show={show} onHide={() => setShow(false)} >
        <Modal.Header closeButton>
          <Modal.Title>{id === "" ? "Add Mapping" : "Update Mapping"}</Modal.Title>
        </Modal.Header>
        <Form onSubmit={handleSubmit} ref={form} noValidate validated={validated}>

          <Modal.Body>

            <Row>
              <Col xs="auto">
                <Form.Group className="mb-3" controlId="dbname">
                  <Form.Label>Directory group:</Form.Label>
                  <Form.Control size="sm" type="text" placeholder="alphanumeric"
                    required
                    pattern=".+"
                    value={directorygroup}
                    onChange={e => {
                      setDirectorygroup(e.target.value)
                    }}
                  />
                  <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                  <Form.Control.Feedback type="invalid" >
                    Directory group
                  </Form.Control.Feedback>
                </Form.Group>

              </Col>
              <Col xs="auto" style={{ marginTop: '1.7em' }} >=&gt;
              </Col>
              <Col xs="auto">
                <Form.Group className="mb-3" controlId="dbname">
                  <Form.Label>Dymium group:</Form.Label>
                  <Form.Control size="sm" type="text" placeholder="alphanumeric"
                    required
                    pattern=".+"
                    value={dymiumgroup}
                    onChange={e => {
                      setDymiumgroup(e.target.value)
                    }}
                  />
                  <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                  <Form.Control.Feedback type="invalid" >
                    Dymium group
                  </Form.Control.Feedback>
                </Form.Group>

              </Col>
                <Col  style={{ marginTop: '1.7em' }}>

                <Form.Check
                                style={{ marginTop: '0.2em' }}
                                type="checkbox"
                                label="Admin access"
                                id="adminaccess"
                                defaultChecked={adminaccess}
                                onChange={e => {
                                   setAdminaccess(e.target.checked)
                                }}
                            />

                </Col>
            </Row>
            <Row>
              <Col>
                <Form.Group className="mb-3" controlId="sql">
                  <Form.Label>Comments</Form.Label>
                  <Form.Control
                    required
                    value={comments}
                    as="textarea" onChange={e => { setComments(e.target.value) }} rows={3} />
                </Form.Group>
              </Col>
            </Row>
          </Modal.Body>
          <Modal.Footer>
            <Button variant="dymium" type="submit"
            >{id === "" ? "Add" : "Update"}</Button> <Button variant="dymium" onClick={() => {

              setComments("")
              setId("")
              setDymiumgroup("")
              setDirectorygroup("")
              setValidated(false)
              setShow(false)
            }}>Cancel</Button>
          </Modal.Footer>
        </Form>
      </Modal>
      <Modal centered show={showdelete} onHide={() => setShowdelete(false)} >
        <Modal.Header closeButton>
          <Modal.Title>Delete mapping?</Modal.Title>
        </Modal.Header>
        <Modal.Body>Are you sure you want to remove the mapping {selectedName}? This operation is irreversible.</Modal.Body>
        <Modal.Footer>
          <Button variant="danger" size="sm" onClick={() => {
            deleteMapping()
            setShowdelete(false)
          }
          }>Delete</Button> <Button size="sm" variant="dymium" onClick={() => {
            setShowdelete(false)

          }}>Cancel</Button>
        </Modal.Footer>
      </Modal>

      {mappings.length > 0 &&
        <ToolkitProvider

          bootstrap4
          keyField='Dymiumgroup'
          data={mappings}
          columns={columns}
          search >
          {
            props => (
              <div className="text-left mt-0 pt-0">
                {alert}
                <div className="d-flex">

                  <div style={{ marginLeft: "auto" }}>
                    <SearchBar size="sm" {...props.searchProps} />
                    <ClearSearchButton {...props.searchProps} />
                    <i onClick={e=>getMappings()} className="fa fa-refresh ablue cursor-pointer" style={{position: 'relative', top: '2px'}} aria-hidden="true"></i>

                  </div>
                </div>
                <div className="d-block">
                  <BootstrapTable id="scaledtable"
                    size="sm"
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


      }
      <Button className="mt-4" variant="dymium" onClick={e => {
        setDirectorygroup("")
        setId("")
        setComments("")
        setDymiumgroup("")
        setValidated(false)

        setShow(true)
      }} size="sm">Add mapping</Button>
    </>
  )
}



export default function Groups() {
  const t = useAppSelector((state) => {

    return state.reducer.activeGroupsTab
  }
  )
  const appDispatch = useAppDispatch()

  return (
    <Tabs
      defaultActiveKey={t} id="groups"
      onSelect={(k) => appDispatch(setActiveGroupsTab(k))}

      unmountOnExit={true} className="mb-3 text-left">
      <Tab eventKey="groups" title="Group Mapping" className="mx-4">
        <GroupMapping />
      </Tab>

    </Tabs>

  )

}
