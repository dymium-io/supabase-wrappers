import React, { Component, useEffect, useState, useRef } from 'react';

import Form from 'react-bootstrap/Form'
import Button from 'react-bootstrap/Button'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import { SortableContainer, SortableElement, SortableContainerProps, SortableElementProps, arrayMove } from 'react-sortable-hoc';

import Offcanvas from '@dymium/common/Components/Offcanvas'

import 'react-bootstrap-table-next/dist/react-bootstrap-table2.min.css';
import Spinner from '@dymium/common/Components/Spinner'
import Alert from 'react-bootstrap/Alert'
import * as types from '@dymium/common/Types/Common'
import * as http from '@dymium/common/Api/Http'
import * as com from '../Common'

import { defaultDetectors, regexpDetectors } from "./Detectors"


let handlingOptions = () => {
  let opts: types.DataHandling[] = ["allow", "block", "obfuscate", "redact"]

  return opts.map(x => {
    return <option key={x} value={x}>
      {types.humanReadableDataHandling(x)}
    </option>
  })
}

export default function AccessLevels() {
  const [spinner, setSpinner] = useState(false)
  const [alert, setAlert] = useState<JSX.Element>(<></>)

  const [name, setName] = useState("")
  const [level, setLevel] = useState("")
  const [actions, setActions] = useState<types.DataAction[]>([])
  const [showOffhelp, setShowOffhelp] = useState(com.isInstaller())

  let policy = useRef(new types.DataPolicy())
  let errorGet = <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
    Error retrieving policy.
  </Alert>
  let savePolicies = () => {

    for (let i = 0; i < actions.length; i++) {
      actions[i].index = i
    }
    policy.current.actions = actions

    let js = JSON.parse(policy.current.toJson())
    // do cleannup
    for (let i = 0; i < js.piisuggestions.length; i++) {
      let sactions = js.piisuggestions[i].actions
      let gactions = js.actions
      let out: types.DataAction[] = []
      let getAction = l => {
        for (let i = 0; i < sactions.length; i++) {
          if (sactions[i].role === l.role)
            return sactions[i]
        }
        return l
      }
      for (let j = 0; j < gactions.length; j++) {
        let l = getAction(gactions[j])
        out.push(l)
      }
      js.piisuggestions[i].actions = out

    }
    let body = JSON.stringify(js)
    setSpinner(true)
    http.sendToServer("POST", "/api/savepolicies",
      null, body,
      resp => {
        resp.json().then(js => {
          setSpinner(false)
          if (js.status !== "OK") {

            getPolicies()
            setAlert(
              <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>{js.error}</Alert>
            )
          } else {
            setAlert(
              <Alert variant="success" onClose={() => setAlert(<></>)} dismissible>Access levels saved successfully</Alert>
            )
            policy.current = types.DataPolicy.fromJson(js)

          }
        }).catch((error) => {
          setAlert(
            <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>{error}</Alert>
          )
          setSpinner(false)
        })
      },
      resp => {
        setSpinner(false)
        resp != null && resp.text().then(t =>
          setAlert(
            <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>Error retrieving policy: {t}</Alert>
          ))
        setSpinner(false)
      },
      error => {
        console.log("on exception")
        setSpinner(false)
        setAlert(
          <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>{error.message}</Alert>

        )
        setSpinner(false)
      })
  }
  let initializePolicy = () => {
    policy.current = new types.DataPolicy()

    let det = [...defaultDetectors]
    let newa = det.concat(regexpDetectors)

    let prep = newa.map(x => {
      let y = types.PIISuggestor.fromJson({ actions: [], detector: x.detector })
      return y
    })
    policy.current.piisuggestions = prep
  }
  let getPolicies = () => {
    let error = <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
      Error retrieving policy.
    </Alert>
    setSpinner(true)
    http.sendToServer("GET", "/api/getpolicies",
      null, "",
      resp => {
        resp.json().then(js => {
          setSpinner(false)

          if (js.error !== undefined) {
            // need to initialize the whole iguana
            initializePolicy()

          } else {
            let prep = types.DataPolicy.fromJson(js)
            if (js.piisuggestions.length === 0) {
              initializePolicy()
            } else {
              policy.current = prep
            }
            setActions(prep.actions)
          }
        }).catch((error) => {
          setAlert(
            <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>{error.message}</Alert>

          )
          setSpinner(false)
        })
      },
      resp => {
        setSpinner(false)
        resp != null && resp.text().then(t =>
          setAlert(
            <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>getpolicies failed: {t}</Alert>

          ))
        setSpinner(false)
      },
      error => {
        console.log("on exception")
        setAlert(
          <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>{error.message}</Alert>
        )
        setSpinner(false)
      })
  }

  useEffect(() => {
    getPolicies()
  }, [])

  let onDelete = index => {
    return e => {
      let v = actions.splice(index, 1)
      setActions(v)
    }
  }
  const SortableItem: React.ComponentClass<SortableElementProps & { value: string, iindex: number }, any> = SortableElement(({ value, iindex }: { value: string, iindex: number }) => {
    let onDelete = e => {
      actions.splice(iindex, 1)
      setActions([...actions])
    }
    let onChange = e => {
      actions[iindex].handling = e.target.value
      setActions([...actions])
    }
    return <li className="card licard">
      <Row>
        <Col><div style={{ marginTop: '4px' }}>
          {value}</div>
        </Col>
        <Col xs="auto">

          <select onChange={onChange} style={{ marginTop: '4px' }} value={actions[iindex].handling} className="form-control form-control-sm">
            {handlingOptions()}
          </select>
        </Col>
        <Col xs="auto">
          <Button variant="outline" onClick={onDelete} ><i className="fas fa-trash ablue" aria-label={"delete" + iindex} id={"delete" + iindex} ></i></Button>
        </Col>
      </Row>

    </li>
  }
  );

  const SortableList: React.ComponentClass<SortableContainerProps & { items: types.DataAction[] }, any> = SortableContainer(({ items }: { items: types.DataAction[] }) => {
    return (
      <ul style={{ listStyle: 'none', width: '50%' }} className="liouter">
        {items.map((value: types.DataAction, index: number) => (
          <SortableItem key={`item-${index}`} index={index} iindex={index} value={value.role} />
        ))}
      </ul>
    );
  });


  let addLevel = event => {
    //let x: types.DataHandling = 'allow <*> Allow'
    let v: types.DataAction = new types.DataAction
    v.role = name

    switch (level) {
      case 'allow':
        v.handling = 'allow'
        break
      case 'block':
        v.handling = 'block'
        break
      case 'obfuscate':
        v.handling = 'obfuscate'
        break
      case 'redact':
        v.handling = 'redact'
        break
      default:
        v.handling = 'block'
        break
    }
    actions.push(v)
    setActions([...actions])

    event.preventDefault();
    //setValidated(false)
    event.stopPropagation();
    setName("")
  }

  let onSortEnd = ({ oldIndex, newIndex }) => {
    var a = arrayMove(actions, oldIndex, newIndex)

    setActions(a)
  }

  let handleSubmit = event => {
    event.preventDefault();
    //setValidated(false)
    event.stopPropagation();
    savePolicies()
  }

  return <div>
    <h5 >Define Access Levels <i onClick={e => { setShowOffhelp(!showOffhelp) }} className="trash fa-solid fa-circle-info mr-1"></i> <Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>
    <Offcanvas modal={false} width={300} show={showOffhelp} onClose={(e) => { setShowOffhelp(false) }}>
      <h5>Access Levels</h5>
      <div className="mb-3">
        Access levels are the templates for access rights when you define the Ghost Databases. For example, you can define levels
        <ul>
          <li>
            Administrator
          </li>
          <li>
            Data Engineer
          </li>
          <li>
            Data Scientist
          </li>
        </ul>

      </div>
      <div className="mb-3">
        Once you defined the levels, go to the next tab Rules, and set the suggestion for data handling for various PIIs
      </div>



    </Offcanvas>
    {alert}
    <div>
      <Form onSubmit={addLevel}>
        <Row >
          <Col xs="auto">
            <Form.Group className="mb-3" controlId="dname">
              <Form.Label>Name:</Form.Label>
              <Form.Control size="sm" type="text" placeholder="alpha_num, small caps"
                required
                pattern="[\w '&%]+"
                value={name}
                onChange={e => setName(e.target.value)}
              />
              <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
              <Form.Control.Feedback type="invalid" >
                Type systemwide unique name to use in SQL
              </Form.Control.Feedback>
            </Form.Group>
          </Col>
          <Col xs="auto">
            <Form.Group className="mb-3" controlId="daction">
              <Form.Label>Default Action:</Form.Label>
              <Form.Control as="select" required size="sm"
                value={level}
                onChange={e => {
                  setLevel(e.target.value)
                }}
              >
                {handlingOptions()}

              </Form.Control>
            </Form.Group>
          </Col>
          <Col xs="auto">
            <Button style={{ position: 'relative', top: '0.16em' }} variant="dymium" size="sm" className="mt-4" type="submit">Add</Button>
          </Col>
        </Row>
      </Form>
      <Form onSubmit={handleSubmit}>

        <>
          {actions.length > 0 &&
            <SortableList distance={1} items={actions} onSortEnd={onSortEnd} />
          }
          <Row className="mt-5">
            <Col xs="auto">
              <Button size="sm" variant="dymium" type="submit">Apply</Button>
            </Col>
          </Row>
        </>


      </Form>
    </div>
  </div>
}
