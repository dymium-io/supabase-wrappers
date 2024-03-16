import React, { Component, useEffect, useState, useRef } from 'react';

import Form from 'react-bootstrap/Form'
import Button from 'react-bootstrap/Button'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import { DndContext, closestCenter, KeyboardSensor, PointerSensor, useSensor, useSensors, } from '@dnd-kit/core';
import { arrayMove, SortableContext, sortableKeyboardCoordinates, verticalListSortingStrategy, } from '@dnd-kit/sortable';

import { useSortable } from '@dnd-kit/sortable';
import { CSS } from '@dnd-kit/utilities';

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

export function SortableItem({ id, value, handling, onHandlingChange, onDelete, handlingOptions }) {
  const {
    attributes,
    listeners,
    setNodeRef,
    transform,
    transition,
  } = useSortable({ id });

  debugger
  const style = {
    transform: CSS.Transform.toString(transform),
    transition,
  };
  let onclick = () => {
    onDelete(id)
  }

  return (
    <li ref={setNodeRef} style={style} {...attributes} {...listeners} className="card licard">
      <Row>
        <Col><div style={{ marginTop: '4px' }}>{value}</div></Col>
        <Col xs="auto">
          <select onChange={onHandlingChange} value={handling} className="form-control form-control-sm mt-1">
            {handlingOptions}
          </select>
        </Col>
        <Col xs="auto">
          <Button variant="outline"  id={"deletebutton" + id}   onPointerDown={onclick}>
            <i className="fas fa-trash ablue" aria-label={"delete" + id} id={"delete" + id} ></i>
          </Button>
        </Col>
      </Row>
    </li>
  );
}

export interface ExtendedDataAction {
  id: string; // Using string for ID for consistency with dnd-kit requirements
  role: string,
  index: number,
  handling: string
}

export default function AccessLevels() {
  const [spinner, setSpinner] = useState(false)
  const [alert, setAlert] = useState<JSX.Element>(<></>)

  const [name, setName] = useState("")
  const [level, setLevel] = useState("allow")
  const [actions, setActions] = useState<ExtendedDataAction[]>([])
  const [showOffhelp, setShowOffhelp] = useState(com.isInstaller())

  let policy = useRef(new types.DataPolicy())
  let errorGet = <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
    Error retrieving policy.
  </Alert>
  let savePolicies = () => {

    let _actions = [...actions]
    for (let i = 0; i < _actions.length; i++) {
      _actions[i].index = i
      delete _actions["id"]
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
  useEffect(() => {
    console.log(actions);
  }, [actions]);

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
            js.actions = js.actions.map((action, index) => ({
              role: action.role,
              index: action.index !== null ? action.index : 0, // Assign a default value of 0 if index is null
              handling: action.handling,
            }));
            let prep = types.DataPolicy.fromJson(js)
            if (js.piisuggestions.length === 0) {
              initializePolicy()
            } else {
              policy.current = prep
            }
            let extendedActions: ExtendedDataAction[] = prep.actions.map((action, index) => ({
              role: action.role,
              index: action.index !== null ? action.index : 0, // Assign a default value of 0 if index is null
              handling: action.handling,
              id: index.toString(), // Assigning index as id for simplicity
            }));

            setActions(extendedActions); // Update state with actions including ids
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


  const sensors = useSensors(
    useSensor(PointerSensor),
    useSensor(KeyboardSensor, {
      coordinateGetter: sortableKeyboardCoordinates,
    })
  );

  const onDragEnd = (event) => {
    const { active, over } = event;

    if (over && active.id !== over.id) {
      const oldIndex = actions.findIndex(action => action.id === active.id);
      const newIndex = actions.findIndex(action => action.id === over.id);

      setActions((currentItems) => arrayMove(currentItems, oldIndex, newIndex));
    }
  };


  let addLevel = event => {
    //let x: types.DataHandling = 'allow <*> Allow'
    let v: ExtendedDataAction = {} as ExtendedDataAction
    v.role = name;
    v.index = actions.length
    v.id = actions.length.toString()

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
    for (let i = 0; i < actions.length; i++) {
      if (actions[i].role === v.role) {
        setAlert(
          <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>Access level already exists</Alert>
        )
        event.preventDefault();
        //setValidated(false)
        event.stopPropagation();
        setName("")
        return
      }
    }
    debugger
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

  const handleActionChange = (newHandling, id) => {
    debugger
    const updatedActions = actions.map(action =>
      action.id === id ? { ...action, handling: newHandling } : action
    );
    setActions(updatedActions);
  };

  const handleActionDelete = (id) => {
    debugger
    const filteredActions = actions.filter(action => action.id !== id);
    setActions(filteredActions);
  };

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

            <DndContext sensors={sensors} collisionDetection={closestCenter} onDragEnd={onDragEnd}>
              <SortableContext items={actions.map(action => action.id)} strategy={verticalListSortingStrategy}>



                {actions.map((action, index) => (
                  <SortableItem
                    key={action.id}
                    id={action.id}
                    value={action.role}
                    handling={action.handling}
                    onHandlingChange={(e) => handleActionChange(e.target.value, action.id)}
                    onDelete={() => handleActionDelete(action.id)}
                    handlingOptions={handlingOptions()}
                  />
                ))}
              </SortableContext>
            </DndContext>
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
