import React, { Component } from 'react';
import Form from 'react-bootstrap/Form'
import Button from 'react-bootstrap/Button'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Offcanvas from '@dymium/common/Components/Offcanvas'
import * as types from '@dymium/common/Types/Common'
import * as http from '@dymium/common/Api/Http'
import * as com from '../Common'
import BootstrapTable from 'react-bootstrap-table-next';
import paginationFactory from 'react-bootstrap-table2-paginator';
import ToolkitProvider, { Search } from 'react-bootstrap-table2-toolkit';
import Spinner from '@dymium/common/Components/Spinner'
import Alert from 'react-bootstrap/Alert'
import { defaultDetectors, regexpDetectors } from "./Detectors"

import { v4 as uuidv4 } from 'uuid'
import { Link } from "react-router-dom";

let handlingOptions = () => {
  let opts: types.DataHandling[] = ["allow", "block", "obfuscate", "redact"]

  return opts.map(x => {
    return <option key={x} value={x}>
      {types.humanReadableDataHandling(x)}
    </option>
  })
}

const { SearchBar, ClearSearchButton } = Search;


const defaultSortedBy = [{
  dataField: "name",
  order: "asc"  // or desc
}];

type BuildState = {
  rules: any[],
  headers: any[],
  spinner: boolean,
  alert: JSX.Element,
  actions: types.DataAction[],
  validatedPII: boolean,
  validated: boolean,
  policy: types.DataPolicy,
  name: string,
  method: string,
  data: string,
  showOffhelp: boolean
}

type Rule = {
  index: number,
  id: string,
  name: string,
  method: string,
  data: string,
  actions: types.DataAction[],
}
// made this one a class to avoid excessive issues with stale captures in closures
// just too many useRefs
export default class BuildRulesClass extends Component {
  formpii = React.createRef<HTMLFormElement>()
  formp = React.createRef<HTMLFormElement>()
  state: BuildState = {
    rules: [],
    headers: [],
    spinner: false,
    alert: <></>,
    actions: [],
    validatedPII: false,
    validated: false,
    policy: new types.DataPolicy(),
    name: "",
    method: "columnregexp",
    data: "",
    showOffhelp: com.isInstaller()
  }

  counter = 0
  constructor(props) {
    super(props)

  }
  componentDidMount() {
    this.getPolicies()
  }
  columns = [
    {
      dataField: 'id',
      text: 'id',
      hidden: true,
    },

    {
      dataField: 'name',
      text: 'Name:',
      sort: true,
    },
    {
      dataField: 'method',
      text: 'Type:',
      /*
      isDummyField: true,
      */
      formatter: (cell, row, rowIndex, formatExtraData) => {
        if (row['method'] === 'comprehend')
          return <div>{types.humanReadablePIIDetectionType(row['method'])}</div>
        else
          return <Form.Control as="select" role="select" size="sm"
            id={"method_" + row["index"] + "_" + row["id"]}
            onChange={e => {
              let rules = this.state.rules.map((rule, indx) => {
                if (row["index"] !== rule.index) return rule

                rule = { ...rule }
                rule.method = e.target.value
                return rule
              })

              this.setState({ rules })
            }}

            value={row['method']}
          >
            <option value="columnregexp">Regexp for Table Columns</option>
            <option value="contentregexp">Regexp for Content</option>
          </Form.Control>
      },

      sort: true,
    },

    {
      dataField: 'data',
      text: 'Regexp:',
      sort: false,
      formatter: (cell, row, rowIndex, formatExtraData) => {
        if (row['method'] === 'comprehend')
          return <div></div>
        else
          return <input style={{ width: '40em' }} required pattern=".+"
            onChange={e => {
              let rule = this.state.rules[row["index"]]
              rule.data = e.target.value

              this.setState({ rules: this.state.rules })
            }}
            className="form-control form-control-sm"
            defaultValue={row['data']} >
          </input>
      }
    }
  ]

  getDefaults = () => {
    this.setState({ headers: this.columns })

    let det = [...defaultDetectors]
    let newa = det.concat(regexpDetectors)

    let piisuggestions = newa.map(x => {
      let y = types.PIISuggestor.fromJson({ actions: [], detector: x.detector })
      return y
    })
    
    let suggs = piisuggestions.map((x, ind) => {
      let out: Rule = {
        index: ind,
        id: x.detector.id == null ? "" : x.detector.id,
        name: x.detector.name,
        method: x.detector.method,
        data: x.detector.data,
        actions: []
      }
      if (this.state.policy != null) {
        for (let i = 0; i < this.state.policy.actions.length; i++) {
          out.actions[i] = types.DataAction.fromJson(this.state.policy.actions[i])
        }
      }

      return out
    })
    let policy = types.DataPolicy.fromJson(this.state.policy)
    policy.piisuggestions = suggs
    let headers = this.establishTabletStructure(policy, [...this.columns])

    this.setState({ rules: suggs, policy, headers })
  }
  onActionChange = e => {
    let ids = e.target.id.split('_')

    let index = parseInt(ids[1])
    let i = parseInt(ids[2])
    let rrules = this.state.rules.map((rule, ix) => {
      if (index !== ix) return rule

      rule = { ...rule }
      rule["actions"] = [...rule["actions"]]
      rule["actions"][i] = { ...rule["actions"][i] }
      rule["actions"][i].handling = e.target.value
      return rule
    })

    this.setState({ rules: rrules }, () => {
      this.forceUpdate()
    })
  }
  actionFormatter = i => {
    return (cell, row, rowIndex, formatExtraData) => {
      let index = row["index"]
      let rules = this.state.rules
      //console.log(row["index"], i, row["actions"][i])

      let sl = <Form.Control as="select" role="select" size="sm"
        id={"action_" + row["index"] + "_" + i + "_" + row["id"]}

        onChange={this.onActionChange}
        value={row["actions"][i].handling}
      >
        {handlingOptions()}
      </Form.Control>
      return sl
    }
  }
  establishTabletStructure = (policy: types.DataPolicy, headers) => {
    for (let i = 0; i < policy.actions.length; i++) {
      let c = {
        dataField: "action" + i,
        text: policy.actions[i].role,
        isDummyField: true,
        sort: false,
        formatter: this.actionFormatter(i)
      }
      headers.push(c)

    }
    let d = {
      dataField: "delete",
      text: "Delete",
      isDummyField: true,
      sort: false,
      style: (cell, row, rowIndex, colIndex) => {
        return { textAlign: "center" }

      },
      formatter: (cell, row, rowIndex, formatExtraData) => {

        if (row["method"] === "comprehend") return <></>

        return <i className="fas fa-trash ablue" onClick={
          e => {
            if (window.confirm("Are you sure you want to delete \n" + row["name"] + "\npolicy suggestion?")) {
              let rules = this.state.rules.filter(rule => {
                if (rule.id === row.id) {

                  return false
                }
                return true
              })
              this.setState({ rules })
            }
          }

        } id={"delete" + row["index"]} ></i>
      }
    }
    headers.push(d)
    return headers
  }
  getPolicies = () => {
    let error = <Alert variant="danger" onClose={() => this.setState({ alert: <></> })} dismissible>
      Error retrieving policy.
    </Alert>

    this.setState({ spinner: true })
    http.sendToServer("GET", "/api/getpolicies",
      null, "",
      resp => {
        resp.json().then(js => {
          this.setState({ spinner: false })
          if (js.error !== undefined) {
            this.getDefaults()
          } else {
            let prep = types.DataPolicy.fromJson(js)
            this.setState({ policy: prep })
            let headers = [...this.columns]
            headers = this.establishTabletStructure(prep, headers)

            this.setState({ headers })

            let suggs = JSON.parse(prep.toJson()).piisuggestions.map((x, ind) => {
              let out: Rule = {
                index: ind,
                id: x.detector.id,
                name: x.detector.name,
                method: x.detector.method,
                data: x.detector.data,
                actions: []
              }
              for (let i = 0; i < prep.actions.length; i++) {
                out.actions[i] = JSON.parse(prep.actions[i].toJson())
              }

              return out
            })
            this.setState({ rules: suggs })
          }
        }).catch((_error) => {
          this.setState({ alert: error })
          this.setState({ spinner: false })
        })
      },
      resp => {
        this.setState({ alert: error })
        this.setState({ spinner: false })
      },
      _error => {
        console.log("on exception")
        this.setState({ spinner: false })
        this.setState({ alert: error })

      })
  }
  savePolicy(newpolicy: types.DataPolicy) {
    let error = <Alert variant="danger" onClose={() => this.setState({ alert: <></> })} dismissible>
      Error saving policy.
    </Alert>
    this.setState({ spinner: true })
    let body = newpolicy.toJson()
    http.sendToServer("POST", "/api/savepolicies",
      null, body,
      resp => {
        resp.json().then(js => {
          this.setState({ spinner: false })
          this.setState({
            alert: <Alert variant="success" onClose={() => this.setState({ alert: <></> })} dismissible>
              Policy saved successfully
            </Alert>
          })

        }).catch((_error) => {
          this.setState({ alert: error })
          this.setState({ spinner: false })
        })
      },
      resp => {
        resp != null && resp.text().then(t =>
          this.setState({ alert: <Alert variant="danger" onClose={() => this.setState({ alert: <></> })} dismissible>{t}</Alert> })
        )
        this.setState({ spinner: false })
      },
      _error => {
        console.log("on exception")
        this.setState({ spinner: false })
        this.setState({ alert: error })
      })
  }
  addPII = (e) => {
    if (this.formpii.current == null) {
      return false
    }
    if (this.formpii.current.reportValidity() === false) {
      e.preventDefault();
      this.setState({ validatedPII: true })
      return false
    }
    this.setState({ validatedPII: false })
    e.preventDefault();
    e.stopPropagation();
    let newname = this.state.name.trim()
    for (let i = 0; i < this.state.rules.length; i++) {
      let rule = this.state.rules[i]
      if (rule.name === newname) {
        window.alert("Duplicate rule " + rule.name)
        e.preventDefault();
        e.stopPropagation();
        return false
      }
    }

    let rule = {
      id: uuidv4(),
      name: newname,
      method: this.state.method,
      data: this.state.data,
      actions: this.state.policy != null ? [...this.state.policy.actions] : []
    }

    let rules = this.state.rules.map(x => { return { ...x } })
    rules.push(rule)
    this.setState({ rules, name: "", data: "" })
    return false
  }

  handleSubmit = (e) => {
    if (this.formp.current != null && this.formp.current.reportValidity() === false) {
      e.preventDefault();
      this.setState({ validated: true })
      return false
    }
    let names = {}
    for (let i = 0; i < this.state.rules.length; i++) {
      let rule = this.state.rules[i]
      if (names[rule.name] !== undefined) {
        window.alert("Duplicate rule " + rule.name)
        e.preventDefault();
        e.stopPropagation();
        return false
      }
      names[rule.name] = 1
      if (rule.data === "") {
        window.alert("One or more of Rule regexps are empty")
        e.preventDefault();
        e.stopPropagation();
        return false
      }
      if (rule.name === "") {
        window.alert("One or more of Rule names are empty")
        e.preventDefault();
        e.stopPropagation();
        return false
      }
    }
    // 
    let newpolicy: types.DataPolicy = new types.DataPolicy()
    if (this.state.policy != null) {
      newpolicy.actions = [...this.state.policy.actions]
    }

    for (let i = 0; i < this.state.rules.length; i++) {
      let pii: types.PIISuggestor = new types.PIISuggestor();
      pii.actions = this.state.rules[i].actions
      pii.detector.data = this.state.rules[i].data
      pii.detector.name = this.state.rules[i].name
      pii.detector.id = this.state.rules[i].id
      pii.detector.method = this.state.rules[i].method

      newpolicy.piisuggestions.push(pii)

    }

    this.savePolicy(newpolicy)

    e.preventDefault();
    e.stopPropagation();
    return false
  }

  resetDefaults = () => {
    if (window.confirm("Are you sure you want to reset defaults?")) {
      this.getDefaults()
    }
    return true
  }
  render() {
    return (
      <>
        <h5 >Edit Policy Suggestions  <i onClick={e => { this.setState({ showOffhelp: !this.state.showOffhelp }) }} className="trash fa-solid fa-circle-info mr-1"></i><Spinner show={this.state.spinner} style={{ width: '28px' }}></Spinner></h5>
        <Offcanvas modal={false} width={300} show={this.state.showOffhelp} onClose={(e) => { this.setState({ showOffhelp: false }) }}>
          <h5>Policy Suggestions</h5>
          <div className="mb-3">
            This page allows to craft the policy suggestions for table access in Ghost Database interface.
          </div>
          <div className="mb-3">
            The access levels must be pre-defined in the Access Levels tab.
          </div>

          <div className="mb-3">
            There are three ways to detect PII content:
            <ul>
              <li>Amazon Comprehend</li>
              <li>Regexp on the data content in the column (using a subsample)</li>
              <li>Regexp on the column names</li>
            </ul>
            The latter two types can be used to extend the set of detectable PIIs.
          </div>

          <div className="mb-3">

          </div>

        </Offcanvas>
        <Form ref={this.formpii} onSubmit={this.addPII} noValidate validated={this.state.validatedPII}>
          <Row >
            <Col xs="auto">
              <Form.Group className="mb-3" controlId="dname">
                <Form.Label>Name:</Form.Label>
                <Form.Control size="sm" type="text" placeholder="alpha_num, small caps"
                  required
                  pattern="[\w '&%]+"
                  value={this.state.name}
                  onChange={e => this.setState({ name: e.target.value })}
                />
                <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                <Form.Control.Feedback type="invalid" >
                  Type systemwide unique name to use in SQL
                </Form.Control.Feedback>
              </Form.Group>
            </Col>
            <Col xs="auto">
              <Form.Group className="mb-3" controlId="daction">
                <Form.Label>Type:</Form.Label>
                <Form.Control as="select" required size="sm"
                  value={this.state.method}
                  onChange={e => {
                    this.setState({ method: e.target.value })
                  }}
                >


                  <option value="columnregexp">Regexp for Table Columns</option>
                  <option value="contentregexp">Regexp for Content</option>



                </Form.Control>
              </Form.Group>
            </Col>
            <Col xs="auto">
              <Form.Group className="mb-3" controlId="dname">
                <Form.Label>Regular Expression</Form.Label>
                <Form.Control size="sm" type="text" placeholder="Regexp"
                  style={{ width: '40em' }}
                  required
                  pattern=".+"
                  value={this.state.data}
                  onChange={e => this.setState({ data: e.target.value })}
                />
                <Form.Control.Feedback >Looks good!</Form.Control.Feedback>
                <Form.Control.Feedback type="invalid" >
                  Type a regular expression
                </Form.Control.Feedback>
              </Form.Group>
            </Col>
            <Col xs="auto">
              <Button style={{ position: 'relative', top: '0.16em' }} variant="dymium" size="sm" className="mt-4" type="submit">Add</Button>
            </Col>
          </Row>
        </Form>
        {this.state.alert}
        {this.state.rules.length > 0 && this.state.headers.length > 0 &&
          <Form ref={this.formp} onSubmit={this.handleSubmit} noValidate validated={this.state.validated}>

            <div id="tablecontainer" style={{ width: '90%' }} className="text-center mb-5">

              <ToolkitProvider
                bootstrap4
                keyField='id'
                data={this.state.rules}
                columns={this.state.headers}
                search >
                {
                  props => (<div className="text-left">

                    <div className="d-flex">


                      <div style={{ marginLeft: "auto" }}>
                        <Button onClick={this.resetDefaults} variant="dymium" size="sm" className="mr-4 ">Reset defaults</Button>
                        <SearchBar size="sm" {...props.searchProps} />
                        <ClearSearchButton {...props.searchProps} />

                      </div>
                    </div>
                    <div className="d-block">
                      <BootstrapTable id="scaledtable"
                        condensed
                        key="id"
                        striped bootstrap4 bordered={false}

                        pagination={paginationFactory()}
                        defaultSorted={defaultSortedBy}

                        {...props.baseProps}
                      />
                    </div>
                  </div>)
                }
              </ToolkitProvider>
            </div>
            <Row className="mt-5">
              <Col xs="auto">
                <Button size="sm" variant="dymium" type="submit">Apply</Button>
              </Col>
            </Row>

          </Form>
        }
      </>
    )
  }
}
