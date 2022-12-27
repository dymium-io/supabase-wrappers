import React, { useEffect, useState, useRef } from 'react';
import Form from 'react-bootstrap/Form'
import Button from 'react-bootstrap/Button'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import { SortableContainer, SortableElement, SortableContainerProps, SortableElementProps, arrayMove } from 'react-sortable-hoc';

import BootstrapTable from 'react-bootstrap-table-next';
import paginationFactory from 'react-bootstrap-table2-paginator';
import ToolkitProvider, { Search } from 'react-bootstrap-table2-toolkit/dist/react-bootstrap-table2-toolkit';
import 'react-bootstrap-table-next/dist/react-bootstrap-table2.min.css';
import Spinner from '@dymium/common/Components/Spinner'
import Alert from 'react-bootstrap/Alert'
import * as types from '@dymium/common/Types/Common'
import * as http from '../Api/Http'

import { Link } from "react-router-dom";
import Tabs from 'react-bootstrap/Tabs'
import Tab from 'react-bootstrap/Tab'


const { SearchBar, ClearSearchButton } = Search;

const defaultDetectors = [
  {
    detector: {
      name: 'Address',
      id: '0000-0000-0000-0001',
      method: 'comprehend',
      data: 'ADDRESS'
    }
  },
  {

    detector: {
      name: 'Age',
      id: '0000-0000-0000-0002',
      method: 'comprehend',
      data: 'AGE'
    }
  },
  {

    detector: {
      name: 'CVV',
      id: '0000-0000-0000-0003',
      method: 'comprehend',
      data: 'CREDIT_DEBIT_CVV'
    }
  },
  {

    detector: {
      name: 'Credit Card Expiry',
      id: '0000-0000-0000-0004',
      method: 'comprehend',
      data: 'CREDIT_DEBIT_EXPIRY'
    }
  },
  {

    detector: {
      name: 'Credit Card Number',
      id: '0000-0000-0000-0005',
      method: 'comprehend',
      data: 'CREDIT_DEBIT_NUMBER'
    }
  },
  {

    detector: {
      name: 'Date/Time',
      id: '0000-0000-0000-0006',
      method: 'comprehend',
      data: 'DATE_TIME'
    }
  },
  {

    detector: {
      name: 'Driver License Number',
      id: '0000-0000-0000-0007',
      method: 'comprehend',
      data: 'DRIVER_ID'
    }
  },
  {

    detector: {
      name: 'Email',
      id: '0000-0000-0000-0008',
      method: 'comprehend',
      data: 'EMAIL'
    }
  },
  {

    detector: {
      name: 'International Bank Account Number',
      id: '0000-0000-0000-0009',
      method: 'comprehend',
      data: 'INTERNATIONAL_BANK_ACCOUNT_NUMBER'
    }
  },
  {

    detector: {
      name: 'IP Address',
      id: '0000-0000-0000-0010',
      method: 'comprehend',
      data: 'IP_ADDRESS'
    }
  },
  {

    detector: {
      name: 'License Plate Number',
      id: '0000-0000-0000-0011',
      method: 'comprehend',
      data: 'LICENSE_PLATE'
    }
  },
  {

    detector: {
      name: 'MAC Address',
      id: '0000-0000-0000-0012',
      method: 'comprehend',
      data: 'MAC_ADDRESS'
    }
  },
  {

    detector: {
      name: 'Name',
      id: '0000-0000-0000-0013',
      method: 'comprehend',
      data: 'NAME'
    }
  },
  {

    detector: {
      name: 'Password',
      id: '0000-0000-0000-0014',
      method: 'comprehend',
      data: 'PASSWORD'
    }
  },
  {

    detector: {
      name: 'Phone',
      id: '0000-0000-0000-0015',
      method: 'comprehend',
      data: 'PHONE'
    }
  },
  {

    detector: {
      name: 'Pin',
      id: '0000-0000-0000-0016',
      method: 'comprehend',
      data: 'PIN'
    }
  },
  {

    detector: {
      name: 'SWIFT Code',
      id: '0000-0000-0000-0017',
      method: 'comprehend',
      data: 'SWIFT_CODE'
    }
  },
  {

    detector: {
      name: 'URL',
      id: '0000-0000-0000-0018',
      method: 'comprehend',
      data: 'URL'
    }
  },
  {

    detector: {
      name: 'Username',
      id: '0000-0000-0000-0019',
      method: 'comprehend',
      data: 'USERNAME'
    }
  },
  {

    detector: {
      name: 'VIN',
      id: '0000-0000-0000-0020',
      method: 'comprehend',
      data: 'VEHICLE_IDENTIFICATION_NUMBER'
    }
  },
  {

    detector: {
      name: 'Bank Account Number',
      id: '0000-0000-0000-0021',
      method: 'comprehend',
      data: 'BANK_ACCOUNT_NUMBER'
    }
  },
  {

    detector: {
      name: 'Bank Routing Number',
      id: '0000-0000-0000-0022',
      method: 'comprehend',
      data: 'BANK_ROUTING'
    }
  },
  {

    detector: {
      name: 'Passport Number',
      id: '0000-0000-0000-0023',
      method: 'comprehend',
      data: 'PASSPORT_NUMBER'
    }
  },
  {

    detector: {
      name: 'Individual TIN',
      id: '0000-0000-0000-0024',
      method: 'comprehend',
      data: 'US_INDIVIDUAL_TAX_IDENTIFICATION_NUMBER'
    }
  },
  {
    detector: {
      name: 'SSN',
      id: '0000-0000-0000-0025',
      method: 'comprehend',
      data: 'SSN'
    }
  },
]

const regexpDetectors = [

  {
    detector: {
      name: 'Processor or device serial number',
      id: '0000-0000-0000-0026',
      method: 'columnregexp',
      data: '(serial|serialnumber|serial_number)'
    },
  },
  {
    detector: {
      name: 'Device ID',
      id: '0000-0000-0000-0027',
      method: 'columnregexp',
      data: '(device_id|deviceid)'
    },
  },
  {
    detector: {
      name: 'Cookie',
      id: '0000-0000-0000-0028',
      method: 'columnregexp',
      data: '(cookie|Cookie|COOKIE)'
    },
  },
  {
    detector: {
      name: 'Citizenship',
      id: '0000-0000-0000-0029',
      method: 'columnregexp',
      data: '(?i)(Citizenship|nationality)'
    },
  },
  {
    detector: {
      name: 'Visa or immigration status',
      id: '0000-0000-0000-0030',
      method: 'columnregexp',
      data: '(?i)(visa|immigration)'
    },
  },
  {
    detector: {
      name: 'Ethnic background',
      id: '0000-0000-0000-0031',
      method: 'columnregexp',
      data: '(?i)(ethnic|race|hispanic)'
    },
  },
  {
    detector: {
      name: 'Religious affiliation',
      id: '0000-0000-0000-0032',
      method: 'columnregexp',
      data: '(?i)(Religi)'
    },
  },
  {
    detector: {
      name: 'Sexual orientation',
      id: '0000-0000-0000-0033',
      method: 'columnregexp',
      data: '(?i)(sexual|gender|male|female)'
    },
  },
  {
    detector: {
      name: 'Criminal history',
      id: '0000-0000-0000-0034',
      method: 'columnregexp',
      data: '(?i)(criminal|arrest|felony)'
    },
  },
  {
    detector: {
      name: 'Medical information',
      id: '0000-0000-0000-0035',
      method: 'columnregexp',
      data: '(?i)(medical|illness|patient)'
    },
  },
  {
    detector: {
      name: 'Authentication information',
      id: '0000-0000-0000-0036',
      method: 'columnregexp',
      data: '(?i)(authentication|login|username)'
    },
  },
  {
    detector: {
      name: 'Country',
      id: '0000-0000-0000-0037',
      method: 'columnregexp',
      data: '(?i)(country)'
    },
  },
  {
    detector: {
      name: 'State',
      id: '0000-0000-0000-0038',
      method: 'columnregexp',
      data: '(?i)(state)'
    },
  },
  {
    detector: {
      name: 'City',
      id: '0000-0000-0000-0039',
      method: 'columnregexp',
      data: '(?i)(city|town|locality)'
    },
  },
  {
    detector: {
      name: 'Zip code',
      id: '0000-0000-0000-0040',
      method: 'columnregexp',
      data: '(?i)(Zipcode|zip_code|zip code|postalcode|postal_code)'
    },
  },
  {
    detector: {
      name: 'Job position',
      id: '0000-0000-0000-0041',
      method: 'columnregexp',
      data: '(?i)(job|position)'
    },
  },
  {
    detector: {
      name: 'Company',
      id: '0000-0000-0000-0043',
      method: 'columnregexp',
      data: '(?i)(Company|work_place|workplace)'
    },
  },
  {
    detector: {
      name: 'Work Address',
      id: '0000-0000-0000-0044',
      method: 'columnregexp',
      data: '(?i)(Work_Address|WorkAddress)'
    },
  },
  {
    detector: {
      name: 'Vehicle Identification Number',
      id: '0000-0000-0000-0045',
      method: 'columnregexp',
      data: '(?i)(vin|VehicleIdentificationNumber|Vehicle_Identification_Number)'
    },
  },
  {
    detector: {
      name: 'Contact Info',
      id: '0000-0000-0000-0046',
      method: 'columnregexp',
      data: '(?i)(contact)'
    },
  },

]
let columns = [
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
    dataField: 'typ',
    text: 'Type:',
    /*
    isDummyField: true,
    */
    formatter: (cell, row, rowIndex, formatExtraData) => {
      if (row['typ'] === 'comprehend')
        return <div>{types.humanReadablePIIDetectionType(row['typ'])}</div>
      else
        return <select
          onChange={e => {
          }}
          value={row['typ']} >
          <option value="columnregexp">Regexp for Table Columns</option>
          <option value="contentregexp">Regexp for Table Columns</option>

        </select>

    },

    sort: true,
  },

  {
    dataField: 'data',
    text: 'Regexp:',
    sort: false,
    formatter: (cell, row, rowIndex, formatExtraData) => {
      if (row['typ'] === 'comprehend')
        return <div></div>
      else
        return <input style={{ width: '40em' }}
          onChange={e => {
          }}
          value={row['data']} >


        </input>

    }
  }
]

const defaultSortedBy = [{
  dataField: "name",
  order: "asc"  // or desc
}];
function BuildRules() {
  const [rules, setRules] = useState<any[]>([])
  const [headers, setHeaders] = useState<any[]>([])
  const [spinner, setSpinner] = useState(false)
  const [alert, setAlert] = useState<JSX.Element>(<></>)

  let getPolicy = () => {
    setHeaders(columns)
    let det = defaultDetectors.map((x: any) => {
      return {
        id: x.detector.id,
        name: x.detector.name,
        typ: x.detector.method,
        data: x.detector.data
      }
    })
    let regs = regexpDetectors.map((x: any) => {
      return {
        id: x.detector.id,
        name: x.detector.name,
        typ: x.detector.method,
        data: x.detector.data
      }
    })

    setRules(det.concat(regs))
  }
  useEffect(() => {
    getPolicy()
  }, [])

  return (
    <>

      <div id="tablecontainer" style={{ width: '90%' }} className="text-center mb-5">
        <ToolkitProvider
          bootstrap4
          keyField='name'
          data={rules}
          columns={columns}
          search >
          {
            props => (
              <div className="text-left">
                {alert}
                <div className="d-flex">
                  <h5 >Edit Policy Suggestions  <Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>


                  <div style={{ marginLeft: "auto" }}>
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
              </div>
            )
          }
        </ToolkitProvider>
      </div>
    </>
  )
}


function AccessLevels() {
  const [spinner, setSpinner] = useState(false)
  const [alert, setAlert] = useState<JSX.Element>(<></>)

  const [name, setName] = useState("")
  const [level, setLevel] = useState("")
  const [actions, setActions] = useState<types.DataAction[]>([])
  let policy = useRef(new types.DataPolicy())

  let savePolicies = () => {
    let error = <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
      Error retrieving policy.
    </Alert>

    let js = JSON.parse(policy.current.toJson() )
    // do cleannup
    js.actions.sort( (a, b) => {
      return (a.index > b.index) ? 1 : -1
    })
    debugger
    for(let i = 0; i < js.piisuggestions.length; i++) {

    }
    let body = JSON.stringify(js)
    setSpinner(true)
    http.sendToServer("POST", "/api/savepolicies",
      null, body,
      resp => {
        resp.json().then(js => {
          setSpinner(false)
          if (js.error !== undefined) {
            // need to initialize the whole iguana
            let dp = new types.DataPolicy()
            dp.piisuggestions =
              policy.current = dp

          } else {
            policy.current = types.DataPolicy.fromJson(js)

          }
        }).catch((error) => {
          setAlert(
            error
          )
          setSpinner(false)
        })
      },
      resp => {
        setSpinner(false)
        setAlert(
          error
        )
        setSpinner(false)
      },
      error => {
        console.log("on exception")
        setSpinner(false)
        setAlert(
          error
        )
        setSpinner(false)
      })
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
            policy.current = new types.DataPolicy()

            let det = [...defaultDetectors]
            let newa = det.concat(regexpDetectors)
            
            let prep = newa.map(x => {
              let y = types.PIISuggestor.fromJson(  {actions: [], detector: x.detector} ) 
              return y
            })
            policy.current.piisuggestions = prep
            
            let actions = types.
          } else {
            policy.current = types.DataPolicy.fromJson(js)

          }
        }).catch((error) => {
          setAlert(
            error
          )
          setSpinner(false)
        })
      },
      resp => {
        setSpinner(false)
        setAlert(
          error
        )
        setSpinner(false)
      },
      error => {
        console.log("on exception")
        setSpinner(false)
        setAlert(
          error
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
        <Col>
          {value}
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

  let handlingOptions = () => {
    let opts: types.DataHandling[] = ["allow", "block", "obfuscate", "redact"]

    return opts.map(x => {
      return <option key={x} value={x}>
        {types.humanReadableDataHandling(x)}
      </option>
    })

  }
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
    <h5 >Define Access Levels  <Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>
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
        <SortableList distance={1} items={actions} onSortEnd={onSortEnd} />
        {actions.length > 0 &&
          <Row className="mt-5">
            <Col xs="auto">
              <Button size="sm" variant="dymium" type="submit">Apply</Button>
            </Col>
          </Row>
        }
      </Form>
    </div>
  </div>
}


export default function Rules() {

  return (
    <Tabs


      unmountOnExit={true} className="mb-3 text-left">

      <Tab eventKey="levels" title="Access Levels" className="mx-4">
        <AccessLevels />
      </Tab>
      <Tab eventKey="build" title="Rules" className="mx-4" >
        <BuildRules />
      </Tab>
    </Tabs>

  )

}