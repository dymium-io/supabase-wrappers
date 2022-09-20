import React, { useEffect, useState, useRef } from 'react';
import Tabs from 'react-bootstrap/Tabs'
import Tab from 'react-bootstrap/Tab'
import { useLocation, useNavigate } from "react-router-dom";
import Form from 'react-bootstrap/Form'
import Button from 'react-bootstrap/Button'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import { Link } from "react-router-dom";
import Alert from 'react-bootstrap/Alert'
import Spinner from '@dymium/common/Components/Spinner'
import * as http from '../Api/Http'
import * as com from '../Common'
import * as types from '@dymium/common/Types/Common'
import * as internal from '@dymium/common/Types/Internal'
import { useInitialize } from '../Utils/CustomHooks'
import { useAppDispatch, useAppSelector } from './hooks'
import { setActiveAccessTab } from '../Slices/menuSlice'

function YourDatascopes() {
  const [spinner, setSpinner] = useState(false)
  const [alert, setAlert] = useState<JSX.Element>(<></>)
  const [datascopes, setDatascopes] = useState<types.UserDatascopes>()
  let port = com.getTokenProperty("port")
  //"/api/getdatascopesaccess"
  let displayDatascope = (index) => {

  }
  let commandLine = (scope) => {
    let c = `psql -h localhost -p ${port} -d ${scope.name} -U ${datascopes !== undefined && datascopes.username}`

    return <div style={{ display: "flex" }}> <div className=" terminal">
      &gt;{c}
      </div>
      <i onClick={copy(c)} className="fas fa-copy clipbtn"></i>
    </div>
  }
  let python = scope => {
    let p = `conn = psycopg2.connect("host=localhost port=${port} dbname=${scope.name} user=${datascopes !== undefined && datascopes.username} password=${datascopes !== undefined && datascopes.password}")`

  return <div style={{ display: "flex" }}> <div className=" terminal">
        {p}
      </div>
      <i onClick={copy(p)} className="fas fa-copy clipbtn"></i>
    </div>
  }
let java = scope => {
  let j = `
  String url = "jdbc:postgresql://localhost:`+ port + `/`+scope.name+`";
  Properties props = new Properties();
  props.setProperty("user","${datascopes !== undefined && datascopes.username}");
  props.setProperty("password","${datascopes !== undefined && datascopes.password}");
  
  `

return <div style={{ display: "flex" }}> 
  <div><div className=" terminal"> <pre><code>{j}
  </code></pre></div>
  </div> 
  <i onClick={copy(j)} className="fas fa-copy clipbtn"></i>  
  </div>
}
let displayDatascopes = () => {
  let copy = () => {
    return e => {
      if (datascopes !== undefined)
        navigator.clipboard.writeText(datascopes.password);
    }
  }

  return <div>
    <div className="viewport">
      <div className="thickblue mb-2" style={{ fontWeight: 'bold' }}><i className="fa fa-key mr-2" aria-hidden="true"></i>Credentials:</div>
      <div>Username: {datascopes !== undefined && datascopes.username}</div>
      <div>Password: {datascopes !== undefined && datascopes.password} {datascopes !== undefined && datascopes.password === `**********` ?
        <Button onClick={regenerate} style={{ paddingBottom: '0.1em', paddingTop: '0.1em' }} variant="dymium" className="ml-3" size="sm">Regenerate</Button>
        :
        <i onClick={copy()} className="fas fa-copy clipbtn"></i>

      }</div>
      <div className="mt-3" style={{ fontSize: '0.8em' }}>Password is only visible to you for a day. You can always generate a new one.</div>
    </div>
    {datascopes !== undefined && datascopes.datascopes.map(x => {
      return <div className="my-5">
        <h5 className="mb-2"><i className="fa fa-unlock mr-2" aria-hidden="true"></i>Datascope: {x.name}</h5>
        <div className="datascopeuse">
          <Tabs
            id="datascope"


            unmountOnExit={true} className=" mt-0 text-left ">
            <Tab eventKey="cmd" title="Command Line" className=" mx-2">
              {commandLine(x)}
            </Tab>
            <Tab eventKey="python" title="Python" className=" mx-2">
              {python(x)}
            </Tab>
            <Tab eventKey="java" title="Java" className=" mx-2">
              {java(x)}
            </Tab>

          </Tabs>
        </div>
      </div>
    })}
  </div>
}
let getDatascopeAccess = () => {
  let error = <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
    Error retrieving datascopes.
  </Alert>
  setSpinner(true)
  http.sendToServer("GET", "/api/getdatascopesaccess",
    null, "",
    resp => {
      resp.json().then(js => {
        setDatascopes(types.UserDatascopes.fromJson(js))
        setSpinner(false)
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

let regenerate = () => {
  let error = <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
    Error regenerating password.
  </Alert>
  setSpinner(true)
  http.sendToServer("GET", "/api/regenpassword",
    null, "",
    resp => {
      resp.json().then(js => {
        setDatascopes(types.UserDatascopes.fromJson(js))
        setSpinner(false)
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
  getDatascopeAccess()
}, [])

return (
  <div className=" text-left">
    {alert}
    <h5 > Your Datascopes <Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>
    <div className=" text-left">

      {datascopes !== undefined && displayDatascopes()}


    </div>
  </div>
)
}
let orgid = com.getTokenProperty("orgid")
let url = window.location.protocol + "//" + window.location.host + "/"
let params = `-c ${orgid} -p ${url}`

function copy(str) {

  return e => {
    navigator.clipboard.writeText(str);
  }
}
function Downloads() {

  return (
    <div className=" text-left">

      <h5 > Client Downloads</h5>
      <div className=" text-left">

        <div className="viewport">
          <div>On MS Windows:</div>
          <a href="/tunnel_win.zip" download="tunnel_win.zip"> <i className="fab fa-windows mr-2" aria-hidden="true"></i>Click to Download MS Windows Client</a>
          <div>
            Usage:
            <div style={{ display: "flex" }}>
              <div className="terminal">
                &gt;tunnel.exe {params}
              </div>
              <i onClick={copy("tunnel.exe " + params)} className="fas fa-copy clipbtn"></i>
            </div>
          </div>
        </div>


        <div className="viewport">
          <div>On Mac OS X:</div>
          <a href="/tunnel_mac.tar.gz" download="tunnel_mac.tar.gz"> <i className="fab fa-apple mr-2" aria-hidden="true"></i>Click to Download Mac OS X Client</a>
          <div>
            Usage:
            <div style={{ display: "flex" }}>
              <div className="terminal">
                &gt;./tunnel {params}
              </div>
              <i onClick={copy("./tunnel " + params)} className="fas fa-copy clipbtn"></i>
            </div>
          </div>
        </div>

        <div className="viewport">
          <div>On Linux:</div>
          <a href="/tunnel.tar.gz" download="tunnel.tar.gz"> <i className="fab fa-linux mr-2" aria-hidden="true"></i>Click to Download Linux Client</a>
          <div>
            Usage:
            <div style={{ display: "flex" }}>
              <div className="terminal">
                &gt;./tunnel {params}
              </div>
              <i onClick={copy("./tunnel " + params)} className="fas fa-copy clipbtn"></i>
            </div>
          </div>
        </div>
      </div>
    </div>
  )
}
function useQuery() {
  const { search } = useLocation();

  return React.useMemo(() => new URLSearchParams(search), [search]);
}
export default function Access() {
  const navigate = useNavigate();
  let t = useAppSelector((state) => {

    return state.reducer.activeAccessTab
  }
  )
  console.log("t=", t)
  const appDispatch = useAppDispatch()
  let query = useQuery();

  useEffect(() => {
    if (query.get("key") !== undefined) {

      appDispatch(setActiveAccessTab(query.get("key")))
      navigate("/app/access")
    }
  }, [])
  let tt = query.get("key")
  if (tt !== null) {
    t = tt
  }
  if (t == null) {
    t = "datascopes"
  }
  return (
    <Tabs defaultActiveKey={t}
      onSelect={(k) => appDispatch(setActiveAccessTab(k))}

      unmountOnExit={true} className="mb-3 text-left">
      <Tab eventKey="datascopes" title="Your Datascopes" className="mx-4">
        <YourDatascopes />
      </Tab>
      <Tab eventKey="download" title="Client Download And Use" className="mx-4">
        <Downloads />
      </Tab>

    </Tabs>
  )
}