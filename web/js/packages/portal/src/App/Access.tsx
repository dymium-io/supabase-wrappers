import React, { useEffect, useState, useRef } from 'react';
import Tabs from 'react-bootstrap/Tabs'
import Tab from 'react-bootstrap/Tab'
import { useLocation, useNavigate } from "react-router-dom";
import Form from 'react-bootstrap/Form'
import Button from 'react-bootstrap/Button'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import { Link } from "react-router-dom";
import Offcanvas from '@dymium/common/Components/Offcanvas'
import Alert from 'react-bootstrap/Alert'
import Spinner from '@dymium/common/Components/Spinner'
import * as http from '@dymium/common/Api/Http'
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
  const [showOffhelp, setShowOffhelp] = useState(com.isInstaller())

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
  String url = "jdbc:postgresql://localhost:`+ port + `/` + scope.name + `";
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
        <Offcanvas modal={false} width={300} show={showOffhelp} onClose={(e) => { setShowOffhelp(false) }}>
          <h5>Accessing Ghost Databases</h5>
          <div className="mb-3">
            This page allows the end user to access the allowed Ghost Databases.
          </div>
          <div className="mb-3">
            In addition to using single sign on to authenticate you to the Dymium portal, an extra level of authentication is introduced for accessing the data. The system generates a password for you, and you must save it. It will only be visible in the GUI for a day
          </div>

          <div className="mb-3">
            For a given database use the connection string from the appropriate tab: Psql, Python, JDBC, etc.
          </div>

          <div className="mb-3">
            Note that before accessing a Ghost Database, a Dymium tunneling client must be downloaded and run.
            Please go to the <Link to="/app/access?key=download">Client Download And Use tab</Link>.
          </div>

        </Offcanvas>

        <div className="thickblue mb-2" style={{ fontWeight: 'bold' }}><i className="fa fa-key mr-2" aria-hidden="true"></i>Credentials:</div>
        <div>Username: {datascopes !== undefined && datascopes.username}</div>
        <div>Password: {datascopes !== undefined && datascopes.password}
          <Button onClick={regenerate} style={{ marginTop: '-4px', paddingBottom: '0.1em', paddingTop: '0.0em' }} variant="dymium" className="mx-2" size="sm">Regenerate</Button>

          {datascopes !== undefined && datascopes.password !== `**********` &&
            <i onClick={copy()} className="fas fa-copy clipbtn"></i>
          } </div>
        <div className="mt-3" style={{ fontSize: '0.8em' }}>Password is only visible to you for a day. You can always generate a new one.</div>
      </div>
      {datascopes !== undefined && datascopes.datascopes.map(x => {
        return <div className="my-5">
          <h5 className="mb-2"><i className="fa fa-unlock mr-2" aria-hidden="true"></i>Ghost Database: {x.name}</h5>
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
            <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>{error.message}</Alert>
          )
          setSpinner(false)
        })
      },
      resp => {
        setSpinner(false)
        resp != null && resp.text().then(t =>
          setAlert(
            <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>Get datascope access failed: {t}</Alert>

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
            <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>{error.message}</Alert>
          )
          setSpinner(false)
        })
      },
      resp => {
        setSpinner(false)
        resp != null && resp.text().then(t =>
          setAlert(<Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>Regenerate password failed: {t}</Alert>
          ))
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
    getDatascopeAccess()
  }, [])

  return (
    <div className=" text-left">
      {alert}
      <h5 > Accessing Ghost Databases  <i onClick={e => { setShowOffhelp(!showOffhelp) }} className="trash fa-solid fa-circle-info mr-1"></i><Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>
      <div className=" text-left">
        {datascopes !== undefined && displayDatascopes()}
      </div>
    </div>
  )
}
let orgid = com.getTokenProperty("schema")
let url = window.location.protocol + "//" + window.location.host + "/"
let params = `-c ${orgid} -p ${url}`

function copy(str) {

  return e => {
    navigator.clipboard.writeText(str);
  }
}
function Downloads() {
  const [showOffhelp, setShowOffhelp] = useState(com.isInstaller())

  return (
    <div className=" text-left">
      <Offcanvas modal={false} width={300} show={showOffhelp} onClose={(e) => { setShowOffhelp(false) }}>
        <h5>Clients</h5>
        <div className="mb-3">
          This page give you access to the tunneling clients, and their launch parameters.
        </div>
        <div className="mb-3">
          Both command line and GUI clients are available. Download a client for your OS.
        </div>
        <div className="mb-3">
          You will get authenticated through the browser, and the connection will be established.
        </div>
        <div className="mb-3">
          Go to <Link to="/app/access?key=datascopes">Your Ghost Databases</Link> to learn how to run the client.
          Copy/paste the command for CLI, or run the GUI client, and copy/paste the parameters into the GUI.
        </div>

      </Offcanvas>
      <h5 > Client Downloads <i onClick={e => { setShowOffhelp(!showOffhelp) }} className="trash fa-solid fa-circle-info mr-1"></i></h5>
      <div className=" text-left">

        <div className="viewport">
          <div>On MS Windows:</div>
          <a href="/DymiumInstaller.exe" download="DymiumInstaller.exe"> <i className="fab fa-windows mr-2" aria-hidden="true"></i>Click to Download MS Windows Client</a>
          <div>
            CLI Usage:
            <div style={{ display: "flex" }}>
              <div className="terminal">
                &gt;dymium.exe {params}
              </div>
              <i onClick={copy("dymium.exe " + params)} className="fas fa-copy clipbtn"></i>
            </div>
          </div>
        </div>


        <div className="viewport">
          <div>On Mac OS X:</div>
          <a href="/DymiumInstaller.pkg" download="DymiumInstaller.pkg"> <i className="fab fa-apple mr-2" aria-hidden="true"></i>Click to Download Mac OS X Client</a>
          <div>
            CLI Usage:
            <div style={{ display: "flex" }}>
              <div className="terminal">
                &gt;./dymium {params}
              </div>
              <i onClick={copy("./dymium " + params)} className="fas fa-copy clipbtn"></i>
            </div>
          </div>
        </div>

        <div className="viewport">
          <div>On Linux:</div>
          <a href="/tunnel.tar.gz" download="tunnel.tar.gz"> <i className="fab fa-linux mr-2" aria-hidden="true"></i>Click to Download Linux Client</a>
          <div>
            CLI Usage:
            <div style={{ display: "flex" }}>
              <div className="terminal">
                &gt;./dymium {params}
              </div>
              <i onClick={copy("./dymium " + params)} className="fas fa-copy clipbtn"></i>
            </div>
          </div>
        </div>
        <div>
          To use the GUI, launch dymiumgui.app on Mac OS, or dymiumgui.exe on Windows.
        </div><div>
          <img src="/dymiumgui.png" style={{ width: "540px" }}></img>
          <div className="mb-5">
            Paste the connection string for your OS into the application, and click Connect.
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

  const appDispatch = useAppDispatch()
  let query = useQuery();


  let tt = query.get("key")
  if (tt !== null) {
    t = tt
  }
  if (t == null) {
    t = "datascopes"
  }
  useEffect(() => {
    if (tt != null) {

      appDispatch(setActiveAccessTab(t))
      //location.href = "/app/access"
      console.log("navigate!")
      navigate("/app/access", { replace: true })
    }
  }, [t])
  console.log(t)
  return (
    <Tabs activeKey={t}
      onSelect={(k) => appDispatch(setActiveAccessTab(k))}

      unmountOnExit={true} className="mb-3 text-left">
      <Tab eventKey="datascopes" title="Your Ghost Databases" className="mx-4">
        <YourDatascopes />
      </Tab>
      <Tab eventKey="download" title="Client Download And Use" className="mx-4">
        <Downloads />
      </Tab>

    </Tabs>
  )
}