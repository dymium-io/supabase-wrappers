import React, { useEffect } from 'react';
import { Outlet } from "react-router-dom";
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Backdrop from "../Backdrop"
import Menu from './Menu'
import Auth from '../Auth'
import { Link } from "react-router-dom";
import Sidebar from './Sidebar'
import * as com from '../Common'

const AuthenticatedApp = () => {
  let roles = com.getTokenProperty("roles")
  if (roles === null) {
    return <div className="py-0 my-0">
      <Backdrop />
      <div className="text-center" style={{
        position: 'absolute', top: '0px', left: '0px',
        width: '100%', height: '100vh'
      }}>
        <div id="loginbox" >
          <div className="row">
            <div className="col-sm ml-5  text-center">
              <h1 style={{ marginTop: '1.3em', fontSize: '5em', fontWeight: '300' }} className="logoheader ">
                <Link className="linklink" to="/" > Dymium </Link></h1>
            </div>
            <div className="col-sm mt-5  text-center">
              <h1 className="mt-5 logoheader">Error</h1>
              <h1 className="unauthorized" >Not authorized!</h1>
              <div className="pt-2 logofooter">
                You are succesfully authenticated but not authorized to use Dymium. Please contact your administrator.
              </div>

            </div>
          </div>

        </div>
      </div>



    </div>
  }
  return (
    <>
      <Auth />
      <Menu />

      <Row style={{ height: '100vh' }} className="p-0 m-0">
        <Col xs="auto" id="sidebar_container" className="px-0 sidebar_container"><Sidebar /></Col>
        <Col xs className="p-0 spider" style={{ overflowX: "scroll" }}>
          <Outlet />
        </Col>

      </Row>

    </>
  )
}

export default AuthenticatedApp;