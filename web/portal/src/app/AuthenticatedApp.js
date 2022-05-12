import React, { useEffect } from 'react';
import { Outlet } from "react-router-dom";
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Error404 from "../Error404"
import Menu from './Menu'
import Sidebar from './Sidebar'
const Auth = () => {
    return (
        ""
    )
}
const HomePage = (props) => {
    debugger
    return (
        "homepage"
    )
}
const AuthenticatedApp = () => {
    return (
        <>
            <Auth />
            <Menu />

            <Row style={{height: '100vh'}}>
                <Col xs="auto"><Sidebar/></Col>
                <Col>
                    <Outlet />
                </Col>

            </Row>

        </>
    )
}

export default AuthenticatedApp;