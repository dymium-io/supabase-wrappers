import React, { useEffect } from 'react';
import { Outlet } from "react-router-dom";
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Auth from '../Auth'
import Menu from './Menu'
import Sidebar from './Sidebar'

const HomePage = (props) => {
    return (
        "homepage"
    )
}
const AuthenticatedApp = () => {
    useEffect( () => {

    }, []) 
    return (
        <>
            <Auth />
            <Menu />

            <Row style={{height: '100vh'}}>
                <Col xs="auto" id="sidebar_container" className="sidebar_container"><Sidebar/></Col>
                <Col>
                    <Outlet />
                </Col>

            </Row>

        </>
    )
}

export default AuthenticatedApp;