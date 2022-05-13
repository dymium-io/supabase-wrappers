import React, { useEffect } from 'react';
import { Outlet } from "react-router-dom";
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'

import Menu from './Menu'
import Auth from '../Auth'
import Sidebar from './Sidebar'

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