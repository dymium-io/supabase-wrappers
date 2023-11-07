import React, { useEffect } from 'react';
import { Outlet } from "react-router-dom";
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Auth from '../Auth'
import Menu from './Menu'
import Sidebar from './Sidebar'

const AuthenticatedApp = () => {
    useEffect( () => {

    }, []) 
    return (
        <>
            <Auth />
            <Menu />

            <Row  style={{height: '100vh'}} className="p-0 m-0">
                <Col xs="auto" id="sidebar_container"  className="px-0 sidebar_container"><Sidebar/></Col>
                <Col xs className="p-0 spider" style={{overflowX: "scroll"}}>
                    <Outlet />
                </Col>

            </Row>

        </>
    )
}

export default AuthenticatedApp;