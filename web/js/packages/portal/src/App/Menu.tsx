import React, { useState } from 'react';
import Navbar from 'react-bootstrap/Navbar'
import Button from 'react-bootstrap/Button'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Nav from 'react-bootstrap/Nav'
import Modal from 'react-bootstrap/Modal'
import * as com from '../Common'
import * as hex from '../Utils/Hex'


function Menu() {

    let [avatar, setAvatar] = useState('/avatar.png') // eslint-disable-line no-unused-vars
    let [show, setShow] = useState(false)

    let onAva = e => {
        setShow(true)
    }
    let handleClose = () => {
        setShow(false)
    }
    let getGroups = () => {
        let gr = com.getTokenProperty("groups")
        if (gr === null || gr === undefined)
            return ""
        return gr.join(", ")
    }
    let pic = com.getTokenProperty("picture")

    let p = pic && hex.HexStringToByteArray(pic)
    let picture = p && hex.StringFromUTF8Array(p)

    if (undefined !== picture && avatar != picture) {
        setAvatar(picture)
    }
    return (
        <div id="home" className="w-100 text-center deepestblue">
            <Modal show={show} onHide={handleClose} centered >
                <Modal.Header closeButton>
                    <Modal.Title>Current user:</Modal.Title>
                </Modal.Header>
                <Modal.Body style={{maxHeight: '500px', overflowY: 'scroll'}}>
                    <Row>
                        <Col xs={2}>Name: </Col>
                        <Col>{com.getTokenProperty("name")}</Col>
                    </Row>
                    <Row>
                        <Col xs={2}>Groups: </Col>
                        <Col>{getGroups()}</Col>
                    </Row>
                </Modal.Body>
                <Modal.Footer>
                    <Button variant="secondary" onClick={handleClose}>Close</Button>
                </Modal.Footer>
            </Modal>
            <Navbar id="navb"
                className="p-0 m-0 navbar-expand-sm"
                collapseOnSelect expand="lg" variant="light">
                <Navbar.Brand className="text-left p-0 m-0 mr-2" >

                    <Nav.Link className="p-1 m-0" style={{ width: '48px' }} href="/">
                        <img src="/logo.svg" style={{ width: '100%' }} alt="logo" />
                    </Nav.Link >
                </Navbar.Brand>
                <Navbar.Brand className="text-left p-0 m-0 mr-2" >
                    <Nav.Link className="p-0 m-0 ml-1" style={{ width: '140px' }} href="/">
                        <img src="/brand.svg" style={{ width: '100%', marginTop: '-2px', marginLeft: '3px' }} alt="logo" />
                    </Nav.Link >
                </Navbar.Brand>

                <Navbar.Toggle aria-controls="responsive-navbar-nav" />

                <Navbar.Collapse id="responsive-navbar-nav">
                    <Nav className="mx-auto " >

                    </Nav>
                    <img alt="avatar" onClick={onAva} style={{ cursor: 'pointer' }} className="avatar mr-2" src={avatar} />
                    <Nav className="pr-3">
                        <Nav.Link id="logout" href="/api/logout">Logout</Nav.Link>
                    </Nav>
                </Navbar.Collapse>
            </Navbar>

        </div>
    )
};

export default Menu