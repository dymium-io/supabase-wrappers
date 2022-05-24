import React, { useState } from 'react';
import Navbar from 'react-bootstrap/Navbar'
import Nav from 'react-bootstrap/Nav'
import * as com from '../Common'



function Menu() {

    let [avatar, setAvatar] = useState('/avatar.png') // eslint-disable-line no-unused-vars

    let picture = com.getTokenProperty("picture")
    if(undefined !== picture && avatar != picture) {
        setAvatar(picture)
    }
        return (
            <div id="home" className="w-100 text-center deepestblue">
                <Navbar id="navb"
                    className="p-0 m-0 navbar-expand-sm"
                    collapseOnSelect expand="lg" variant="light">
                    <Navbar.Brand className="text-left p-0 m-0 mr-2" >

                        <Nav.Link className="p-1 m-0" style={{width: '48px'}} href="/">
                            <img src="/logo.svg" style={{width: '100%'}} alt="logo" />
                        </Nav.Link >
                        </Navbar.Brand>
                        <Navbar.Brand className="text-left p-0 m-0 mr-2" >
                        <Nav.Link className="p-0 m-0 ms-1" style={{width: '140px'}} href="/">
                            <img src="/brand.svg" style={{width: '100%'}} alt="logo" />
                        </Nav.Link >
                    </Navbar.Brand>

                    <Navbar.Toggle aria-controls="responsive-navbar-nav" />

                    <Navbar.Collapse id="responsive-navbar-nav">
                        <Nav className="mx-auto " >
                           
                        </Nav>
                        <img alt="avatar" className="avatar mr-2" src={avatar} />
                        <Nav className="pr-3">
                            <Nav.Link id="logout" href="/api/logout">Logout</Nav.Link>
                        </Nav>
                    </Navbar.Collapse>
                </Navbar>

            </div>
        )
};

export default Menu