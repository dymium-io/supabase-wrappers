import React, { useState } from 'react';
import Navbar from 'react-bootstrap/Navbar'
import Nav from 'react-bootstrap/Nav'


function Menu() {

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    let [avatar, setAvatar] = useState('/avatar.png') // eslint-disable-line no-unused-vars

        return (
            <div className="w-100 text-center deepestblue">
                <Navbar id="navb"
                    className="p-0 m-0"
                    collapseOnSelect expand="lg" variant="light"  >
                    <Navbar.Brand className="text-left p-0 m-0 mr-2" >

                    <Nav.Link className="p-1 m-0" style={{width: '200px'}} href="/">
                            <img src="/fullbrand.svg" style={{width: '100%'}} alt="logo" />
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