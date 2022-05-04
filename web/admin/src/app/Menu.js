import React, { Component } from 'react';
import Navbar from 'react-bootstrap/Navbar'
import Nav from 'react-bootstrap/Nav'



class Menu extends Component {
    constructor(props) {
        super(props)
        this.props=props
    }

    componentDidMount() {
    }

    componentWillUnmount() {
    }


    render = () => {
        return (
            <div name="home" className="w-100 text-center deepestblue">
                <Navbar id= "navb"
                    className="p-0 m-0"
                     collapseOnSelect expand="lg" variant="light"  >
                    <Navbar.Brand className="text-left p-0 m-0 mr-2" >

                            <Nav.Link className="px-0 py-0 m-0" to="/">
                                <img  src="/logo.png" style={{width: '160px', height: 'auto'}} alt="logo" />
                            
                            </Nav.Link >
                        
                    </Navbar.Brand>

                      
                </Navbar>
   
            </div>
        )
    }
};

export default Menu