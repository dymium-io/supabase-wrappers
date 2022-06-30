import React from 'react';
import 'bootstrap/dist/css/bootstrap.min.css';
import '../App.css';
import { Link } from 'react-router-dom'
import Col from 'react-bootstrap/Col'
import Row from 'react-bootstrap/Row'
import Button from 'react-bootstrap/Button'
import * as com from '../Common'

import './Sidebar.css';



export default function Sidebar() {

    return (
      <div className="sidenav h-100" id="sidebar">
        <Link className='hover-sidebar' 
          to='/app/dashboard' > <i className="fas fa-tachometer-alt mr-1"></i>Dashboard</Link >

        <Link className='hover-sidebar' 
          to='/app/customers' > <i className="fa fa-users mr-1"></i>Customers</Link >

        <a className='hover-sidebar' 
                 href="/app/logout"
         > 
                  <i className="fa fa-sign-out" aria-hidden="true"></i>Logout</a >

      </div>

    )
}

