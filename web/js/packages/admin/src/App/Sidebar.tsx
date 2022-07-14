import React from 'react';
import 'bootstrap/dist/css/bootstrap.min.css';
import '@dymium/common/App.scss';
import { Link } from 'react-router-dom'
import Col from 'react-bootstrap/Col'
import Row from 'react-bootstrap/Row'
import Button from 'react-bootstrap/Button'


import '@dymium/common/Sidebar.scss';



export default function Sidebar() {

  return (
    <div className="sidenav h-100" id="sidebar">
      <Link className='hover-sidebar'
        to='/app/dashboard' > <div className='darkblue'> <i className="fas fa-tachometer-alt mr-1"></i>Dashboard</div></Link >

      <Link className='hover-sidebar'
        to='/app/customers' > <div className='darkblue'> <i className="fa fa-users mr-1"></i>Customers</div></Link >

      <a className='hover-sidebar'
        href="/app/logout"
      >
         <div className='darkblue'> <i className="fa fa-sign-out" aria-hidden="true"></i>Logout </div></a >

    </div>

  )
}

