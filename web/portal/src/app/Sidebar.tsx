import React, { useState } from 'react';
import 'bootstrap/dist/css/bootstrap.min.css';
import '../App.css';
import { Link } from 'react-router-dom'
import * as com from '../Common'

import './Sidebar.css';


com.customizeStyleSheet() 
export default function Sidebar() {

    return (
      <div className="sidenav h-100" id="sidebar">
        <Link className='hover-sidebar' 
          to='/app/dashboard' > <i className="fas fa-tachometer-alt me-1 fa-fw"></i>Dashboard</Link >

        <Link className='hover-sidebar' 
          to='/app/customers' > <i className="fas fa-database me-1 fa-fw"></i>Connections</Link >

        <Link className='hover-sidebar' 
          to='/app/customers' > <i className="fas fa-users me-1 fa-fw"></i>Groups</Link >

        <a className='hover-sidebar' 
                 href="/api/logout"
         > 
                  <i className="fas fa-sign-out me-1 fa-fw" aria-hidden="true"></i>Logout</a >

      </div>

    )
}

