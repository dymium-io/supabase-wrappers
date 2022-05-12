import React, { Component } from 'react';
import 'bootstrap/dist/css/bootstrap.min.css';
import '../App.css';
import { Link } from 'react-router-dom'
import Col from 'react-bootstrap/Col'
import Row from 'react-bootstrap/Row'
import Button from 'react-bootstrap/Button'
import * as com from '../Common'

import './Sidebar.css';



export default class Sidebar extends Component {
  constructor(props) {
    super(props);
    this.state = {
      logoutURL: ""
    };
    com.customizeStyleSheet() 
  }

  componentDidMount() {
    let fetchData = () => {
      fetch('/api/getlogout', {
        method: 'GET',
        headers: {
          Cache: "no-cache"
        }
      }).then(
        response => {
          if (response.status !== 200) {
            console.log('Looks like there was a problem. Status Code: ' +
              response.status);
            return;
          }
          // Examine the text in the response
          response.json().then(js => {
            this.setState({logoutURL: js.LogoutURL})
          });
        }
      )
        .catch(function (err) {
          console.log('Fetch Error :', err);
        });    
    }
    fetchData()
  }

  render() {
    return (
      <div className="sidenav h-100" id="sidebar">
        <Link className='hover-sidebar' 
          to='/app/dashboard' > <i className="fas fa-tachometer-alt mr-1"></i>Dashboard</Link >

        <Link className='hover-sidebar' 
          to='/app/customers' > <i className="fa fa-users mr-1"></i>Customers</Link >

        <a className='hover-sidebar' 
                 /* 
          https://dymium.us.auth0.com/v2/logout?returnTo=https%3A%2F%2Fadmin.dymium.us%3A3000%2Flogout&client_id=oXX423SHkmtfR6qxrcJddAnneYffsdki%!(EXTRA%20string=oXX423SHkmtfR6qxrcJddAnneYffsdki)             
    href='https://dymium.us.auth0.com/v2/logout?returnTo=https%3A%2F%2Fadmin.dymium.us%3A3000%2Flogout&client_id=oXX423SHkmtfR6qxrcJddAnneYffsdki' */
                 href={this.state.logoutURL}
         > 
                  <i className="fa fa-sign-out" aria-hidden="true"></i>Logout</a >

      </div>

    )
  }
}

