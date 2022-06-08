import React, { useEffect } from 'react';
import LandingPage from './LandingPage'
import AuthenticatedApp from './app/AuthenticatedApp'
import Error404 from "./Error404"
import Logout from "./Logout"
import Error from "./Error"
import Invitation from "./Invitation"
import Sticky from "./app/Sticky"
import Dashboard from "./app/Dashboard"
import Customers from "./app/Customers"
import Connections from "./app/Connections"
import Datascopes from "./app/Datascopes"
import Groups from "./app/Groups"
import Rules from "./app/Rules"
import TestSQL from "./app/TestSQL"
import * as com from './Common'

import "@fontsource/roboto";
import 'bootstrap/dist/css/bootstrap.min.css';
import 'react-bootstrap-typeahead/css/Typeahead.css';

import { BrowserRouter, Routes, Route } from "react-router-dom";
//import Helmet from "react-helmet";

import '@dymium/common/App.scss';
com.customizeStyleSheet()

function App() {
  return (
    <div className="App">

      <BrowserRouter>
        <Routes>

          <Route  path="/" element={<LandingPage />} />

          <Route path="/app/" element={<AuthenticatedApp />} >
            <Route path="/app/dashboard" element={<Dashboard />} />
            <Route path="/app/connections" element={<Connections />} />     
            <Route path="/app/datascopes" element={<Datascopes />} />                
            <Route path="/app/groups" element={<Groups />} />    
            <Route path="/app/rules" element={<Rules />} />         
            <Route path="/app/test" element={<TestSQL />} />                                             
            <Route path="/app/" element={<Sticky />} />          
          </Route>
          <Route  path="/app/logout" element={<Logout/>} />
          <Route path="/app/error" element={<Error />} />
          <Route path="/login" element={<Invitation />} />
          <Route path="*" element={<Error404 />} />
        </Routes>
      </BrowserRouter>

    </div>
  );
}

export default App;
