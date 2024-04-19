import React, { useEffect } from 'react';
import LandingPage from './LandingPage'
import AuthenticatedApp from './App/AuthenticatedApp'
import Error404 from "./Error404"
import Logout from "./Logout"
import Error from "./Error"
//import Invitation from "./Invitation"
import Sticky from "./App/Sticky"
import Signup, {IdP} from "./App/Signup"
import Dashboard from "./App/Dashboard"
import Customers from "./App/Customers"
import Connections from "./App/Connections"
import Datascopes from "./App/Datascopes"
import Connectors from "./App/Connectors"
import Groups from "./App/Groups"
import Rules from "./App/Rules"
import Access from "./App/Access"
import TestSQL from "./App/TestSQL"
import MachineAccess from "./App/MachineAccess"
import * as com from './Common'

import "@fontsource/roboto";
import 'bootstrap/dist/css/bootstrap.min.css';
import 'react-bootstrap-typeahead/css/Typeahead.css';

import { BrowserRouter, Routes, Route } from "react-router-dom";
//import Helmet from "react-helmet";

import '@dymium/common/App.scss';


function App() {
  return (
    <div className="App" id="app">

      <BrowserRouter>
        <Routes>

          <Route  path="/" element={<LandingPage />} />

          <Route path="/app/" element={<AuthenticatedApp />} >
            <Route path="/app/signup" element={<Signup />} />
            <Route path="/app/dashboard" element={<Dashboard />} />
            <Route path="/app/idp" element={<IdP />} />
            <Route path="/app/connections" element={<Connections />} />     
            <Route path="/app/datascopes" element={<Datascopes />} />     
            <Route path="/app/connectors/*" element={<Connectors />} />                
            <Route path="/app/groups" element={<Groups />} />    
            <Route path="/app/rules" element={<Rules />} />   
            <Route path="/app/access" element={<Access />} />      
            <Route path="/app/machineaccess/*" element={<MachineAccess />} />      
            <Route path="/app/test" element={<TestSQL />} />                                             
            <Route path="/app/" element={<Sticky />} />          
          </Route>
          <Route  path="/app/logout" element={<Logout/>} />
          <Route path="/app/error" element={<Error />} />
          {/*
          <Route path="/login" element={<Invitation />} />
           */}
          <Route path="*" element={<Error404 />} />
        </Routes>
      </BrowserRouter>

    </div>
  );
}

export default App;
