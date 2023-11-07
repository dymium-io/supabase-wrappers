import React from 'react';
import LandingPage from './LandingPage'
import AuthenticatedApp from './App/AuthenticatedApp'
import Error404 from "./Error404"
import Logout from "./Logout"
import Error from "./Error"
import Sticky from "./App/Sticky"
import Dashboard from "./App/Dashboard"
import Customers from "./App/Customers"
import Help from "./App/Help"
import "@fontsource/roboto";
import 'bootstrap/dist/css/bootstrap.min.css';

import { BrowserRouter, Routes, Route } from "react-router-dom";
//import Helmet from "react-helmet";

import '@dymium/common/App.scss';

function App() {
  return (
    <div className="App">

      <BrowserRouter>
        <Routes>

          <Route  path="/" element={<LandingPage />} />

          <Route path="/app/" element={<AuthenticatedApp />} >
            <Route path="/app/dashboard" element={<Dashboard />} />
            <Route path="/app/customers" element={<Customers />} />
            <Route path="/app/help" element={<Help />} />
            <Route path="/app/" element={<Sticky />} />   
          </Route>
          <Route  path="/app/logout" element={<Logout/>} />
          <Route path="/app/error" element={<Error />} />

          <Route path="*" element={<Error404 />} />
        </Routes>
      </BrowserRouter>

    </div>
  );
}

export default App;
