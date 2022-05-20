import React, { useEffect } from 'react';
import LandingPage from './LandingPage'
import AuthenticatedApp from './app/AuthenticatedApp'
import Error404 from "./Error404"
import Logout from "./Logout"
import Error from "./Error"
import Invitation from "./Invitation"
import Dashboard from "./app/Dashboard"
import Customers from "./app/Customers"
import "@fontsource/roboto";
import 'bootstrap/dist/css/bootstrap.min.css';

import { BrowserRouter, Routes, Route } from "react-router-dom";
//import Helmet from "react-helmet";

import './App.css';

function App() {
  return (
    <div className="App">

      <BrowserRouter>
        <Routes>

          <Route  path="/" element={<LandingPage />} />

          <Route path="/app/" element={<AuthenticatedApp />} >
            <Route path="/app/dashboard" element={<Dashboard />} />
            <Route path="/app/customers" element={<Customers />} />
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
