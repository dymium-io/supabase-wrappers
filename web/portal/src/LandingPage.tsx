import React, { useEffect, useState } from 'react';
import Backdrop from "./Backdrop"

function LandingPage() {

  return (
    <div>
      <Backdrop />
      <div className="text-center" style={{
        position: 'absolute', top: '0px', left: '0px',
        width: '100%', height: '100vh'
      }}>
        <div id="loginbox">
          <h1 style={{ marginTop: '0.7em', fontSize: '7em', fontWeight: '300' }} className="logoheader mb-4">Dymium</h1>

          <div className="mt-5">
         
              <a
                className=" dymium-button-link"
             
                href="/auth/login"
      
                rel="noopener noreferrer"
              >
                Click to login!
              </a>
  
          </div>

        </div>
      </div>
    </div>
  )
}

export default LandingPage;