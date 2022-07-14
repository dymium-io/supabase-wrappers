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
        <img style={{position: 'relative', top: '50%', marginTop: '12em'}} className="text-center mb-5 svgshadow" src="/brand.svg"></img>
          <h4 className="logoheader ">This computer system is intended only for Dymium employees</h4>
   
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