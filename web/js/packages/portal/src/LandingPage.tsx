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
        <div className="text-center w-100 d-flex align-items-center justify-content-center pt-2" >
          <div id="loginbox" style={{width: '30%', paddingTop: '100%'}}>
            <img style={{ position: 'relative', top: '70%', marginTop: '12em' }} className="text-center mb-5 svgshadow" src="/brand.svg"></img>
           
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
    </div>
  </div>
  )
}

export default LandingPage;