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
          <img style={{position: 'relative', top: '50%',  marginTop: '-1.8em'}} className="text-center svgshadow" src="/brand.svg"></img>

        <div id="loginbox">
        
          <div style={{marginTop: '11em'}}>
         
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