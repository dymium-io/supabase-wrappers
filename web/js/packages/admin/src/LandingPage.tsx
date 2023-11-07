import React from 'react';
import Backdrop from "./Backdrop"

function LandingPage() {

  return (
    <div>
      <Backdrop />
      <div className="text-center" style={{
        position: 'absolute', top: '0px', left: '0px',
        width: '100%', height: '100vh'
      }}>

        <div id="outer" style={{ height: '100vh' }} className="text-center w-100 d-flex align-items-center justify-content-center pt-2">
          <div className="w-100 ">
            <div id="loginb" style={{ width: '30%', marginLeft: 'auto', marginRight: 'auto' }}>

              <img alt="brand" className="text-center mb-5 svgshadow" src="/brand.svg"></img>
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
      </div>
    </div>
  )
}

export default LandingPage;