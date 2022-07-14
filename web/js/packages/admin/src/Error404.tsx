import React from 'react';
import { Link } from "react-router-dom";
import Backdrop from "./Backdrop"
import '@dymium/common/App.scss';

export default function Error404() {

  return (
    <div className="py-0 my-0">
      <Backdrop />
      <div className="text-center" style={{
        position: 'absolute', top: '0px', left: '0px',
        width: '100%', height: '100vh'
      }}>
        <div id="loginbox" >
          <div className="row">
            <div className="col-sm ml-5  text-center">
              <h1 style={{ marginTop: '1.3em',fontSize: '5em', fontWeight: '300' }} className="logoheader ">
               <Link className="linklink" to="/" > Dymium </Link></h1>
            </div>
            <div className="col-sm mt-5  text-center">
              <h1 className="mt-5 logoheader">Error 404.</h1>
              <h1 className="logoheader">Page Not Found.</h1>
              <div className="pt-2 logofooter">
                The application is still under construction. Please be patient!
              </div>

            </div>
          </div>

        </div>
      </div>



    </div>
  )

}
