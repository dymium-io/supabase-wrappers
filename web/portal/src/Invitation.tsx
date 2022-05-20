import React, { useEffect, useState } from 'react';
import {useLocation} from "react-router-dom";
import Backdrop from "./Backdrop"

function useQuery() {
    const { search } = useLocation();
  
    return React.useMemo(() => new URLSearchParams(search), [search]);
}

function Invitation() {
  let query = useQuery();

  const [login, setLogin] = useState("");

  let fetchData = () => {
    fetch('/api/getlogin', {
      method: 'GET',
      headers: {
        Cache: "no-cache"
      }
    }).then(
      response => {
        if (response.status !== 200) {
          console.log('Looks like there was a problem. Status Code: ' +
            response.status);
          return;
        }
        // Examine the text in the response
        response.json().then(js => {
          setLogin(js.LoginURL)
        });
      }
    )
      .catch(function (err) {
        console.log('Fetch Error :', err);
      });    
  }
  useEffect(() => {
    fetchData()
  }, [])

  return (
    <div>
      <Backdrop />
      <div className="text-center" style={{
        position: 'absolute', top: '0px', left: '0px',
        width: '100%', height: '100vh'
      }}>
        <div id="loginbox">
          <h1 style={{ fontSize: '5em', fontWeight: '300' }} className="logoheader mb-4">Dymium</h1>
          <h4 className="logoheader ">This computer system is intended only for Dymium employees</h4>
          {login === "" ? "" :
            <div className="mt-5">
              <a
                className=" dymium-button-link"

                href={login}

                rel="noopener noreferrer"
              >
                Click to login!
              </a>
            </div>}
        </div>
      </div>
    </div>
  )
}

export default Invitation;