import React, { useEffect, useState } from 'react';
import Backdrop from "./Backdrop"

function LandingPage() {
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
          <h1 style={{ marginTop: '0.7em', fontSize: '7em', fontWeight: '300' }} className="logoheader mb-4">Dymium</h1>

          <div className="mt-5">
            {login !== "" &&
              <a
                className=" dymium-button-link"
             
                href={login}
      
                rel="noopener noreferrer"
              >
                Click to login!
              </a>
            }
          </div>

        </div>
      </div>
    </div>
  )
}

export default LandingPage;