import React from 'react';
import { Navigate } from "react-router-dom";


function Logout() {
    sessionStorage.removeItem('Session')

    return (
        <div>
         <Navigate to="/" replace={true} />
        </div>
    )
}

export default Logout;