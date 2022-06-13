import React, { useEffect, useState, useRef } from 'react';


function Spinner(props) {

    return <>{props.show ? <img src="/logo.png" style={props.style !== undefined ? props.style : {} }className="spinner"></img> :<img src="/logo.png" style={props.style !== undefined ? props.style : {} }className="hiddenspinner"></img> }</>
}

export default Spinner;