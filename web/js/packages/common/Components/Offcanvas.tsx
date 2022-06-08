import React, { useEffect, useState, useRef } from 'react';
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'

import './Offcanvas.scss'

export default function Offcanvas(props) {
    let closeIt = () => {
        if(props.onClose != undefined) 
            props.onClose()
    }
    let clo = "bs-canvas-overlay position-fixed  w-100 h-100"
    let cls = "bs-offcanvas bs-offcanvas-anim position-fixed  h-100"
    if(props.show) {
        clo = "bs-canvas-overlay-show position-fixed  w-100 h-100"
        cls = " bs-offcanvas bs-offcanvas-anim position-fixed  h-100 bs-offcanvas-show"
    }
    return (
        <>
            <div id="bs-offcanvas-overlay" className={clo}>
             
            </div>        
            <div className={cls}>
                <div><Row><Col> <h5>{props.title} </h5></Col><Col xs="auto"><i onClick={closeIt} className="cursor-pointer fa fa-times mr-1 fa-2x close" aria-hidden="true"></i></Col></Row></div>
                {props.children}
            </div>
        </>
    )
}