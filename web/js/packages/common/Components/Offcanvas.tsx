import React, { useEffect, useState, useRef } from 'react';
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import useGlobalEvent from 'beautiful-react-hooks/useGlobalEvent';

import './Offcanvas.scss'
const initialwidth=600
export default function Offcanvas(props) {
    let [width, setWidth] = useState(initialwidth)
    let [ismousedown, setIsmousedown] = useState(false)

    let closeIt = () => {
        if(props.onClose != undefined) 
            props.onClose()
    }
    let widthRef = useRef(initialwidth)
    let Xorig = useRef(0)
    let Xtrack = useRef(0)

    let onMouseMove = (e) => {
        Xtrack.current = e.clientX
        setWidth(widthRef.current + Xorig.current - Xtrack.current)
        e.preventDefault()
        e.stopPropagation()
    };

    let onMouseUp = (e) => {
        Xtrack.current = e.clientX
        let app = document.getElementById("app");
        app.removeEventListener('mousemove', onMouseMove, true);
        app.removeEventListener('mouseup', onMouseUp, true);
        let nw = widthRef.current + Xorig.current - Xtrack.current
        setWidth(nw)

        widthRef.current = nw
        e.preventDefault()
        e.stopPropagation()
        setIsmousedown(false)        
    };

    let onDown = e => {
        Xorig.current = e.clientX
        let app = document.getElementById("app");
        app.addEventListener('mousemove', onMouseMove, true);
        app.addEventListener('mouseup', onMouseUp, true);
        setIsmousedown(true)        
    }
    let clo = "bs-canvas-overlay position-fixed  w-100 h-100"
    let cls = "bs-offcanvas position-fixed  h-100"
    if(props.show) {
        clo = "bs-canvas-overlay-show position-fixed  w-100 h-100"
    }
    if(!ismousedown) {
        cls = cls + " bs-offcanvas-anim "
    }

    return (
        <>
            <div id="bs-offcanvas-overlay"  className={clo}>
             
            </div>        
            <div className={cls} style={props.show ? {width: width, right: 0} : {width: 0, right: -width}} >
                <div><Row><Col> <h5>{props.title} </h5></Col><Col xs="auto"><i onClick={closeIt} className="cursor-pointer fa fa-times mr-1 fa-2x close" aria-hidden="true"></i></Col></Row></div>
                <Row><Col xs="auto" onMouseDown={onDown} className="offcanvas_handle p-0 m-0 px-2"></Col><Col>{props.children}</Col></Row>
            </div>
        </>
    )
}