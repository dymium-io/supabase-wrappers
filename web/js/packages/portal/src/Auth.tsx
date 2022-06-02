import React, { useState, useEffect, useRef } from 'react';
import Modal from 'react-bootstrap/Modal'
import Button from 'react-bootstrap/Button'
import {useInitialize} from './Utils/CustomHooks'
const sessionTimeoutMin = 20



export default function Auth() {
    let active = useRef(false)
    let [show, setShow] = useState(false)
    let lastSessionUpdate = useRef( Date.now()/1000 )
    let showtimer = useRef(0)
    let  timeoutId = useRef(0)

    let redirectPublicWebsite = () => {
        return "/app/logout"
    }
    let revalidate = () => {
        if(!active.current)
            return
        let token = window.sessionStorage.getItem("Session");
        lastSessionUpdate.current = Date.now()/1000                       
        if (token === null) {
            console.log("not authenticated");
            window.location.href = redirectPublicWebsite();
            return;
        }
        fetch(window.location.origin + "/auth/refresh", {
            cache: 'no-cache',
            method: 'POST',
            body: "",            
            headers: {
                Authorization: "Bearer " + token,
                Cache: "no-cache"                      
            },
        }).then(response => {
            let d = new Date()
    
            if (!response.ok) {
                console.log("Auth fetch error, at: " + d.toLocaleString())
                sessionStorage.removeItem("Session")
                setShow( true )
            } else {
                response.json().then(js => {
                    if (js.Status !== "OK") {
                        console.log("Status not OK,  " + JSON.stringify(js) + ", at: " + d.toLocaleString())
                        sessionStorage.removeItem("Session")
                        console.log("Pop the message")
                        showtimer.current = window.setTimeout(t => { 
                            console.log("Don't wait until popup shows, close the view")
                            window.location.href = redirectPublicWebsite()
                        }, 30000)
                        if(window.location.pathname !== '/')
                            setShow(true )
                    } else {
                        sessionStorage.setItem("Session", js.Token)
                    }
                })
            }
        }).catch(error => {
            let d = new Date()
            console.log("Revalidate catch error " + error + ", at: " + d.toLocaleString())
        })
    }
    let onTimeout = () => {
        let d = new Date()
        console.log("in onTimeout, revalidate possible expiration, %s", d.toLocaleString() )
        console.log("Elapsed: " + (Date.now()/1000 - lastSessionUpdate.current))
        revalidate();
    }

    let onActivity = (e) => {
        //console.log("in onActivity")
        let now = Date.now()/1000 
        if (now - lastSessionUpdate.current >= 30) {
     
            lastSessionUpdate.current =now            

            revalidate();
        }
  
        clearTimeout(timeoutId.current)
        timeoutId.current = window.setTimeout(onTimeout, 1000*(sessionTimeoutMin*60 + 5))

    }
    let onReauthenticate = (e) =>{
        console.log("Reauthentication requested on failed fetch!")
        revalidate()
    }    
    useInitialize( () => {
        lastSessionUpdate.current = 0
        showtimer.current = 0
        window.addEventListener("mousemove", onActivity, {passive: true} )
        window.addEventListener("keydown", onActivity, {passive: true} )
        window.addEventListener("wheel", onActivity, {passive: true} )
        window.addEventListener("DOMMouseScroll", onActivity, {passive: true} )
        window.addEventListener("mouseWheel", onActivity, {passive: true} )
        window.addEventListener("mousedown", onActivity, {passive: true} )
        window.addEventListener("touchstart", onActivity, {passive: true} )
        window.addEventListener("touchmove", onActivity, {passive: true} )
        window.addEventListener("MSPointerDown", onActivity, {passive: true} )
        window.addEventListener("MSPointerMove", onActivity, {passive: true} )
        window.addEventListener("visibilitychange", onActivity, {passive: true} )

        window.document.addEventListener('reauthenticate', onReauthenticate);
    })

    useEffect(() => {
        
        if(timeoutId.current)
            clearTimeout(timeoutId.current)
        timeoutId.current = window.setTimeout(onTimeout, 1000*(sessionTimeoutMin*60 + 5))
        revalidate();
        active.current = true
        handleShow()
        return () => {
            active.current = false
            if(timeoutId.current)
                clearTimeout(timeoutId.current)
            timeoutId.current = 0            
        }

    }, [])

    let onShow = () => {
        if(showtimer.current)
            clearTimeout(showtimer.current)
        showtimer.current = 0
        window.setTimeout(t => { 
            console.log("kill the popup from popup")
            window.location.href = redirectPublicWebsite()
        }, 5000)
    }
    let handleClose = () => { setShow(true ); window.location.href = redirectPublicWebsite(); }
    let handleShow = () => { setShow(false); }
    return (
        <div className="mx-0 px-0">
            <Modal show={show} onHide={handleClose} onShow={onShow}>
                <Modal.Header closeButton>
                    <Modal.Title>Session expired!</Modal.Title>
                </Modal.Header>
                <Modal.Body>This session timed out because of inactivity</Modal.Body>
                <Modal.Footer>
                    <Button variant="secondary" onClick={handleClose}>Close</Button>
                </Modal.Footer>
            </Modal>
        </div>
    )

}

