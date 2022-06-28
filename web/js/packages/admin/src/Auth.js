import React, { Component } from 'react';
import Modal from 'react-bootstrap/Modal'
import Button from 'react-bootstrap/Button'

const sessionTimeoutMin = 20

export default class Auth extends Component {
    constructor(props) {
        super(props);
        this.state = {
            show: false,

            active: false,
        }
        
        this.lastActivity = Date.now()/1000
        this.lastSessionUpdate = 0
        this.showtimer = null
        window.addEventListener("mousemove", this.onActivity, {passive: true} )
        window.addEventListener("keydown", this.onActivity, {passive: true} )
        window.addEventListener("wheel", this.onActivity, {passive: true} )
        window.addEventListener("DOMMouseScroll", this.onActivity, {passive: true} )
        window.addEventListener("mouseWheel", this.onActivity, {passive: true} )
        window.addEventListener("mousedown", this.onActivity, {passive: true} )
        window.addEventListener("touchstart", this.onActivity, {passive: true} )
        window.addEventListener("touchmove", this.onActivity, {passive: true} )
        window.addEventListener("MSPointerDown", this.onActivity, {passive: true} )
        window.addEventListener("MSPointerMove", this.onActivity, {passive: true} )
        window.addEventListener("visibilitychange", this.onActivity, {passive: true} )

        window.document.addEventListener('reauthenticate', this.onReauthenticate);

    }
    redirectPublicWebsite() {
        return "/app/logout"
    }
    onReauthenticate = (e) =>{
        console.log("Reauthentication requested on failed fetch!")
        this.revalidate()
    }
    onTimeout = () => {
        let d = new Date()
        console.log("in onTimeout, revalidate possible expiration, %s", d.toLocaleString() )
        console.log("Elapsed: " + (Date.now()/1000 - this.lastSessionUpdate))
        this.revalidate();
    }
    onActivity = (e) => {
        //console.log("in onActivity")
        let now = Date.now()/1000 
        if (now - this.lastSessionUpdate >= 30) {
     
            this.lastSessionUpdate =now            

            this.revalidate();
        }

        this.lastActivity = now
        clearTimeout(this.timeoutId)
        this.timeoutId = setTimeout(this.onTimeout, 1000*(sessionTimeoutMin*60 + 5))

    }
    revalidate () {
        if(!this.active)
            return
        let token = window.sessionStorage.getItem("Session");
        this.lastSessionUpdate = Date.now()/1000                       
        if (token === null) {
            console.log("not authenticated");
            window.location.href = this.redirectPublicWebsite();
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
                this.setState({ show: true })
            } else {
                response.json().then(js => {
                    if (js.status !== "OK") {
                        console.log("Status not OK,  " + JSON.stringify(js) + ", at: " + d.toLocaleString())
                        sessionStorage.removeItem("Session")
                        console.log("Pop the message")
                        this.showtimer = setTimeout(t => { 
                            console.log("Don't wait until popup shows, close the view")
                            window.location.href = this.redirectPublicWebsite()
                        }, 30000)
                        if(window.location.pathname !== '/')
                            this.setState({ show: true })
                    } else {
                        sessionStorage.setItem("Session", js.token)
                    }
                })
            }
        }).catch(error => {
            let d = new Date()
            console.log("Revalidate catch error " + error + ", at: " + d.toLocaleString())
        })
    }

    componentDidMount() {
        if(this.timeoutId)
            clearTimeout(this.timeoutId)
        this.timeoutId = setTimeout(this.onTimeout, 1000*(sessionTimeoutMin*60 + 5))

        this.active=true
        this.revalidate();
  
        this.handleShow()
    }
    componentWillUnmount() {
        if(this.timeoutId)
            clearTimeout(this.timeoutId)
        this.timeoutId = null            
        this.active=false

    }
    onShow = () => {
        if(this.showtimer)
            clearTimeout(this.showtimer)
        this.showtimer = null
        setTimeout(t => { 
            console.log("kill the popup from popup")
            window.location.href = this.redirectPublicWebsite()
        }, 5000)
    }
    handleClose = () => { this.setState({ show: true }); window.location.href = this.redirectPublicWebsite(); }
    handleShow = () => this.setState({ show: false });
    render() {
        return (
            <div className="mx-0 px-0">
                <Modal show={this.state.show} onHide={this.handleClose} onShow={this.onShow}>
                    <Modal.Header closeButton>
                        <Modal.Title>Session expired!</Modal.Title>
                    </Modal.Header>
                    <Modal.Body>This session timed out because of inactivity</Modal.Body>
                    <Modal.Footer>
                        <Button variant="secondary" onClick={this.handleClose}>Close</Button>
                    </Modal.Footer>
                </Modal>
            </div>
        )
    }
}

