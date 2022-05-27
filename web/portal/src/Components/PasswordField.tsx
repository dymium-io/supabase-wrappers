import Form from 'react-bootstrap/Form';
import Button from 'react-bootstrap/Button';
import InputGroup from 'react-bootstrap/InputGroup'
import { useState, useRef } from 'react';

export default function PasswordField(props) {
    const [eye, setEye] = useState(true);
    let input = useRef<HTMLInputElement>()

    let toggle = e => {
        if (input.current == undefined)
            return
        console.log(input.current["type"])
        if (input.current["type"] === 'password') {
            setEye(false)
            input.current.type = "text"
        } else {
            setEye(true)
            input.current.type = "password"
        }
        // input.current.focus()
    }
    let handleOnFocus = (e: React.FocusEvent) => {
        e.stopPropagation()
        return false
    }


    let _props = { ...props }
    let validfeedback = props.validfeedback
    let invalidfeedback = props.invalidfeedback
    let cn =  props.className
    delete _props.validfeedback
    delete _props.invalidfeedback
    delete _props.className

    return (
        <InputGroup className={cn}>

            <Form.Control className="no-background" {..._props} style={{ display: 'inline', width: '100%' }} ref={input} />

           {eye ? <i onClick={toggle} className="blue fa fa-eye  fa-fw" style={{ zIndex: '1000', marginTop: '6px', marginLeft: '-25px' }}/> : 
           <i onClick={toggle} className="blue fa fa-eye-slash  fa-fw" style={{ zIndex: '1000', marginTop: '6px', marginLeft: '-25px' }}/> }

            <Form.Control.Feedback >{validfeedback}</Form.Control.Feedback>
            <Form.Control.Feedback style={{ width: '12em' }} type="invalid" >{invalidfeedback}</Form.Control.Feedback>

        </InputGroup>

    );

}