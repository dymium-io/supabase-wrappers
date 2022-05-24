import Form from 'react-bootstrap/Form';
import Button from 'react-bootstrap/Button';
import InputGroup from 'react-bootstrap/InputGroup'
import { useState, useRef } from 'react';

export default function PasswordField(props) {
    const [eye, setEye] = useState(<i className="fa fa-eye" aria-hidden="true"></i>);
    let input = useRef<HTMLInputElement>()
    let openeye = <i className="fa fa-eye" aria-hidden="true"  ></i>
    let closeeye = <i className="fa fa-eye-slash"  ></i>

    let tp = openeye
    if(input.current !== undefined) {
        if(input.current["type"] === 'password')
            tp = closeeye
    }
    let toggle = e => {
        if(input.current == undefined)
            return 
        console.log(input.current["type"])
        if(input.current["type"] === 'password') {
            setEye(closeeye)
            input.current.type = "text"
        } else {
            setEye(openeye)
            input.current.type = "password"
        }
        // input.current.focus()
    }
    let handleOnFocus = (e: React.FocusEvent) => {
        e.stopPropagation()
        return false
    }
    return(
        <InputGroup>
            <Form.Control {...props} ref={input}/>
            <span onClick={toggle} style={{zIndex:'1000', marginTop:'4px', marginLeft:'-25px'}}>{ eye }</span>
        </InputGroup>
    );

}