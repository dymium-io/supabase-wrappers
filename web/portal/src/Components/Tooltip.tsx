import React from "react";
import OverlayTrigger from 'react-bootstrap/OverlayTrigger'
import Popover from 'react-bootstrap/Popover'

const catchall = text =>
    (props) => {
        let p = { ...props, 'show': 'true' }
        return (
            <Popover id="years-tooltip" className="p-2" {...p} >
                {text}
            </Popover>
        )
    }
export const tooltip = (title, text, placement, cls, nocomma) => {
    if("undefined" === typeof placement)
        placement="bottom"
    let cla = "pb-2"
   
    if("undefined" !== typeof cls) {
        cla = cls
    }
    let last = ""
    if (typeof title === "object" ) {
    } else {
        let split = title.split(' ')
        if(split.length > 1) {
            last = split[split.length - 1]
            title = split.slice(0, split.length -1).join(' ')
        } 
    }
    return (
        <div className={cla}>
            <OverlayTrigger
                trigger={['click' , 'hover' ]} rootClose
                placement={placement} overlay={catchall(text)} >
                <div className=" text-wrap">{title} <span className="text-nowrap">{last}<i className="ms-1 blue fas fa-question-circle text-newinfo"></i>{nocomma? '' : ':'}</span></div>
            </OverlayTrigger>
        </div>)
}
