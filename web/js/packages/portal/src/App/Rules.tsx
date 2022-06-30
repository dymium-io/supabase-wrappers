import React from 'react';
import { Link } from "react-router-dom";
import Tabs from 'react-bootstrap/Tabs'
import Tab from 'react-bootstrap/Tab'


function BuildRules() {
  return (
   <></>
  )
}
export default function Groups() {

  return (
    <Tabs


      unmountOnExit={true} className="mb-3 text-left">

      <Tab eventKey="build" title="Build Rules" className="mx-4">
        <BuildRules />
      </Tab>
    </Tabs>

  )

}