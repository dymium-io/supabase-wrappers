import React, { useEffect, useState } from 'react';
import { Navigate } from "react-router-dom";
import Tabs from 'react-bootstrap/Tabs'
import Tab from 'react-bootstrap/Tab'
import Spinner from '@dymium/common/Components/Spinner'

function BirdsEye() {
    const [spinner, setSpinner] = useState(false)
    return (
        <>
        <h5 >Birds Eye View <Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>
        Dashboard placeholder
        </>
    )
}

function Dashboard() {

    return (
        <Tabs
            defaultActiveKey="beye"
            id="birdseye"

            unmountOnExit={true} className="mb-3 text-left">
            <Tab eventKey="beye" title="Bird's Eye View" className="mx-4">
                <BirdsEye />
            </Tab>

        </Tabs>
    )
}
export default Dashboard;