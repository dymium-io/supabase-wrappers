import React from 'react';
import Tabs from 'react-bootstrap/Tabs'
import Tab from 'react-bootstrap/Tab'

function Client() {
    
    return (
        <div style={{ margin: '0px', padding: '0px' }}>
            <iframe title="client" className="embed" src="https://docs.google.com/document/d/1BnlwVB3uQ8bWkL0YhSy0FzpZfj_q_BKwAUWMTHoUCKI/edit?usp=sharing"></iframe>
        </div>
    )}

function Logging() {
    
    return (
        <div style={{ margin: '0px', padding: '0px' }}>
            <iframe title="logging" className="embed" src="https://docs.google.com/document/d/1mWCS9NvjfvTKrBJOueno9SXoZgryz8u0gJyyAbVVVEI/edit?usp=sharing"></iframe>
        </div>
    )}

function Microservices() {
    return (
        <div style={{ margin: '0px', padding: '0px' }}>
            <iframe title="microservices" className="embed" src="https://docs.google.com/document/d/1LveztkbSUWUK6CFm08Nrsm5L7upOp7BsGz2NA3DT1kw/edit?usp=sharing"></iframe>
        </div>
    )
}

function PrivateLink() {
    return (
        <div style={{ margin: '0px', padding: '0px' }}>
            <iframe title="privatelink" className="embed" src="https://docs.google.com/document/d/1iNa3qe2XNDXjiUkwxuYQQzWq_Ct6eoCQGCAXWfMvRXc/edit?usp=sharing"></iframe>
        </div>
    )
}

function Connector() {
    return (
        <div style={{ margin: '0px', padding: '0px' }}>
            <iframe title="connector" className="embed" src="https://docs.google.com/document/d/11Ewg1NBU74_LJamK8TbJMGQsT3OkewTgDptZnkhYi3s/edit?usp=sharing"></iframe>
        </div>
    )
}

function Route53() {
    return (
        <div style={{ margin: '0px', padding: '0px' }}>
            <iframe title="route53" className="embed" src="https://docs.google.com/document/d/1yJWNfuAovcdQCAeSm-sThp8MQt5sm3AlK_1TZXqghPM/edit?usp=sharing"></iframe>
        </div>
    )
}
function Auth0Okta() {
    return (
        <div style={{ margin: '0px', padding: '0px' }}>
            <iframe title="okta" className="embed" src="https://docs.google.com/document/d/1CYW7AFfZByBNDZdhWpAJcLD6uNtHfJuaFuXEQaMsl0o/edit?usp=sharing"></iframe>
        </div>
    )
}
function Auth0Azure() {
    return (
        <div style={{ margin: '0px', padding: '0px' }}>
            <iframe title="azure" className="embed" src="https://docs.google.com/document/d/1U1YZ1xm5n89ch0oQKYhiqIAxzlPUmzlIw-uPlU4BlQM/edit?usp=sharing"></iframe>

        </div>
    )
}

function Help() {

    return (
        <Tabs
            defaultActiveKey="beye"
            id="help"

            unmountOnExit={true} className="text-left mx-0 px-0">
            <Tab eventKey="beye" title="Auth0 and Okta" className="mx-0 helptab">
                <Auth0Okta />
            </Tab>
            <Tab eventKey="aaz" title="Auth0 and AzureAD" className="mx-0 helptab">
                <Auth0Azure />
            </Tab>
            <Tab eventKey="microservices" title="Microservices" className="mx-0 helptab">
                <Microservices />
            </Tab>
            <Tab eventKey="route53" title="Route 53" className="mx-0 helptab">
                <Route53 />
            </Tab>
            <Tab eventKey="connector" title="Connectors" className="mx-0 helptab">
                <Connector />
            </Tab>
            <Tab eventKey="privatelink" title="Private Link" className="mx-0 helptab">
                <PrivateLink />
            </Tab>
            <Tab eventKey="" title="Logging" className="mx-0 helptab">
                <Logging />
            </Tab>            
            <Tab eventKey="client" title="Clients" className="mx-0 helptab">
                <Client />
            </Tab>            
        </Tabs>
    )
}
export default Help;