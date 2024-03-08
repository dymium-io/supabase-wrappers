import React, { useEffect, useState, useRef, FunctionComponent } from 'react';
import Button from 'react-bootstrap/Button'
import ListGroup from 'react-bootstrap/ListGroup'
import Modal from 'react-bootstrap/Modal'
import Spinner from '@dymium/common/Components/Spinner'
import Alert from 'react-bootstrap/Alert'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Form from 'react-bootstrap/Form'
import Tabs from 'react-bootstrap/Tabs'
import Tab from 'react-bootstrap/Tab'
import { useAppDispatch, useAppSelector } from './hooks'
import { setActiveSuperTab } from '../Slices/menuSlice'
import * as com from '../Common'

let discovery_url = ""
let authorization_endpoint = ""
let token_endpoint = ""
let userinfo_endpoint = ""
let jwks_uri = ""
let audience = ""

interface SetterState {
    idp: string;
    issuer: string;
    clientid: string;
    secret: string;
    shortname: string;
    name: string;
    domain: string;
    logo: string;
    fore: string;
    back: string;
    admins: string[];
}
interface StepProps {
    state: SetterState;
    setState: React.Dispatch<React.SetStateAction<SetterState>>;
    onNext?: (i: () => Promise<boolean>) => void; // Optional because not all steps might use it
    onBack?: (i: () => Promise<boolean>) => void; // Optional because not all steps might use it
    left?: boolean;
}

interface StepProgressBarProps {
    steps: string[]; // Array of step names
    currentStep: number; // Current step index (0-based)
    onStep: (index: number) => void; // Optional click handler
}

const StepProgressBar: React.FC<StepProgressBarProps> = ({ steps, currentStep, onStep }) => {
    const width = `${(100 / (steps.length - 1)) * currentStep}%`;

    let fillclass = (index) => {
        if (index == currentStep) {
            return 'stepCircleCurrent'
        }
        if (index <= currentStep)
            return 'stepCircleComplete'
        else
            return 'stepCircleIncomplete'
    }

    return (
        <div className="progressContainer mb-5">
            <div className="stepBar">
                <div className="stepBarBaseLine"></div>
                <div style={{ width: width }} className="stepConnector"></div>
                {steps.map((step, index) => (
                    <div key={index} style={{ position: 'relative', zIndex: 1 }}>
                        <div onClick={
                            (e) => {
                                if (index < currentStep)
                                    onStep(index)
                            }

                        } className={`stepCircle ${index <= currentStep ? 'stepCircleComplete' : 'stepCircleIncomplete'}`}>
                            {index < currentStep ? (
                                <i className="fa-solid fa-check" style={{ color: 'white' }}></i>
                            ) : (
                                <span className="stepNumber" style={(index <= currentStep) ? { color: 'white' } : {}}>{index + 1}</span>
                            )}
                        </div>
                        {index == currentStep ?
                            <div className="stepLabel "><b><Button onClick={e => onStep(index)} variant="link">{step}</Button></b></div>
                            : ((index < currentStep) ?
                                <div className="stepLabel"><Button onClick={e => onStep(index)} variant="link">{step}</Button></div>
                                : <div className="stepLabel">{step}</div>)

                        }
                    </div>
                ))}
            </div>
        </div>
    );
};

const StepThree: FunctionComponent<StepProps> = ({ onNext, state, setState, left }) => {
    const [validated, setValidated] = useState(false)
    const [alert, setAlert] = useState<JSX.Element>(<></>)

    let form = useRef<HTMLFormElement>(null)

    useEffect(() => {
        document.title = 'Signup';
    }, []);

    let test = async () => {
        if (state.issuer === "" || state.clientid === "" || state.secret === "") {
            return true
        }
        let jsb = {
            issuer: state.issuer,
            clientid: state.clientid,
            secret: state.secret
        }
        let token = window.sessionStorage.getItem("Session");

        let response = await fetch("/api/testoidc",
            {
                method: 'POST',
                headers: {
                    Authorization: "Bearer " + token,
                    Cache: "no-cache",
                    ContentType: 'application/json'
                },
                body: JSON.stringify(jsb)
            }
        )
        if (response.status !== 200) {
            let t = await response.text()
            setAlert(
                <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                    {t}
                </Alert>
            )
            return false
        }
        if (!response.ok) {
            return false
        }
        let js = await response.json()
        if (js.errorCode !== undefined) {
            setAlert(
                <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                    {js.errorSummary}
                </Alert>
            )
            return false
        }
        return true
    }

    onNext!(test)
    return (
        <div className="ml-3 mt-3 ">
            {alert}
            <div className="text-left" style={left ? { width: '41em' } : { width: '41em', margin: 'auto' }}>
                <Row className="mt-2">
                    <Col>
                        <Form.Group controlId="issuer">
                            <Form.Label>Issuer URL</Form.Label>
                            <Form.Control size="sm" type="text" placeholder="" value={state.issuer}
                                onChange={e => setState({ ...state, issuer: e.target.value })} style={{ width: '40em' }} required />
                            <Form.Control.Feedback type="invalid">
                                Please provide a valid issuer URL.
                            </Form.Control.Feedback>
                        </Form.Group>
                    </Col>
                </Row>

                <Row className="mt-2">
                    <Col>
                        <Form.Group controlId="clientid">
                            <Form.Label>Client ID</Form.Label>
                            <Form.Control size="sm" type="text" placeholder="" value={state.clientid}
                                onChange={e => setState({ ...state, clientid: e.target.value })} style={{ width: '40em' }} required />
                            <Form.Control.Feedback type="invalid">
                                Please provide a valid client ID.
                            </Form.Control.Feedback>
                        </Form.Group>
                    </Col>
                </Row>
                <Row className="mt-2">
                    <Col xs="auto">
                        <Form.Group controlId="secret">
                            <Form.Label>Client Secret</Form.Label>
                            <Form.Control size="sm" type="text" placeholder="" value={state.secret} onChange={e => setState({ ...state, secret: e.target.value })} style={{ width: '40em' }} required />
                            <Form.Control.Feedback type="invalid">
                                Please provide a valid client secret.
                            </Form.Control.Feedback>
                        </Form.Group>
                    </Col>
                    <Col ></Col>
                </Row>
            </div>
        </div>
    );
}

// Example Step Components
const StepOne: FunctionComponent<StepProps> = ({ onNext, state, setState }) => {
    return <>
        <div className="ml-3 mt-3">
            <div >
                <Form.Group controlId="idp" >
                    <Form.Label>Please select which Identity Provider is used by your company</Form.Label>
                    <Form.Control size="sm"
                        as="select"
                        style={{ width: '20em', display: 'block', margin: '0 auto' }}
                        id="idpchoice"
                        required
                        onChange={e => setState({ ...state, idp: e.target.value })}
                        value={state.idp}
                    >
                        <option value="">...</option>
                        <option value="okta">Okta</option>
                        <option value="ping">Ping</option>
                        <option value="azure">Azure</option>
                    </Form.Control>
                </Form.Group>
            </div>

        </div>
    </>
};

const StepFour: FunctionComponent<StepProps> = ({ onNext, state, setState, left }) => {
    const [alert, setAlert] = useState<JSX.Element>(<></>)

    let checkNext = async () => {
        setAlert(
            <></>
        )
        let jsb = {
            shortname: state.shortname,
            name: state.name,
            logo: state.logo
        }
        let token = window.sessionStorage.getItem("Session");

        let response = await fetch("/api/testnameandlogo",
            {
                method: 'POST',
                headers: {
                    Authorization: "Bearer " + token,
                    Cache: "no-cache",
                    ContentType: 'application/json'
                },
                body: JSON.stringify(jsb)
            }
        )

        if (response.status !== 200) {
            let text = await response.text()
            setAlert(
                <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                    {text}
                </Alert>
            )
            return false
        }

        if (!response.ok) {
            return false
        }

        let js = await response.json()
        if (js.errorCode !== undefined) {
            setAlert(
                <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                    {js.errorSummary}
                </Alert>
            )
            return false
        }

        return true
    }
    onNext!(checkNext)
    return <div style={{ width: '100%' }} className="mx-3 mt-2 ">
        {alert}
        <div className="text-left" style={left ? { width: '42em' } : { width: '42em', margin: 'auto' }}>
            {!left &&
                <>
                    <Row className="mt-1">
                        <Col>
                            <Form.Group controlId="formname">
                                <Form.Label>Short company/group name. Must be globally unique.</Form.Label>
                                <Form.Control size="sm" type="text" placeholder="alphanumeric" style={{ width: '40em' }}
                                    value={state.shortname}
                                    pattern="[a-z][a-z0-9]*"
                                    onChange={e => setState({ ...state, shortname: e.target.value })}
                                    required />
                                <Form.Control.Feedback type="invalid">
                                    Please provide a valid name.
                                </Form.Control.Feedback>
                            </Form.Group>
                        </Col>
                    </Row>
                    <Row className="mt-2">
                        <Col>
                            <Form.Group controlId="humanreadablename">
                                <Form.Label>Human readable company/group name. Must be globally unique.</Form.Label>
                                <Form.Control size="sm" type="text" placeholder="" style={{ width: '40em' }}
                                    value={state.name}
                                    onChange={e => setState({ ...state, name: e.target.value })}

                                    required />
                                <Form.Control.Feedback type="invalid">
                                    Please provide a valid human readable name.
                                </Form.Control.Feedback>
                            </Form.Group>
                        </Col>
                    </Row>
                </>
            }
            <Row className="mt-2">
                <Col>
                    <Form.Group controlId="domain">
                        <Form.Label>Domain as used in company's emails (like xyz.com) </Form.Label>
                        <Form.Control size="sm" type="text" placeholder="" style={{ width: '40em' }}
                            value={state.domain}
                            onChange={e => setState({ ...state, domain: e.target.value })}
                            required
                            pattern="[a-zA-Z0-9]+\.[a-zA-Z]{2,}"
                        />
                        <Form.Control.Feedback type="invalid">
                            Please provide a valid domain.
                        </Form.Control.Feedback>
                    </Form.Group>
                </Col>
            </Row>
            <Row className="mt-2">
                <Col>
                    <Form.Group controlId="logo">
                        <Form.Label>Logo URL </Form.Label>
                        <Form.Control size="sm" type="text" placeholder="" style={{ width: '40em' }}
                            value={state.logo}
                            onChange={e => setState({ ...state, logo: e.target.value })}
                            pattern="^(http:\/\/|https:\/\/).+$"
                            required />
                        <Form.Control.Feedback type="invalid">
                            Please provide a valid URL to a gif, png or jpg image.
                        </Form.Control.Feedback>
                    </Form.Group>
                </Col>
                <Col><img src={state.logo} onError={e=>(e.currentTarget.style.display = 'none')} style={{height: '48px', position: 'absolute', bottom: '0px'}}></img></Col>
            </Row>
            <Row className="mt-2">
                <Col xs="auto">
                    <Form.Control size="sm"
                        type="color"
                        id="fore"
                        value={state.fore}
                        onChange={e => setState({ ...state, fore: e.target.value })}
                        title="Choose your color"
                    />
                </Col><Col>
                    <Form.Label style={{ marginTop: '7px' }} htmlFor="exampleColorInput">Primary color for login box</Form.Label>
                </Col>
            </Row>
            <Row className="mt-2">
                <Col xs="auto">
                    <Form.Control size="sm"
                        type="color"
                        id="back"
                        value={state.back}
                        onChange={e => setState({ ...state, back: e.target.value })}
                        title="Choose your color"
                    />
                </Col><Col>
                    <Form.Label style={{ marginTop: '7px' }} htmlFor="exampleColorInput"> Background color for login box</Form.Label>
                </Col>
            </Row>
        </div>
    </div>
};

const StepTwo: FunctionComponent<StepProps> = ({ state }) => {
    return <>
        <div className="ml-3 mt-3 ">
            <div className="text-left" style={{ width: '41em', margin: 'auto' }}>
                Let's configure a new App on your Identity Provider.
            </div>
        </div>
    </>
};

const AdminEmails: FunctionComponent<StepProps> = ({ state, setState, left }) => {
    const [newEmail, setNewEmail] = useState<string>('');
    const [showModal, setShowModal] = useState<boolean>(false);
    const [editIndex, setEditIndex] = useState<number | null>(null);
    const [editEmail, setEditEmail] = useState<string>('');
    const [validated, setValidated] = useState(false);

    const handleAddEmail = () => {
        const updatedAdmins = [...state.admins, newEmail.trim()];
        setState(prevState => ({ ...prevState, admins: updatedAdmins }));
        setNewEmail('');
    };

    const handleRemoveEmail = (index: number) => {
        const updatedAdmins = state.admins.filter((_, i) => i !== index);
        setState(prevState => ({ ...prevState, admins: updatedAdmins }));
    };

    const handleShowEditModal = (index: number) => {
        setEditIndex(index);
        setEditEmail(state.admins[index]);
        setShowModal(true);
    };

    const handleEditEmail = () => {
        if (editIndex !== null) {
            const updatedAdmins = state.admins.map((email, i) =>
                i === editIndex ? editEmail : email
            );
            setState(prevState => ({ ...prevState, admins: updatedAdmins }));
            handleCloseModal();
        }
    };

    const handleCloseModal = () => {
        setShowModal(false);
        setEditIndex(null);
        setEditEmail('');
    };

    let invitee = com.getTokenProperty("email")
    let pass = () => {
        let r = new RegExp("[^@]+@[^@]+\.[a-zA-Z]{2,}$")
        return r.test(newEmail)
    }
    return (<div style={{ width: '100%' }} className="mx-3 mt-2 ">
        <div className="text-left" style={left ? { width: '42em' } : { width: '42em', margin: 'auto' }}>

            <Row>
                <Col xs="auto" className=" pr-0">
                    <Form.Group>
                        <Form.Label>Add Super Admin:</Form.Label>
                        <Form.Control size="sm"
                            type="email"
                            style={{ width: '36em' }}
                            placeholder="Enter email"

                            value={newEmail}
                            onChange={(e) => setNewEmail(e.target.value)}
                        /> </Form.Group>
                </Col>
                <Col xs="auto" className="pl-0">
                    <Button size="sm" style={{ marginTop: '1.85em' }} variant="dymium" onClick={handleAddEmail} disabled={!pass()}>
                        Add
                    </Button>
                </Col>

            </Row>


            <ListGroup className="mt-5 ">
                <div style={{ marginBottom: '3px' }}>Super Admins:</div>
                {state.admins.map((email, index) => (
                    <ListGroup.Item key={index} style={{ border: 'none', backgroundColor: (index % 2) ? 'rgba(210,223,240, 0.6)' : 'rgba(255,255,255, 0.7)' }} className="d-flex justify-content-between align-items-center p-1">
                        {email}
                        {email !== invitee &&
                            <div className="my-0">
                                <i className="fas fa-edit ablue" onClick={() => handleShowEditModal(index)} ></i>
                                <i className="ml-1 fa-solid fa-trash ablue" onClick={() => handleRemoveEmail(index)}></i>
                            </div>
                        }
                    </ListGroup.Item>
                ))}
            </ListGroup>

            <Modal show={showModal} onHide={handleCloseModal}>
                <Modal.Header closeButton>
                    <Modal.Title>Edit Email</Modal.Title>
                </Modal.Header>
                <Modal.Body>
                    <Form.Control size="sm"
                        type="email"
                        value={editEmail}
                        onChange={(e) => setEditEmail(e.target.value)}
                    />
                </Modal.Body>
                <Modal.Footer>
                    <Button size="sm" variant="secondary" onClick={handleCloseModal}>
                        Close
                    </Button>
                    <Button size="sm" variant="primary" onClick={handleEditEmail}>
                        Save Changes
                    </Button>
                </Modal.Footer>
            </Modal>
        </div>
    </div>
    );
};

const steps = [StepOne, StepTwo, StepThree, StepFour, AdminEmails];
const stepnames = ["IdP type", "IdP configuration", "IdP integration", "Login configuration", "Super Admins"];


let onnextdefault = () => { return true }
let onnext = onnextdefault
let next: (() => Promise<boolean>) | null = null
let timer: NodeJS.Timeout

const Signup: FunctionComponent = () => {
    const [currentStep, setCurrentStep] = useState(0);
    const [validated, setValidated] = useState(false);
    const [status, setStatus] = useState<string[]>([])
    const [invitationstatus, setInvitationStatus] = useState<string>("")

    const [success, setSuccess] = useState<string>("")
    const [state, setState] = useState<SetterState>(
        {
            idp: "",
            issuer: "",
            clientid: "",
            secret: "",
            shortname: "",
            name: "",
            domain: "",
            logo: "",
            fore: "",
            back: "",
            admins: []
        }
    );
    const formRef = useRef<HTMLFormElement>(null);
    const funcRef = useRef<() => boolean>(() => true)
    let getInvitationStatus = () => {
        // 
        let token = window.sessionStorage.getItem("Session");
        fetch("/api/invitationstatus",
            {
                method: 'GET',
                headers: {
                    Authorization: "Bearer " + token,
                    Cache: "no-cache",
                }
            }
        )
            .then(response => {
                if (!response.ok) {
                    throw new Error('Network response was not ok');
                }
                return response.json();
            })
            .then(data => {
                setInvitationStatus("completed")
                console.log(data)

            })
            .catch(error => {
                setInvitationStatus("open")
                console.error('There has been a problem with your fetch operation:', error);
            });
    }
    useEffect(() => {
        getInvitationStatus()
        setSuccess("")
        setStatus([])
        getJson()
    }, []);

    let getJson = () => {
        let token = window.sessionStorage.getItem("Session");
        fetch("/api/getinvitationjson",
            {
                method: 'GET',
                headers: {
                    Authorization: "Bearer " + token,
                    Cache: "no-cache",

                }
            }
        )
            .then(response => {
                if (!response.ok) {
                    throw new Error('Network response was not ok');
                }
                return response.json();
            })
            .then(data => {
                if (data.admins === undefined) {
                    data = state
                    data.fore = "#00527c"
                    data.back = "#fc8f24"
                }
                if (data.admins.length === 0) {
                    data.admins.push(com.getTokenProperty("email"))
                }
                setState({ ...state, ...data })
                console.log(data)

            })
            .catch(error => {
                console.error('There has been a problem with your fetch operation:', error);
            });
    }

    let postJson = () => {
        let token = window.sessionStorage.getItem("Session");
        fetch("/api/postinvitationjson",
            {
                method: 'POST',
                headers: {
                    Authorization: "Bearer " + token,
                    Cache: "no-cache",
                    ContentType: 'application/json'
                },
                body: JSON.stringify(state)
            }
        )
            .then(response => {
                if (!response.ok) {
                    throw new Error('Network response was not ok');
                }
                return response.json();
            })
            .then(data => {

                console.log(data)

            })
            .catch(error => {
                console.error('There has been a problem with your fetch operation:', error);
            });
    }

    let finalpostJson = () => {
        let token = window.sessionStorage.getItem("Session");
        fetch("/api/createfootprint",
            {
                method: 'GET',
                headers: {
                    Authorization: "Bearer " + token,
                    Cache: "no-cache",
                    ContentType: 'application/json'
                }
            }
        )
            .then(response => {
                if (!response.ok) {
                    throw new Error('Network response was not ok');
                }
                return response.json();
            })
            .then(data => {

                console.log(data)

            })
            .catch(error => {
                console.error('There has been a problem with your fetch operation:', error);
            });
    }
    const queryStatus = async () => {
        let token = window.sessionStorage.getItem("Session");

        fetch("/api/checkfootprintstatus",
            {
                method: 'GET',
                headers: {
                    Authorization: "Bearer " + token,
                    Cache: "no-cache",
                    ContentType: 'application/json'
                },
            }
        )
            .then(response => {
                if (!response.ok) {
                    throw new Error('Network response was not ok');
                }
                return response.json();
            })
            .then(data => {

                console.log(data)
                setStatus(data)
                // check if the last line in status contains "Error" or "Complete"

                if (data[data.length - 1].startsWith("Error")) {
                    clearTimeout(timer)
                    setSuccess("error")
                    // want to enable the end flow
                    return
                }
                if (data[data.length - 1].startsWith("Success")) {
                    clearTimeout(timer)
                    setSuccess("success")
                    // want to enable the end flow
                    return
                }
                checkStatus()
            })
            .catch(error => {
                console.error('There has been a problem with your fetch operation:', error);
                checkStatus()
            });
    }
    const checkStatus = async () => {
        timer = setTimeout(() => {

            queryStatus()

        }, 2000)
    }

    useEffect(() => {
        if (currentStep === steps.length) {
            finalpostJson()
            checkStatus()
        }
        if (currentStep === 0) {
            setSuccess("")
            setStatus([])
        }
    }, [currentStep]);

    const progress = ((currentStep) / (steps.length - 1)) * 100;
    const CurrentStepComponent = steps[currentStep];

    const handleNext = (e: React.FormEvent) => {
        e.preventDefault();
        if (formRef.current?.checkValidity()) {
            postJson()
            setCurrentStep((prev) => prev + 1);
            setValidated(false);
        } else {
            formRef.current?.reportValidity();
            setValidated(true);
        }

        e.stopPropagation();
    };
    const handleBack = () => {
        setCurrentStep((prev) => Math.max(prev - 1, 0));
    };
    const handleFinish = () => {
        finalpostJson()
    }
    let nextCallback = async (i: () => Promise<boolean>) => {
        next = i
    }
    let onClickNext = async (e) => {
        if (next == null) {
            formRef.current!.dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));
            next = null
            return
        }

        if (! await next()) {
            e.preventDefault();
            e.stopPropagation();
            return
        }
        next = null
        formRef.current!.dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));
    }
    let handleError = () => {
        let token = window.sessionStorage.getItem("Session");

        fetch("/api/resetinvitedtenant",
            {
                method: 'GET',
                headers: {
                    Authorization: "Bearer " + token,
                    Cache: "no-cache",
                    ContentType: 'application/json'
                },
            }
        )
            .then(response => {
                if (!response.ok) {
                    throw new Error('Network response was not ok');
                }
                return response.json();
            })
            .then(data => {
                setCurrentStep(0)
                setStatus([])

            })
            .catch(error => {
                console.error('There has been a problem with your fetch operation:', error);
                checkStatus()
            });
    }
    let handleSuccess = () => {
        window.location.href = "/api/logout"
    }

    return (
        <div style={{ width: '100%' }} className="mx-3 mt-2 ">
            {invitationstatus === "completed" &&
                <div className="mt-5 ml-4">
                    <h5>Your account is already configured!</h5>
                </div>
            }
            {invitationstatus === "open" &&
                <>
                    {currentStep <= steps.length - 1 ? <>
                        <div className="w-75" style={{ margin: 'auto' }}>
                            <StepProgressBar currentStep={currentStep} onStep={(i: number) => setCurrentStep(i)} steps={stepnames} />
                        </div>

                        <Form ref={formRef} noValidate validated={validated} onSubmit={handleNext}>
                            <div className="text-center">
                                <CurrentStepComponent onNext={nextCallback} state={state} setState={setState} />
                            </div>
                            <Row className="mt-5">
                                <Col className="text-right" >

                                    <Button size="sm" variant="dymium" disabled={currentStep == 0} onClick={handleBack}>Previous</Button>

                                </Col>
                                <Col className="text-left" >
                                    {currentStep < steps.length - 1 && (
                                        <Button size="sm" variant="dymium" onClick={onClickNext}>Next</Button>
                                    )}
                                    {currentStep === steps.length - 1 && (
                                        <Button size="sm" variant="dymium" onClick={onClickNext}>Finish</Button>
                                    )}
                                </Col>
                            </Row>
                        </Form>
                    </> : <div>
                        <div className="w-75" style={{ margin: 'auto' }}>
                            <h5>Creating your footptint...</h5>
                            <div className="mb-5">Please don't navigate away from this page, and don't close the browser window</div>
                            <ul id="status">
                                {
                                    status.map((p, i) => {

                                        if (p.startsWith("Error")) {
                                            return (
                                                <li key={i} style={{ fontWeight: 'bold', color: '#cc0000' }}>
                                                    {p}
                                                </li>
                                            )
                                        }
                                        if (p.startsWith("Success")) {

                                            return (
                                                <li key={i} style={{ fontWeight: 'bold', color: '#00cc00' }}>
                                                    {p}
                                                </li>
                                            )
                                        }
                                        return (
                                            <li key={i}>
                                                {p}
                                            </li>
                                        )
                                    })
                                }
                            </ul>
                            {success === "success" && <Row>
                                <Col>
                                    Your footprint has been created. You can now login to your Dymium account as a superuser, and further configure secure access to data.
                                    <div>
                                        We recommend to first try logging in a different browser or an incognito window, to ensure that the login works as expected.
                                    </div>
                                </Col>
                            </Row>
                            }
                            {success === "error" &&
                                <Row>
                                    <Col style={{ color: '#cc0000' }}>
                                        Something went wrong. Please hit Reset and try again. Do not reload the page.
                                    </Col>
                                </Row>
                            }
                            <Row className="mt-5">
                                <Col xs="auto">
                                    {(success === "error" || success === "success") && <Button size="sm" variant="dymium" onClick={handleError}>Reset</Button>}
                                </Col>
                                <Col xs="auto">
                                    {success === "success" && <Button size="sm" variant="dymium" onClick={handleSuccess}>Login</Button>}
                                </Col>
                            </Row>

                        </div>
                    </div>
                    }
                </>
            }
        </div>

    );
};

export default Signup;



export function IdP() {
    const [alert, setAlert] = useState<JSX.Element>(<></>)
    const [spinner, setSpinner] = useState<boolean>(false)
    const [state, setState] = useState<SetterState>(
        {
            idp: "",
            issuer: "",
            clientid: "",
            secret: "",
            shortname: "",
            name: "",
            domain: "",
            logo: "",
            fore: "",
            back: "",
            admins: []
        }
    );
    let getAdmins = () => {
        let token = window.sessionStorage.getItem("Session");
        setSpinner(true)
        fetch("/api/getsuperadmins",
            {
                method: 'GET',
                headers: {
                    Authorization: "Bearer " + token,
                    Cache: "no-cache",
                    ContentType: 'application/json'
                },
            }
        )
            .then(response => {
                if (!response.ok) {
                    throw new Error('Network response was not ok');
                }
                return response.json();
            })
            .then(data => {
                //types.AuthLogin{Domain: domain, Logo_url: logo_url, Primary: primary, Page_background: page_background}

                setState(state => {
                    return { ...state, admins: data }
                })
                setSpinner(false)
                console.log(data)
            })
            .catch(error => {
                console.error('There has been a problem with your fetch operation:', error);
                setSpinner(false)
            });
    }
    let getLogin = () => {
        let token = window.sessionStorage.getItem("Session");
        setSpinner(true)
        fetch("/api/getlogindetails",
            {
                method: 'GET',
                headers: {
                    Authorization: "Bearer " + token,
                    Cache: "no-cache",
                    ContentType: 'application/json'
                },
            }
        )
            .then(response => {
                if (!response.ok) {
                    throw new Error('Network response was not ok');
                }
                return response.json();
            })
            .then(data => {
                //types.AuthLogin{Domain: domain, Logo_url: logo_url, Primary: primary, Page_background: page_background}

                setState(state => {
                    return { ...state, domain: data.domain, logo: data.logo_url, fore: data.primary, back: data.page_background }
                })
                setSpinner(false)
                getAdmins()
                console.log(data)
            })
            .catch(error => {
                console.error('There has been a problem with your fetch operation:', error);
                setSpinner(false)
            });
    }
    useEffect(() => {
        let token = window.sessionStorage.getItem("Session");
        setSpinner(true)
        fetch("/api/getoidcconnection",
            {
                method: 'GET',
                headers: {
                    Authorization: "Bearer " + token,
                    Cache: "no-cache",
                    ContentType: 'application/json'
                },
            }
        )
            .then(response => {
                if (!response.ok) {
                    throw new Error('Network response was not ok');
                }
                return response.json();
            })
            .then(data => {
                setState(state => { return { ...state, ...data } })
                setSpinner(false)
                getLogin()
                console.log(data)

            })
            .catch(error => {
                setSpinner(false)
                console.error('There has been a problem with your fetch operation:', error);

            });
    }, []);

    const SetSecrets: FunctionComponent<StepProps> = ({ state, setState }) => {
        const [validated, setValidated] = useState(false);
        const formRef = useRef<HTMLFormElement>(null);

        let setIdpConnection = async () => {
            let jsb = {
                issuer: state.issuer,
                clientid: state.clientid,
                secret: state.secret
            }
            let token = window.sessionStorage.getItem("Session");
            setSpinner(true)
            let response = await fetch("/api/setoidcconnection",
                {
                    method: 'POST',
                    headers: {
                        Authorization: "Bearer " + token,
                        Cache: "no-cache",
                        ContentType: 'application/json'
                    },
                    body: JSON.stringify(jsb)
                }
            )
            if (response.status !== 200) {
                let t = await response.text()
                setAlert(
                    <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        {t}
                    </Alert>
                )
                setSpinner(false)
                return false
            }
            if (!response.ok) {
                setSpinner(false)
                return false
            }
            let js = await response.json()
            if (js.errorCode !== undefined) {
                setSpinner(false)
                return false
            }
            setAlert(
                <Alert variant="success" onClose={() => setAlert(<></>)} dismissible>
                    Connection to Identity Provider has been updated
                </Alert>
            )
            setSpinner(false)
            return true
        }

        let handleSubmit = e => {
            e.preventDefault();
            if (formRef.current?.checkValidity()) {
                setIdpConnection()
                setValidated(false);
            } else {
                formRef.current?.reportValidity();
                setValidated(true);
            }

            e.stopPropagation();
        }
        return (
            <div className="text-left pl-0 ml-0">
                <h5>Connection to Identity Provider<Spinner show={spinner} style={{ width: '28px', marginLeft: '4px' }}></Spinner></h5>
                {alert}
                <Form className="p-0" ref={formRef} noValidate validated={validated} onSubmit={handleSubmit}>
                    <StepThree onNext={onnextdefault}
                        state={state} setState={setState} left={true} />
                    <Button data-testid="apply-structure" variant="dymium" size="sm" className="mt-4 ml-3" type="submit">
                        Apply
                    </Button>
                </Form>
            </div >
        )
    }
    const Login: FunctionComponent<StepProps> = ({ state, setState }) => {
        const [validated, setValidated] = useState(false);
        const formRef = useRef<HTMLFormElement>(null);

        let setLoginDetails = async () => {
            let jsb = {
                domain: state.domain,
                logo_url: state.logo,
                primary: state.fore,
                page_background: state.back
            }
            let token = window.sessionStorage.getItem("Session");
            setSpinner(true)
            let response = await fetch("/api/setlogindetails",
                {
                    method: 'POST',
                    headers: {
                        Authorization: "Bearer " + token,
                        Cache: "no-cache",
                        ContentType: 'application/json'
                    },
                    body: JSON.stringify(jsb)
                }
            )
            if (response.status !== 200) {
                let t = await response.text()
                setAlert(
                    <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        {t}
                    </Alert>
                )
                setSpinner(false)
                return false
            }
            if (!response.ok) {
                setSpinner(false)
                return false
            }
            let js = await response.json()
            if (js.errorCode !== undefined) {
                setSpinner(false)
                return false
            }
            setAlert(
                <Alert variant="success" onClose={() => setAlert(<></>)} dismissible>
                    Login page has been updated
                </Alert>
            )
            setSpinner(false)
            return true
        }
        let handleSubmit = e => {
            e.preventDefault();
            if (formRef.current?.checkValidity()) {
                setLoginDetails()
                setValidated(false);
            } else {
                formRef.current?.reportValidity();
                setValidated(true);
            }

            e.stopPropagation();
        }
        return (
            <div className="text-left">
                <h5>Login Customization<Spinner show={spinner} style={{ width: '28px', marginLeft: '4px' }}></Spinner></h5>
                {alert}

                <Form ref={formRef} noValidate validated={validated} onSubmit={handleSubmit}>
                    <StepFour state={state} setState={setState} onNext={onnextdefault} left={true} />
                    <Button data-testid="apply-structure" variant="dymium" size="sm" className="mt-4 ml-3" type="submit">
                        Apply
                    </Button>
                </Form>
            </div >
        )
    }
    const SuperAdmins: FunctionComponent<StepProps> = ({ state, setState }) => {
        const [validated, setValidated] = useState(false);
        const formRef = useRef<HTMLFormElement>(null);

        let addSuperadmin = async () => {
            let jsb = state.admins
            let token = window.sessionStorage.getItem("Session");
            setSpinner(true)
            let response = await fetch("/api/setsuperadmins",
                {
                    method: 'POST',
                    headers: {
                        Authorization: "Bearer " + token,
                        Cache: "no-cache",
                        ContentType: 'application/json'
                    },
                    body: JSON.stringify(jsb)
                }
            )
            if (response.status !== 200) {
                let t = await response.text()
                setAlert(
                    <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                        {t}
                    </Alert>
                )
                setSpinner(false)
                return false
            }
            if (!response.ok) {
                setSpinner(false)
                return false
            }
            let js = await response.json()
            if (js.errorCode !== undefined) {
                setSpinner(false)
                return false
            }
            setAlert(
                <Alert variant="success" onClose={() => setAlert(<></>)} dismissible>
                    Connection to Identity Provider has been updated
                </Alert>
            )
            setSpinner(false)
            return true
        }
        let handleSubmit = e => {
            e.preventDefault();
            if (formRef.current?.checkValidity()) {
                addSuperadmin()
                setValidated(false);
            } else {
                formRef.current?.reportValidity();
                setValidated(true);
            }

            e.stopPropagation();
        }        
        return (
            <div className="text-left">
                <h5>Superadmins <Spinner show={spinner} style={{ width: '28px', marginLeft: '4px' }}></Spinner></h5>
                {alert}

                <Form ref={formRef} noValidate validated={validated} onSubmit={handleSubmit}>
                    <AdminEmails state={state} setState={setState} left={true} />
                    {state.admins.length > 0 &&
                    <Button data-testid="apply-structure" variant="dymium" size="sm" className="mt-4 ml-3" type="submit">
                        Apply
                    </Button>

    }
                </Form>
            </div>
        )
    }
    const t = useAppSelector((state) => {
        return state.reducer.activeSuperTab
    }
    )
    const appDispatch = useAppDispatch()

    return (
        <Tabs defaultActiveKey={t} id="superadmins"
        onSelect={(k) => {
            setAlert(<></> )
            appDispatch(setActiveSuperTab(k))
        }}
            unmountOnExit={true} className="mb-3 text-left">
            <Tab eventKey="secrets" title="Connection" className="mx-4">
                <SetSecrets state={state} setState={setState} />
            </Tab>
            <Tab eventKey="login" title="Login" className="mx-4">
                <Login state={state} setState={setState} />
            </Tab>
            <Tab eventKey="superadins" title="Super Admins" className="mx-4">
                <SuperAdmins state={state} setState={setState} />
            </Tab>

        </Tabs>
    );
}
