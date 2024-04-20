import React from 'react';
import Card from 'react-bootstrap/Card';
import Button from 'react-bootstrap/Button';
import { useLocation } from "react-router-dom";
import DOMPurify from 'dompurify';
import Backdrop from "./Backdrop";

function useQuery() {
  const { search } = useLocation();
  return React.useMemo(() => new URLSearchParams(search), [search]);
}

export default function Error() {
  let query = useQuery();

  // Sanitize the query parameters
  const header = DOMPurify.sanitize(query.get("header"));
  const body = DOMPurify.sanitize(query.get("body"));

  return (
    <div className="py-0 my-0 text-center">
      <Backdrop />
      <div className="text-center" style={{
        position: 'absolute', top: '0px', left: '0px',
        width: '100%', height: '100vh'
      }}>
        <div id="loginbox" className="d-flex" style={{ position: 'relative', 
        marginLeft: 'auto', marginRight: 'auto', 
        alignItems: 'center',
        justifyContent: 'center'}}>

          <Card style={{width: '50%'}}>
            <Card.Header><h4>Error: {header}</h4></Card.Header>
            <Card.Body className="my-3">Details: {body}</Card.Body>
            <Card.Footer><Button onClick={e => {window.location.href='/'}}>Retry</Button></Card.Footer>
          </Card>

        </div>
      </div>
    </div>
  );
}
