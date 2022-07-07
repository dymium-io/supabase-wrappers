/**
 * @jest-environment jsdom
 */
import React from 'react';
import {render, fireEvent, waitFor, screen } from '@testing-library/react'
import userEvent from '@testing-library/user-event';
import { BrowserRouter } from "react-router-dom";
import { act } from 'react-dom/test-utils';
import { Provider } from 'react-redux'
import store from '../Store/index'
import { Http } from '../Api/Http'
import { AddDatascope } from '../App/Datascopes'
import 'bootstrap/dist/css/bootstrap.min.css';
import '@dymium/common/App.scss';
import "@fortawesome/fontawesome-free/css/all.min.css";
import mockFetch from "../mocks/mockFetch";


beforeEach(() => {
  global.fetch = jest.fn().mockImplementation(() => mockFetch)();
  global.sessionStorage.setItem("Session", "mockJWT");
  globalThis.IS_REACT_ACT_ENVIRONMENT = true;
})

afterEach(() => {
   jest.restoreAllMocks()
});

// test that builds a simple datascope, and sends it to the server
test('add-datascope', async () => {

    // render the page
    const component = await act( async () => render(
        <Provider store={store}>
            <BrowserRouter>
                <AddDatascope />
            </BrowserRouter>
        </Provider >
    ));

    // fill in the name testdb
    let connectionname =  
    await act( async () => 
        component.findByRole("textbox", {id: "dbname"} )
    )
    await act(async () => {    
        fireEvent.change(connectionname, { target: { value: "testdb" } })
    }) 

    // select adventureworks connection
    userEvent.selectOptions(
        screen.getByRole('combobox'),
        screen.getByTestId( 'adventureworks' ),
      )
    // click on add connection button
    let addconnection =  
    await act( async () => 
        component.findByText("Add Connection" )
    )
    await act(async () => {    
        fireEvent.click(addconnection)
    })
    // take snapshot and run expect
    expect(component).toMatchSnapshot()

    // find add table button
    let addtable =  
    await act( async () => 
        component.findByText("Add Table" )
    )    
    // click on add table
    await act(async () => {    
        fireEvent.click(addtable)
    })
    // the add table sidebar should open, snapshot and expect it
    expect(component).toMatchSnapshot()

    // find the schema name typeahead control, and fill with humanresources
    let schema =  
    await act( async () => 
        component.findByLabelText("Schema Name:" )
    )    
    await act(async () => {    
        fireEvent.change(schema, { target: { value: "humanresources" } })
    }) 
    // click on the dropdown
    let schemaselection =  
    await act( async () => 
        component.findByText( "humanresources" )
    )    
    await act(async () => {    
        fireEvent.click(schemaselection)
    })    
    // find the choose table typeahead
    let table =  
    await act( async () => 
        component.getByPlaceholderText( "Choose table..." )
    )    
    // fill it with employee
    await act(async () => {    
        fireEvent.change(table, { target: { value: "employee" } })
    }) 
    // find and click on the dropdown
    let tableselection =  
    await act( async () => 
        component.findByText( "employee" )
    )    
    await act(async () => {    
        fireEvent.click(tableselection)
    })    
    // select confidential
    let seclevel = screen.getByTestId('seclevel')
    let confidential = screen.getByTestId( 'confidential' )

    await act(async () => {    
        userEvent.selectOptions(seclevel, confidential )
    })    
    // fill in security default
    let fill = screen.getByTestId( 'fill-security' )
    await act(async () => {    
        fireEvent.click(fill)
    })     
    // take snapshot of the filled-in form
    expect(component).toMatchSnapshot()
    // click on apply, this should finish the datascope
    let applystructure = screen.getByTestId( 'apply-structure' )
    await act(async () => {    
        fireEvent.click(applystructure)
    })     
    // take the snapshot
    expect(component).toMatchSnapshot()
    // click apply, and expect the network call
    let applydatascope = screen.getByTestId( 'apply-datascope' )
    await act(async () => {    
        fireEvent.click(applydatascope)
    })     
    // take the final snapshot
    expect(component).toMatchSnapshot()
});