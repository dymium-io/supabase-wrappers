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

test('add-datascope', async () => {
    const component = await act( async () => render(
        <Provider store={store}>
            <BrowserRouter>
                <AddDatascope />
            </BrowserRouter>
        </Provider >
    ));

    // click on the edit button


    let connectionname =  
    await act( async () => 
        component.findByRole("textbox", {id: "dbname"} )
    )
    await act(async () => {    
        fireEvent.change(connectionname, { target: { value: "testdb" } })
    }) 

    userEvent.selectOptions(
        // Find the select element, like a real user would.
        screen.getByRole('combobox'),
        // Find and select the Ireland option, like a real user would.
        screen.getByTestId( 'adventureworks' ),
      )

/*
    //let find =  await act( async () => component.findByRole("combobox", {id: "connection"} )
    //)

    let find =  await act( async () => component.findAllByTestId('select-option')
    )    
    userEvent.selectOptions(find[0], 'adventureworks');

    let options = getAllByTestId('select-option')

    expect(options[1].selected).toBeFalsy();
    expect(options[2].selected).toBeTruthy();

*/

    let addconnection =  
    await act( async () => 
        component.findByText("Add Connection" )
    )
    await act(async () => {    
        fireEvent.click(addconnection)
    })
   
    expect(component).toMatchSnapshot()
    
    let addtable =  
    await act( async () => 
        component.findByText("Add Table" )
    )    
  
    await act(async () => {    
        fireEvent.click(addtable)
    })

    expect(component).toMatchSnapshot()



});