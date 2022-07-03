/**
 * @jest-environment jsdom
 */
import React from 'react';
import ReactDOM from 'react-dom';
import renderer from 'react-test-renderer';
import {render, fireEvent, waitFor, screen } from '@testing-library/react'

import { BrowserRouter } from "react-router-dom";
import { act } from 'react-dom/test-utils';
import { Provider } from 'react-redux'
import store from '../Store/index'

import { EditConnections } from '../App/Connections'
import 'bootstrap/dist/css/bootstrap.min.css';
import '@dymium/common/App.scss';
import "@fortawesome/fontawesome-free/css/all.min.css";
import mockFetch from "../mocks/mockFetch";

beforeEach(() => {
// mock fetch
    global.fetch = jest.fn().mockImplementation(() => mockFetch)();
// create a fake JWT token
    global.sessionStorage.setItem("Session", "mockJWT");
})

afterEach(() => {
    jest.restoreAllMocks()
});

test('delete-connections-form', async () => {
    let component
    await act(async () => {
        component = render (
            <Provider store={store}>
                <BrowserRouter>
                    <EditConnections />
                </BrowserRouter>
            </Provider >
        )
    });

    // click on the edit button
    let find = component.getByRole("button", {name: "delete0"} )
    await act(async () => {    
        fireEvent.click(find)
    })
    // wait until Apply shows up
    await waitFor(() => {
        expect(screen.getByTestId('Delete')).toBeInTheDocument()
    }) 

    // take snapshot of the modal component
    let modal = component.getByTestId("modal-delete")
    expect(modal).toMatchSnapshot()
    
    // click on apply and test the update api call
    let apply = screen.getByTestId('Delete')
    await act(async () => {    
        fireEvent.click(apply)
    })
});
