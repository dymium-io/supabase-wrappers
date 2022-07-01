/**
 * @jest-environment jsdom
 */
import React from 'react';
import renderer from 'react-test-renderer';

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
})

afterEach(() => {
   jest.restoreAllMocks()
});

test('add-datascope', async () => {
    const component = await act( async () => renderer.create(
        <Provider store={store}>
            <BrowserRouter>
                <AddDatascope />
            </BrowserRouter>
        </Provider >
    ));
    let tree = component.toJSON();

    expect(tree).toMatchSnapshot();
});