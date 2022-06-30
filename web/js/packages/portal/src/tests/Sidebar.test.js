/**
 * @jest-environment jsdom
 */
import React from 'react';
import renderer from 'react-test-renderer';

import { BrowserRouter, Routes, Route } from "react-router-dom";

import { Provider } from 'react-redux'
import { PersistGate } from 'redux-persist/integration/react';
import { persistStore } from 'redux-persist';
import store from '../Store/index'
import Sidebar from '../App/Sidebar'
import 'bootstrap/dist/css/bootstrap.min.css';
import '@dymium/common/App.scss';
import "@fortawesome/fontawesome-free/css/all.min.css";

let persistor = persistStore(store);




test('Instantiate sidebar', () => {
    const component = renderer.create(
        <Provider store={store}>
            <PersistGate loading={null} persistor={persistor}>
                <BrowserRouter>
                    <Sidebar />
                </BrowserRouter>
            </PersistGate>
        </Provider >
    );
    let tree = component.toJSON();
   
    expect(tree).toMatchSnapshot();
});