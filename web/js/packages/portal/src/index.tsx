import React from 'react';
import ReactDOM from 'react-dom/client';
import './index.css';
import App from './App';
import { Provider } from 'react-redux'
import { PersistGate } from 'redux-persist/integration/react';
import { persistStore } from 'redux-persist';
import store from './Store/index'

import 'bootstrap/dist/css/bootstrap.min.css';
import '@dymium/common/App.css';
import "@fortawesome/fontawesome-free/css/all.min.css";

let persistor = persistStore(store);
const root = ReactDOM.createRoot(document.getElementById('root')!);
root.render(

  //<React.StrictMode>
  <Provider store={store}>
    <PersistGate loading={null} persistor={persistor}>
      <App />
    </PersistGate>
  </Provider >
  //</React.StrictMode>

);


