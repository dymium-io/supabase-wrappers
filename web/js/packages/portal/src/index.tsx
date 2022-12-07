import React from 'react';
import ReactDOM from 'react-dom/client';
import './index.css';
import './toggle.css';
import App from './App';
import { Provider } from 'react-redux'
import { PersistGate } from 'redux-persist/integration/react';
import { persistStore } from 'redux-persist';
import store from './Store/index'
import * as com from './Common'
import 'bootstrap/dist/css/bootstrap.min.css';
import '@dymium/common/App.scss';
import "@fortawesome/fontawesome-free/css/all.min.css";

let persistor = persistStore(store);
(function() {
  let name = com.getTokenProperty("name")
  let user = window.localStorage.getItem('User')
  if(user === undefined || user === null) {
    window.localStorage.setItem('User', name)
    return 
  }
  let session = sessionStorage.getItem('Session')
  if(session === undefined)
    return 


  if(name != user)
    persistor.purge()
  window.localStorage.setItem('User', name)
})()
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


