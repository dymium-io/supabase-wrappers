import React, { useState } from 'react';
import 'bootstrap/dist/css/bootstrap.min.css';
import '@dymium/common/App.css';
import { Link } from 'react-router-dom'
import * as com from '../Common'
import { useAppDispatch, useAppSelector } from './hooks'
import { setActiveMenu } from '../Slices/menuSlice'


import './Sidebar.css';




com.customizeStyleSheet()
export default function Sidebar() {
  const selected = useAppSelector((state) => {

    return state.reducer.activeMenu
  })
  const appDispatch = useAppDispatch()

  let getMenuItems = () => {
    let items: any[] = []
    items.push(
      {
        item: <div className='darkblue'> <i className="fas fa-tachometer-alt mr-1 fa-fw "></i>Dashboard </div>,
        to: '/app/dashboard',
        id: 'dashboard'
      },
      {
        item: <div className='darkblue'> <i className="fas fa-database mr-1 fa-fw"></i>Connections</div>,
        to: '/app/connections',
        id: 'connections'
      },
      {
        item:  <div className='darkblue'><i className="fas fa-lock mr-1  fa-fw"></i>Data Scopes</div>,
        to: '/app/datascopes',
        id: 'datascopes'
      },      
      {
        item:  <div className='darkblue'>  <i className="fas fa-users mr-1 fa-fw"></i>Groups </div>,
        to: '/app/groups',
        id: 'groups'
      },
      {
        item: <div className='darkblue'> <i className="fa fa-gavel mr-1 fa-fw"></i>Rules </div>,
        to: '/app/rules',
        id: 'rules'
      },      
      {
        item:  <div className='darkblue'>  <i className="fa-solid fa-vial mr-1 fa-fw"></i>Test SQL </div>,
        to: '/app/test',
        id: 'test'
      },      
  
    )

    return items.map(ob => {
      let cl = 'hover-sidebar '
    
      if(ob.id === selected) {
        cl = 'hover-sidebar navselected '
      }
      console.log(ob.id + ", " + selected + ", " + cl) 
      return <Link onClick={(k) => appDispatch( setActiveMenu(ob.id) )} key={ob.id} id={ob.id} className={cl} to={ob.to} > {ob.item} </Link>
    }
    )

  }
  return (
    <div className="sidenav h-100" id="sidebar">
      {getMenuItems()}

      <a  className='darkblue hover-sidebar '
        href="/api/logout"
      >
        <i className="fas fa-sign-out mr-1 fa-fw " aria-hidden="true"></i><span className="">Logout</span></a >
        </div>


  )
}

