import React, { useState } from 'react';
import 'bootstrap/dist/css/bootstrap.min.css';
import '../App.css';
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
        item: <> <i className="fas fa-tachometer-alt mr-1 fa-fw"></i>Dashboard </>,
        to: '/app/dashboard',
        id: 'dashboard'
      },
      {
        item: <>  <i className="fas fa-database mr-1 fa-fw"></i>Connections</>,
        to: '/app/connections',
        id: 'connections'
      },
      {
        item: <>  <i className="fas fa-users mr-1 fa-fw"></i>Groups </>,
        to: '/app/groups',
        id: 'groups'
      },
    )

    return items.map(ob => {
      let cl = 'hover-sidebar'
    
      if(ob.id === selected) {
        cl = 'hover-sidebar navselected'
      }
      console.log(ob.id + ", " + selected + ", " + cl) 
      return <Link onClick={(k) => appDispatch( setActiveMenu(ob.id) )} key={ob.id} id={ob.id} className={cl} to={ob.to} > {ob.item} </Link>
    }
    )

  }
  return (
    <div className="sidenav h-100" id="sidebar">
      {getMenuItems()}

      <a className='hover-sidebar'
        href="/api/logout"
      >
        <i className="fas fa-sign-out mr-1 fa-fw" aria-hidden="true"></i>Logout</a >

    </div>

  )
}

