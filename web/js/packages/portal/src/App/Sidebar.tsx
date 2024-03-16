import React, { useState } from 'react';
import 'bootstrap/dist/css/bootstrap.min.css';
import { Link } from 'react-router-dom'
import * as com from '../Common'
import { useAppDispatch, useAppSelector } from './hooks'
import { setActiveMenu } from '../Slices/menuSlice'


import './Sidebar.scss';

export default function Sidebar() {
  let selected = useAppSelector((state) => {

    return state.reducer.activeMenu
  })
  const appDispatch = useAppDispatch()
  let roles = com.getTokenProperty("roles")
  if (roles == null || roles.length == 0) {
    return <></>
  }

  let isuser = roles.includes("user")
  let issigner = roles.includes("signer")
  let issuperadmin = roles.includes("superadmin")

  let isadmin = issuperadmin || roles.includes("admin")
  if (!isadmin ) {
    if (issigner) {
      selected = "signup"
      appDispatch(setActiveMenu(selected))
    } else {
      if (["dashboard", "connections", "datascopes", "groups", "rules"].includes(selected)) {
        selected = "access"
        appDispatch(setActiveMenu(selected))
      }
    }
  }

  let getMenuItems = () => {


    let items: any[] = []
    if (issigner)
      items.push(
        {
          item: <div className='darkblue'> <i className="fas fa-id-card mr-2 fa-fw "></i>Authentication</div>,
          to: '/app/signup',
          id: 'signup'
        })

    if (isadmin)
      items.push(
        {
          item: <div className='darkblue'> <i className="fas fa-tachometer-alt mr-2 fa-fw "></i>Dashboard </div>,
          to: '/app/dashboard',
          id: 'dashboard'
        })
        if(issuperadmin)     
        items.push(
       {
         item: <div className='darkblue'> <i className="fas fa-id-card mr-2 fa-fw "></i>IdP integration</div>,
         to: '/app/idp',
         id: 'idp'
       })
    if (isadmin)
      items.push(
        {
          item: <div className='darkblue'>  <i className="fas fa-users mr-2 fa-fw"></i>Groups </div>,
          to: '/app/groups',
          id: 'groups'
        })

    if (isadmin)
      items.push(
        {
          item: <div className='darkblue'><i className="fas fa-diagram-project mr-2  fa-fw"></i>Connectors</div>,
          to: '/app/connectors',
          id: 'connectors'
        })
    if (isadmin)
      items.push(
        {
          item: <div className='darkblue'> <i className="fas fa-database mr-2 fa-fw"></i>Data Sources</div>,
          to: '/app/connections',
          id: 'connections'
        })

    if (isadmin)
      items.push(
        {
          item: <div className='darkblue'> <i className="fa fa-gavel mr-2 fa-fw"></i>Rules </div>,
          to: '/app/rules',
          id: 'rules'
        })

    if (isadmin)
      items.push(
        {
          item: <div className='darkblue'><i className="fas fa-ghost mr-2  fa-fw"></i>Ghost Databases</div>,
          to: '/app/datascopes',
          id: 'datascopes'
        })

    if (isuser)
      items.push(
        {
          item: <div className='darkblue'> <i className="fa fa-key mr-2 fa-fw"></i>User Access </div>,
          to: '/app/access',
          id: 'access'
        })

    if (isadmin)
      items.push(
        {
          item: <div className='darkblue'><i className="fas fa-network-wired mr-2  fa-fw"></i>Machine Access</div>,
          to: '/app/machineaccess',
          id: 'machineaccess'
        })
    if (!issigner)
      items.push(
        {
          item: <div className='darkblue'>  <i className="fa-solid fa-vial mr-2 fa-fw"></i>Test SQL </div>,
          to: '/app/test',
          id: 'test'
        }

      )

    return items.map(ob => {
      let cl = 'hover-sidebar '

      if (ob.id === selected) {
        cl = 'hover-sidebar navselected '
      }
      return <Link onClick={(k) => appDispatch(setActiveMenu(ob.id))} key={ob.id} id={ob.id} className={cl} to={ob.to} > {ob.item} </Link>
    }
    )

  }
  return (
    <div className="sidenav h-100" id="sidebar">
      {getMenuItems()}

      <a className='darkblue hover-sidebar '
        href="/api/logout"
      >
        <i className="fas fa-sign-out mr-2 fa-fw " aria-hidden="true"></i><span className="">Logout</span></a >
    </div>


  )
}

