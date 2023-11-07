import React from 'react';
import 'bootstrap/dist/css/bootstrap.min.css';
import '@dymium/common/App.scss';
import { Link } from 'react-router-dom'
import { useAppDispatch, useAppSelector } from './hooks'
import { setActiveMenu } from '../Slices/menuSlice'

import '@dymium/common/Sidebar.scss';



export default function Sidebar() {
  let selected = useAppSelector((state) => {

    return state.reducer.activeMenu
  })
  const appDispatch = useAppDispatch()

  let getMenuItems = () => {


    let items: any[] = []
    items.push(
      {
        item: <div className='darkblue'> <i className="fas fa-tachometer-alt mr-2 fa-fw "></i>Dashboard </div>,
        to: '/app/dashboard',
        id: 'dashboard'
      }
    )
    items.push(
      {
        item: <div className='darkblue'> <i className="fas fa-users mr-2 fa-fw "></i>Customers </div>,
        to: '/app/customers',
        id: 'customers'
      }
    )
    
    items.push(
      {
        item: <div className='darkblue'> <i className="fas fa-question-circle mr-2 fa-fw "></i>HowTos </div>,
        to: '/app/help',
        id: 'help'
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

