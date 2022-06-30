import React, { useState } from 'react';

import * as com from '../Common'
import { useAppDispatch, useAppSelector } from './hooks'
import { setActiveMenu } from '../Slices/menuSlice'
import { Navigate } from 'react-router-dom';

import './Sidebar.scss';





export default function Sticky() {
  const selected = useAppSelector((state) => {

    return state.reducer.activeMenu
  })
  const appDispatch = useAppDispatch()


  return (
    <Navigate to={"/app/"+selected} />

  )
}

