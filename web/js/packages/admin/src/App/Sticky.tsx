import React, { useState } from 'react';

import { useAppDispatch, useAppSelector } from './hooks'
import { setActiveMenu } from '../Slices/menuSlice'
import { Navigate } from 'react-router-dom';




export default function Sticky() {
  const selected = useAppSelector((state) => {

    return state.reducer.activeMenu
  })
  const appDispatch = useAppDispatch()

  return (
    <Navigate to={"/app/"+selected} />

  )
}

