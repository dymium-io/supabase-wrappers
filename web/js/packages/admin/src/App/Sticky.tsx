import React from 'react';
import { useAppSelector } from './hooks'
import { Navigate } from 'react-router-dom';

export default function Sticky() {
  const selected = useAppSelector((state) => {

    return state.reducer.activeMenu
  })

  return (
    <Navigate to={"/app/"+selected} />

  )
}

