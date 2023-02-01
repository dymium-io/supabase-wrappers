import { createSlice } from '@reduxjs/toolkit'
import type { RootState } from '../Store'

// Define a type for the slice state
interface MenuState {
  activeMenu: string,
  activeCustomer: string,
  activeCustomersTab: string,

}

// Define the initial state using that type
const initialState: MenuState = {
  activeMenu: "dashboard",
  activeCustomer: "",
  activeCustomersTab: "add",

}

export const menuSlice = createSlice({
  name: 'selected',
  // `createSlice` will infer the state type from the `initialState` argument
  initialState,
  reducers: {
      setActiveMenu: (state, action) => {
        state.activeMenu = action.payload
      },
      setActiveCustomer: (state, action) => {
        state.activeCustomer = action.payload
      },
      setActiveCustomersTab: (state, action) => {
        state.activeCustomersTab = action.payload
      },
    
  },
})


export const { setActiveMenu, setActiveCustomer, setActiveCustomersTab } = menuSlice.actions

// Other code such as selectors can use the imported `RootState` type
export const selectActiveMenu = (state: RootState) => state.activeMenu
export const selectActiveCustomer = (state: RootState) => state.activeCustomer
export const selectActiveCustomersTab = (state: RootState) => state.activeCustomersTab

export default menuSlice.reducer