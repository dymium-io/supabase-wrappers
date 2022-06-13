import { createSlice, PayloadAction } from '@reduxjs/toolkit'
import type { RootState } from '../Store'

// Define a type for the slice state
interface MenuState {
  activeMenu: string,
  activeConnectionTab: string,
  activeDatascopeTab: string,
}

// Define the initial state using that type
const initialState: MenuState = {
  activeMenu: "dashboard",
  activeConnectionTab: "add",
  activeDatascopeTab: "add",
}

export const menuSlice = createSlice({
  name: 'selected',
  // `createSlice` will infer the state type from the `initialState` argument
  initialState,
  reducers: {
    setActiveMenu: (state, action) => {
      state.activeMenu = action.payload
    },
    setActiveConnectionTab: (state, action) => {
        state.activeConnectionTab = action.payload
      },
      setActiveDatascopeTab: (state, action) => {
        state.activeDatascopeTab = action.payload
      },      
  },
})


export const { setActiveMenu, setActiveConnectionTab, setActiveDatascopeTab } = menuSlice.actions

// Other code such as selectors can use the imported `RootState` type
export const selectActiveMenu = (state: RootState) => state.activeMenu
export const selectActiveConnectionTab = (state: RootState) => state.activeConnectionTab
export const selectActiveDatascopeab = (state: RootState) => state.activeDatascopeTab
export default menuSlice.reducer