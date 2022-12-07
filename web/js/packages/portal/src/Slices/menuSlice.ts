import { createSlice, PayloadAction } from '@reduxjs/toolkit'
import type { RootState } from '../Store'

// Define a type for the slice state
interface MenuState {
  activeMenu: string,
  activeConnectionTab: string,
  activeConnectorTab: string,
  selectedConnector: string,
  activeDatascopeTab: string,
  selectedDatascope: string,
  activeAccessTab: string,
  activeGroupsTab: string,
}

// Define the initial state using that type
const initialState: MenuState = {
  activeMenu: "dashboard",
  activeConnectionTab: "add",
  activeConnectorTab: "add",
  selectedConnector: "",
  activeDatascopeTab: "add",
  selectedDatascope: "",
  activeAccessTab: "datascopes",
  activeGroupsTab: "groups"  
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
      setActiveConnectorTab: (state, action) => {
        state.activeConnectorTab = action.payload
      },      
      setSelectedConnectorDefault: (state, action) => {
        state.selectedConnector = action.payload
      },              
      setActiveDatascopeTab: (state, action) => {
        state.activeDatascopeTab = action.payload
      },     
      setSelectedDatascopeDefault: (state, action) => {
        state.selectedDatascope = action.payload
      },          
      setActiveAccessTab:(state, action) => {
        state.activeAccessTab = action.payload
      },        
      setActiveGroupsTab:(state, action) => {
        state.activeGroupsTab = action.payload
      },           
  },
})


export const { setActiveMenu, setActiveConnectionTab, setActiveConnectorTab, setSelectedConnectorDefault, setActiveDatascopeTab, setSelectedDatascopeDefault, setActiveAccessTab, setActiveGroupsTab } = menuSlice.actions

// Other code such as selectors can use the imported `RootState` type
export const selectActiveMenu = (state: RootState) => state.activeMenu
export const selectActiveConnectionTab = (state: RootState) => state.activeConnectionTab
export const selectActiveConnectorTab = (state: RootState) => state.activeConnectorTab
export const selectActiveDatascopeTab = (state: RootState) => state.activeDatascopeTab
export const selectActiveAccessTab = (state: RootState) => state.activeAccessTab
export const selectActiveGroupsTab = (state: RootState) => state.activeGroupsTab
export const selectSelectedDatascope = (state: RootState) => state.selectedDatascope
export const selectSelectedConnector = (state: RootState) => state.selectedConnector
export default menuSlice.reducer