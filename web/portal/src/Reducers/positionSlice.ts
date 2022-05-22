import { createSlice } from '@reduxjs/toolkit'

export const positionSlice = createSlice({
  name: 'url',
  initialState: {
    url: '/app/'
  },
  reducers: {

    seturl: (state, action) => {
      state.url += action.payload
    }
  }
})

export const { seturl } = positionSlice.actions

export default positionSlice.reducer