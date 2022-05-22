import { configureStore } from '@reduxjs/toolkit'
import positionSlice from '../Reducers/positionSlice'

export default configureStore({
  reducer: {
    position: positionSlice
  },
})