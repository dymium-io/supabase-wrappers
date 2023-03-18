import React, { Component, useEffect, useState, useRef } from 'react';

import Tabs from 'react-bootstrap/Tabs'
import Tab from 'react-bootstrap/Tab'
import { useAppDispatch, useAppSelector } from './hooks'
import { setActiveRuleTab } from '../Slices/menuSlice'
import AccessLevels from './AccessLevels'
import BuildRulesClass from './BuildRulesClass'
var _ = require('lodash')



export default function Rules() {
  const t = useAppSelector((state) => {
    return state.reducer.activeRuleTab
  }
  )

  const appDispatch = useAppDispatch()

  return (
    <Tabs defaultActiveKey={t}
      onSelect={(k) => appDispatch(setActiveRuleTab(k))}

      unmountOnExit={true} className="mb-3 text-left">

      <Tab eventKey="add" title="Access Levels" className="mx-4">
        <AccessLevels />
      </Tab>
      <Tab eventKey="edit" title="Rules" className="mx-4" >
        <BuildRulesClass />
      </Tab>
    </Tabs>

  )
}