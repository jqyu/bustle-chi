import React from 'react'
import { Router, IndexRoute, Route, browserHistory } from 'react-router'

import Directory from './Directory'
import Sandbox from './Sandbox'

const Routes = (
  <Router history={browserHistory}>
    <Route path="/">
      <IndexRoute component={Directory} />
      <Route path=":cardSlug" component={Sandbox} />
    </Route>
  </Router>
)

export default Routes
