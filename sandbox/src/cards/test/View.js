import React, { Component } from 'react'

import styles from './View.css'

export default class TestView extends Component {

  render() {
    return (
      <div className={styles.test}>
        {`my fun test component!`}
      </div>
    )
  }

}
