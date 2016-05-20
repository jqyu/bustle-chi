import React, { Component } from 'react'

//bind styles

import styles from './View.css'
import classNames from 'classnames/bind'

const cx = classNames.bind(styles)

export default class TextView extends Component {

  render() {
    return (
      <div className={cx('view')}>{this.props.payload}</div>
    )
  }

}
