import React, { Component } from 'react'

import Edit from '../../util/Edit'
import View from './View'

// bind stylesheets
import styles from './Edit.css'
import classNames from 'classnames/bind'

const cx = classNames.bind(styles)

const DEBOUNCE = 400

class TextEdit extends Component {

  componentWillReceiveProps(next) {
    this.refs.input.value = next.payload || ''
  }

  // debounce timer
  timer = null

  update = e => {
    let val = e.target.value
    clearTimeout(this.timer)
    this.timer = setTimeout(() => {
      this.props.onChange(val)
    }, DEBOUNCE)
  }

  render() {
    return (
      <textarea
        className={cx('edit')}
        ref="input"
        rows="5"
        placeholder="What's on your mind buddy?"
        defaultValue={this.props.payload || ''}
        onChange={this.update}
        >
      </textarea>
    )

  }

}

export default Edit(View, TextEdit, { fullHeight: true })
