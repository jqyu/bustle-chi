import React, { Component } from 'react'

import styles from './Edit.css'
import classNames from 'classnames/bind'

import { List } from 'immutable'

const cx = classNames.bind(styles)

export default function(View, Edit, { fullHeight = false }) {

  // the Edit wrapper maintains typical state associated with a card
  // including toggling between view/edit state, maintaining a state history for undo/redo
  // and batching/committing changes to the parent component

  return class extends Component {

    // expose references to base components
    static Edit = Edit
    static View = View

    state =
      { history: List([this.props.payload])
      , editing: false
      , saving: true
      }

    // listen to state pushes from wrapped component
    pushState = state => {
      this.setState
        ( { history: this.state.history.push(state)
          }
        )
    }

    // switch to edit mode
    edit   = () => this.setState({ editing: true })
    cancel = () => this.setState({ editing: false })

    // revert to previous state
    // this is currently a destructive action (just as a proof of concept)
    // TODO: maintain history and history position, or maintain redo stack
    undo = () => {
      this.setState
        ( { history: this.state.history.pop()
          }
        )
    }

    // send save action to parent
    save = () => {
      this.props.onSave(this.state.history.last())
    }

    render() {

      const { history, editing, saving } = this.state

      // editing state
      if (editing) return (
        <div className={cx('wrapper')}>
          <div className={cx('viewport')}>
            <Edit payload={history.last()} onChange={this.pushState} />
          </div>
          <div className={cx('toolbar')}>
            <button onClick={this.save}>
              <i className="fa fa-save" />
            </button>
            <button onClick={this.undo}>
              <i className="fa fa-undo" />
            </button>
            <button onClick={this.cancel}>
              <i className="fa fa-times" />
            </button>
          </div>
        </div>
      )

      // view state
      return (
        <div className={cx('wrapper')}>
          <div className={cx('viewport')}>
            <View payload={history.last()} />
          </div>
          <div className={cx('toolbar')}>
            <button onClick={this.edit}>
              <i className="fa fa-pencil-square-o" />
            </button>
          </div>
        </div>
      )
    }

  }

}
