import React, { Component } from 'react'
import EQ from 'css-element-queries/src/ElementQueries'
import { Link } from 'react-router'

import { getCard } from './cards'

import styles from './Sandbox.css'
import classNames from 'classnames/bind'

const cx = classNames.bind(styles)

const MODES =
  { EDIT: 0
  , VIEW: 1
  }


export default class Sandbox extends Component {

  state =
    { mode: MODES.EDIT
    , payload: null
    , width: 50
    , height: 50
    }

  setMode = m => () => this.setState({ mode: m })
  setW = w => this.setState({ width:  w })
  setH = h => this.setState({ height: h })

  save = payload => this.setState({ mode: MODES.VIEW, payload })

  // debounce timer
  timer = null

  render() {

    const Card = getCard(this.props.params.cardSlug)
    const { mode, payload, width, height } = this.state

    // hack to handle hotloading
    // do not use this in production!
    clearTimeout(this.timer)
    this.timer = setTimeout(() => EQ.init(), 100)

    return (
      <div>
        <div className={cx('header')}>

          <NavButton mode={mode} setMode={this.setMode} value={MODES.EDIT} icon="pencil" />
          <NavButton mode={mode} setMode={this.setMode} value={MODES.VIEW} icon="eye" />

          <Link to="/" className={cx('button', 'logo')}>χ</Link>

          <Slider label="width"  value={width}  onChange={this.setW}/>
          <Slider label="height" value={height} onChange={this.setH} />

        </div>
        <div className={cx('wrapper')}>
          <div className={cx('sandbox')} style={{ width: `${width}%`, height: `${height}%` }}>
            { (mode === MODES.EDIT)
            ? <Card.Edit payload={payload} onSave={this.save} />
            : <Card.View payload={payload} />
            }
          </div>
        </div>

      </div>
    )
  }

}

// navigation fragments
function NavButton({ mode, setMode, value, icon }) {
  return (
    <button className={cx('button', { active: mode === value })} onClick={setMode(value)}>
      <i className={`fa fa-${icon}`} />
    </button>
  )
}


// dimension slider

function Slider({ label, value, onChange }) {
  return (
    <div className={cx('slider')}>
      <div className={cx('sliderLabel')}>
        <strong>{label}</strong>
        <div className={cx('sliderValue')}>{`${value}%`}</div>
      </div>
      <input type="range" min="20" max="100" step="5" onChange={change} className={cx('sliderRange')}/>
    </div>
  )
  function change(e) {
    onChange(e.currentTarget.value)
  }
}
