import React, { Component } from 'react'

import { Link } from 'react-router'

import styles from './Directory.css'
import classNames from 'classnames/bind'

import cards from './cards'

const cx = classNames.bind(styles)

export default class Directory extends Component {

  render() {
    return (
      <div>
        <h1 className={cx('logo')}>χ</h1>
        <div>
          { cards.map(card =>
              <Link to={`/${card.slug}`} key={card.slug} className={cx('link')}>
                <h2>{card.name}</h2>
                <p>{card.description}</p>
              </Link>
            )
          }
        </div>
      </div>
    )

  }

}
