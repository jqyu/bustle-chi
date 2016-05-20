import Test from './test'
import TextCard from './core/text'

// full directory of all cards we can try out
const cards =
  [ Test
  , TextCard
  ]

export default cards

export function getCard(slug) {
  for (let i = 0; i < cards.length; i++) {
    const card = cards[i]
    if (card.slug === slug)
      return card
  }
  return undefined
}
