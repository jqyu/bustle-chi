module Bustle.Card.Core.Text where

import qualified Data.Text as T

import Bustle.Env

data TextCard = TextCard
  { text :: T.Text
  }

instance GraphQLValue Haxl TextCard
instance GraphQLType OBJECT Haxl TextCard where

  def = defineObject "TextCard" $ do

    describe "A piece of text, meant to test persistence"

    field "text" $ resolve $-> text
