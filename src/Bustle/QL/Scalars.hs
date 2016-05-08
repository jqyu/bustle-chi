module Bustle.QL.Scalars where

import qualified Data.ByteString as B

import Bustle.Env

newtype Id = Id B.ByteString

instance GraphQLScalar Id where
  serialize (Id x) = serialize x
  deserialize = fmap Id . deserialize

instance GraphQLValue Haxl Id
instance GraphQLType SCALAR Haxl Id where
  def = defineScalar "ID"
    $.. "an id"
    |.. "which is really just a wrapper around a ByteString"
