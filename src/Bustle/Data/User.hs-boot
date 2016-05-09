module Bustle.Data.User
  -- friendly fetchers
  ( get
  -- types
  , Id(..)
  , User
  ) where

import Data.Typeable

import Bustle.Env

get :: Id -> Haxl (Maybe User)

newtype Id = Id { unwrapId :: Int }

instance Eq Id
instance Show Id
instance Typeable Id
instance GraphQLScalar Id
instance GraphQLValue Haxl Id
instance GraphQLType SCALAR Haxl Id

data User

instance Eq User
instance Show User
instance Typeable User
instance GraphQLValue Haxl User
instance GraphQLType OBJECT Haxl User

data UserReq a
