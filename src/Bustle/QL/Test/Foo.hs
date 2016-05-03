{-# LANGUAGE DeriveGeneric,
             OverloadedStrings #-}

module Bustle.QL.Test.Foo (fooDef, getFooQL) where

import Data.Typeable
import GHC.Generics
import Rad.QL

import Bustle.Data.Test.Types
import Bustle.Env

-- foo definition
fooDef :: ObjectDef Haxl Foo
fooDef = defineObject

  [ "one"
    |.. "First field of Foo"
    $~> fooOne

  , "two"
    |.. "Second field of Foo"
    $~> fooTwo

  , "three"
    |.. "Third field of Foo"
    $~> fooThr

  ]

-- root accessor

data GetFooArgs = GetFooArgs { index :: Int } deriving (Eq, Show, Typeable, Generic)
instance GraphQLArgs GetFooArgs

getFooQL :: GetFooArgs -> Maybe Foo
getFooQL = getFoo . index
