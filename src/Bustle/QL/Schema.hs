{-# LANGUAGE DeriveGeneric,
             OverloadedStrings #-}

module Bustle.QL.Schema
  ( schema
  ) where

import GHC.Generics
import Rad.QL

import Bustle.Env
import Bustle.QL.Crap
import Bustle.QL.Test.Foo (fooDef, getFooQL)

schema :: Schema Haxl
schema = defineSchema
  -- root query resolver
  [ "myOtherTest"
    |.. "Returns my other test"
    *-> myOtherTest

  , "myTest"
    |.. "This is my fun test"
    *~> myTest

  , "bop"
    |.. "Arbitrary argument test"
    *-> bop

  , "red"
    |.. "Construct a red node"
    *-> red

  , "testt"
    |.. "my fun test"
    *~>> testing

  , "black"
    |.. "Construct a black node"
    *-> black

  , "foo"
    |.. "Retrieve foo from in-memory store by index"
    *-> getFooQL
  ]
  -- TODO: root mutation resolver
  []
  -- types
  [ typeObject redDef
  , typeObject blackDef
  , typeObject fooDef
  ]

data BopArgs = BopArgs
  { baw :: Int
  , awp :: Int
  } deriving (Generic)

instance GraphQLArgs BopArgs

bop :: BopArgs -> Int
bop a = baw a + awp a

testing :: Haxl Int
testing = return 5

-- DUMMY TYPES

