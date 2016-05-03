{-# LANGUAGE DeriveGeneric,
             OverloadedStrings #-}

-- tests and such for stupid shit nobody cares about

module Bustle.QL.Crap where

import Data.ByteString (ByteString)
import Data.Typeable
import GHC.Generics
import Rad.QL

import Bustle.Env

-- GENERIC DERIVATION

data Publication = BUSTLE
                 | ROMPER
                 deriving (Eq, Show, Typeable, Generic)
instance GraphQLNamed Publication where
  docs _
    = "Publication"
    |.. "Enum representing a publication"
instance GraphQLEnum Publication
instance GraphQLScalar Publication
instance GraphQLValue Publication where
  typeDef = defineEnum
  resolve = resolveScalar

data MyOtherTest = MyOtherTest
  { testFooO :: Int
  , testBooO :: Double
  , testPubO :: Publication
  } deriving (Eq, Show, Typeable, Generic)

instance GraphQLNamed MyOtherTest where
  docs _
    = "MyOtherTest"
    |.. "Testing recursive automatic derivation"
instance GraphQLObject' MyOtherTest
instance GraphQLObject MyOtherTest
instance GraphQLValue MyOtherTest where
  typeDef = defineObject'
  resolve = resolveObject'
instance GraphQLArgs MyOtherTest

myOtherTest :: MyOtherTest -> MyOtherTest
myOtherTest = id

data MyTest = MyTest
  { testFoo :: Int
  , testBar :: Int
  , testOth :: MyOtherTest
  } deriving (Eq, Show, Typeable, Generic)

instance GraphQLNamed MyTest where
  docs _
    = "MyTest"
    |.. "Testing automatic object derivation"
instance GraphQLObject' MyTest where
  objectDocs' _ =
    [ "testFoo"
      |.. "test foo"
      |.. "# IT'S A TEST"
      |.. "foo foo foo foo"
      |.. "### kufufufufu"
    , "testOth"
      |.. "Nested object resolution"
    ]
instance GraphQLObject MyTest
instance GraphQLValue MyTest where
  typeDef = defineObject'
  resolve = resolveObject'

myTest :: MyTest
myTest = MyTest
  { testFoo = 10
  , testBar = 20
  , testOth = MyOtherTest
      { testFooO = 5
      , testBooO = 1.2
      , testPubO = BUSTLE
      }
  }

-- MUTUALLY RECURSIVE TYPES

data Red = Red
  { rfoo :: Int
  , rbar :: Double
  } deriving (Eq, Show, Typeable, Generic)

instance GraphQLArgs Red

instance GraphQLNamed Red where
  docs _
    = "RedNode"
    |.. "An arbitrary node"
instance GraphQLObject Red
instance GraphQLValue Red

red :: Red -> Red
red = id

redDef :: ObjectDef Haxl Red
redDef = defineObject
  [ "foo"
    |.. "foo"
    |.. "This should make it pretty easy to make multiple lines of text"
    |.. "So descriptions suck less"
    $~> rfoo
  , "bar"
    |.. "# This description takes a thing!!"
    |.. "# This description takes a thing!!"
    $~> rbar
  , "blacks"
    *-> redBlacks
  ]

data RedBlacksArgs = RedBlacksArgs { num :: Int } deriving (Eq, Show, Typeable, Generic)
instance GraphQLArgs RedBlacksArgs

redBlacks :: RedBlacksArgs -> [Black]
redBlacks a = [ Black { bfoo = "WOOP", bbar = r' } | r' <- [1..(num a)]  ]


data Black = Black
  { bfoo :: ByteString
  , bbar :: Int
  } deriving (Eq, Show, Typeable, Generic)

instance GraphQLArgs Black

instance GraphQLNamed Black where
  docs _
    = "BlackNode"
    |.. "An arbitrary black node"
instance GraphQLObject Black
instance GraphQLValue  Black

black :: Black -> Black
black = id

blackDef :: ObjectDef Haxl Black
blackDef = defineObject

  [ "foo"
    |.. "Some arbitrary string field"
    $~> bfoo

  , "bar"
    |.. "Some arbitrary int field"
    $~> bbar

  , "red"
    $~> blackRed

  , "test"
    |.. "A test field that multiplies bbar by two"
    $~>> \v -> do
      let b = bbar v
      return $ 2 * b
  ]

blackRed :: Black -> Red
blackRed b = Red
  { rfoo = 5 * bbar b
  , rbar = fromIntegral $ bbar b
  }
