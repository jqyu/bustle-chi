{-# LANGUAGE DeriveGeneric,
             OverloadedStrings #-}

module Bustle.Data.Test.Types where

import Data.ByteString (ByteString)
import Data.Typeable
import GHC.Generics

import Rad.QL

-- FOO:

data Foo = Foo
  { fooOne :: ByteString
  , fooTwo :: ByteString
  , fooThr :: Int
  } deriving (Eq, Show, Typeable)

instance GraphQLNamed Foo where
  docs _
    = "Foo"
    |.. "As in kung"
instance GraphQLObject Foo
instance GraphQLValue Foo

foos :: [Foo]
foos =
  [ Foo "Daria" "Morgendorffer" 0
  , Foo "Quinn" "Morgendorffer" 1
  , Foo "Jane"  "Lane"          2
  ]

getFoo :: Int -> Maybe Foo
getFoo = getFoo' foos
  where getFoo' []     _ = Nothing
        getFoo' (x:xs) 0 = Just x
        getFoo' (x:xs) n = getFoo' xs $ n - 1

-- BAR:

data Bar = Bar
  { bar'    :: Bar'
  , barName :: ByteString
  } deriving (Eq, Show, Typeable)

instance GraphQLNamed Bar where
  docs _
    = "Bar"
    |.. "A priest, a rabbi, and an imam walk into one"
instance GraphQLObject Bar
instance GraphQLValue Bar

data Bar' = Bar'
  { barOne :: Int
  , barTwo :: Double
  , barThr :: Bool
  } deriving (Eq, Show, Typeable, Generic)

instance GraphQLNamed Bar' where
  docs _
    = "BarPrime"
    |.. "Inferred type containing actual Bar payload"
instance GraphQLObject' Bar' where
  objectDocs' _ =
    [ "barOne"
      |.. "internal first field"
    , "barTwo"
      |.. "internal second field"
    , "barThr"
      |.. "internal third field"
    ]
instance GraphQLObject Bar'
instance GraphQLValue Bar' where
  typeDef = defineObject'
  resolve = resolveObject'

-- BAZ:

data Baz = Baz
  { bazOne :: ByteString
  , bazTwo :: ByteString
  , bazThr :: ByteString
  } deriving (Eq, Show, Typeable)

instance GraphQLNamed Baz where
  docs _ = "Baz"
instance GraphQLObject Baz
instance GraphQLValue Baz
