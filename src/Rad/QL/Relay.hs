{-# LANGUAGE ExistentialQuantification,
             MultiParamTypeClasses,
             OverloadedStrings,
             ScopedTypeVariables,
             StandaloneDeriving #-}

-- Meant to be imported separately:
-- import Rad.QL
-- import Rad.QL.Relay

-- TODO: constrain output
module Rad.QL.Relay where

import           Data.ByteString (ByteString)
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8 as BC8
import           Data.Typeable

import Rad.QL

-- ID type representing a unique primary key for a node
-- This is used by Rad.Graph

newtype Id = Id ByteString deriving (Eq, Show, Typeable)

instance GraphQLNamed Id where
  docs _ = "ID"
         |.. "Globally unique ID identifying a node in a graph"
instance GraphQLScalar Id where
  toValue   (Id i) = buildString i
  fromValue (ValueInt    int   ) = Just $ Id $ BC8.pack $ show int
  fromValue (ValueString string) = Just $ Id string
  fromValue _ = Nothing
instance GraphQLValue Id where
  typeDef = defineScalar
  resolve = resolveScalar


-- A Relay Node is any object that can be uniquely identified
-- by its ID field
class (GraphQLObject a, GraphQLValue a) => RelayNode a where
  nodeId :: a -> Id
  fromId :: Id -> Maybe a

-- Node describes the materialized instance
data Node = forall a. (RelayNode a) => Node a deriving (Typeable)

instance Eq Node where
  (Node a) == (Node b) = nodeId a == nodeId b
deriving instance Show Node

instance GraphQLNamed Node where
  docs _ = "Node"
         |.. "An object containing a globally unique ID"
instance GraphQLInterface Node where
  interfaceFields _ = undefined -- TODO:
instance GraphQLValue Node where
  typeDef = defineInterface
  resolve d (Node a) = resolve d a


-- TODO: free Edge and Connection instances
-- this should probably be provided by Rad.Graph
--
-- Notice that these functions are pure.
-- Creating a Connection object should prefetch all data required to render
-- a minimal connection

class (GraphQLObject (e a), GraphQLValue (e a), GraphQLValue a) => RelayEdge e a where
  node   :: e a -> Maybe a
  cursor :: e a -> Id

data Edge = forall e a. (RelayEdge e a) => Edge (e a) deriving (Typeable)
instance Eq Edge where
  (Edge a) == (Edge b) = maybe False (== b) (cast a)
deriving instance Show Edge

instance GraphQLNamed Edge where
  docs _ = "Edge"
         |.. "An object describing a relationship between two objects"
instance GraphQLInterface Edge
instance GraphQLValue Edge where
  typeDef = defineInterface
  resolve d (Edge a) = resolve d a

-- A Relay Connection describes an adjacency to other nodes
class (GraphQLObject (c e a), GraphQLValue (c e a), RelayEdge e a) => RelayConnection c e a where
  edges    :: c e a -> [e a]
  pageInfo :: c e a -> PageInfo

-- A connection is a materialized instance of a general connection
data Connection = forall c e a. (RelayConnection c e a) => Connection (c e a) deriving (Typeable)

instance Eq Connection where
  (Connection a) == (Connection b) = maybe False (== b) (cast a)
deriving instance Show Connection

instance GraphQLNamed Connection where
  docs _ = "Connection"
         |.. "An object contain edges and page info"
instance GraphQLInterface Connection
instance GraphQLValue Connection where
  typeDef = defineInterface
  resolve d (Connection a) = resolve d a

-- pageinfo object
data PageInfo = PageInfo
  { hasNextPage     :: Bool
  , hasPreviousPage :: Bool
  } deriving (Eq, Show, Typeable)

instance GraphQLNamed PageInfo where
  docs _ = "PageInfo"
         |.. "Information about pagination"
instance GraphQLObject PageInfo
instance GraphQLValue PageInfo
  -- we could theoretically write a custom resolveable instance here that's more performant

pageInfoDef :: (Monad m) => ObjectDef m PageInfo
pageInfoDef = defineObject
  [ "hasNextPage"
    |.. "returns true if you can paginate forward"
    $~> hasNextPage
  , "hasPreviousPage"
    |.. "returns true if you can paginate backwards"
    $~> hasPreviousPage
  ]

-- TODO: mutations
class RelayMutation a

-- helpers

parseInt :: ByteString -> Maybe Int
parseInt = fmap fst . BC8.readInt
