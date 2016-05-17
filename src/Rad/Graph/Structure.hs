module Rad.Graph.Structure where

import Data.Typeable

import qualified Data.ByteString as B

-- An Element has an ID

newtype Id = Id { getId :: B.ByteString } deriving (Eq, Show)

-- An Element has some property set

class Property a where
  serialize   :: a -> B.ByteString
  deserialize :: B.ByteString -> Maybe a

class PropertySet a where
  serializeSet   :: a -> B.ByteString
  deserializeSet :: B.ByteString -> Maybe a

data EmptySet = EmptySet

type Label   = B.ByteString
type Payload = B.ByteString
type Weight  = Int

-- A vertex is a tuple (id, label, properties)
-- TODO: versioning
data GenVertex a = Vertex Id Label a
data SomeVertex = SomeVertex Id Label B.ByteString
                deriving (Eq, Show, Typeable)

-- An edge is a tuple (id, from, label, to, weight, properties)
data GenEdge a = Edge Id Id Label Id Weight a
data SomeEdge = SomeEdge Id Id Label Id Weight Payload
              deriving (Eq, Show, Typeable)

class (Monad m) => GraphSource m where

  -- data fetchers
  getVertex    :: Id -> m SomeVertex
  getEdge      :: Id -> m SomeEdge
  getAdjacency :: Id -> Label -> Maybe Weight -> Int -> m [SomeEdge]

  -- creates
  addVertex    :: Label -> Payload -> m Id
  addEdge      :: Id -> Label -> Id -> Maybe Weight -> Payload -> m Id

  -- updates
  updateVertex :: Id -> Payload -> m ()
  updateEdge   :: Id -> Payload -> m ()
  moveEdge     :: Id -> Weight -> m ()

  -- deletes
  deleteVertex :: Id -> m ()
  deleteEdge   :: Id -> m ()
