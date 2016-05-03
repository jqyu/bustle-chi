module Rad.Graph.Edge
  ( EdgeMult(..)
  , EdgeOrd(..)
  , EdgeDef
  ) where

import Data.ByteString (ByteString)
import Data.Typeable

import Rad.Graph.Vertex

data EdgeMult = ManyToMany
              | ManyToOne
              | OneToMany
              | OneToOne
              deriving (Eq, Show, Typeable)

data EdgeOrd = Chron
             | ReverseChron
             | ArrayOrd
             deriving (Eq, Show, Typeable)

class (Eq a, Show a, Typeable a) => EdgeDef a where
  edgeLabel        :: a -> ByteString
  edgeDescription  :: a -> ByteString
  edgeOrdering     :: a -> EdgeOrd
  edgeMultiplicity :: a -> EdgeMult
