module Rad.Graph.Vertex
  ( Vertex
  ) where

newtype Id       = Id Int
newtype Vertex a = Vertex Id
