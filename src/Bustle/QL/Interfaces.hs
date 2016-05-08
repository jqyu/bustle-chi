module Bustle.QL.Interfaces where

import Data.Monoid ((<>))
import GHC.Generics

import Bustle.Env
import Bustle.QL.Scalars

class IsNode a where
  getId :: a -> Id

data Node = forall a. (GraphQLType OBJECT Haxl a, IsNode a) => Node a

instance GraphQLValue Haxl Node
instance GraphQLType INTERFACE Haxl Node where

  def = defineInterface "Node" $ do

    describe
      $.. "This is a node, it has an id"

    field "id" $ do
      describe "globally unique ID which identifies a node"
      resolve $~> \(Node a) -> getId a

    resolveInterface $ \args (Node a) ->
      resolveObject args a

nodes :: [ Node ]
nodes = [ Node x | x <- intNodes  ]
     <> [ Node x | x <- boolNodes ]

-- IntNode implementation

data IntNode = IntNode Id Int

intNodes :: [ IntNode ]
intNodes = [ IntNode (Id "node:1") 1
           , IntNode (Id "node:2") 4
           , IntNode (Id "node:4") 9
           ]

instance IsNode IntNode where
  getId (IntNode i _) = i

instance GraphQLValue Haxl IntNode
instance GraphQLType OBJECT Haxl IntNode where

  def = defineObject "IntNode" $ do

    describe
      $.. "A node with an int value"

    implements Node

    field "intValue" $ do
      describe "the int payload of the node"
      resolve $~> \(IntNode _ v) -> v

-- BoolNode implementation

data BoolNode = BoolNode Id Bool

boolNodes :: [ BoolNode ]
boolNodes = [ BoolNode (Id "node:3") True
            , BoolNode (Id "node:5") False
            ]

instance IsNode BoolNode where
  getId (BoolNode i _) = i

instance GraphQLValue Haxl BoolNode
instance GraphQLType OBJECT Haxl BoolNode where

  def = defineObject "BoolNode" $ do

    describe
      $.. "A node with a boolean value"

    implements Node

    field "boolValue" $ do
      describe "the bool payload of the node"
      resolve $~> \(BoolNode _ v) -> v

-- union type

unionNodes :: [ NodeUnion ]
unionNodes = [ NodeInt  x | x <- intNodes  ]
          <> [ NodeBool x | x <- boolNodes ]

data NodeUnion = NodeInt  IntNode
               | NodeBool BoolNode
               deriving (Generic)

instance GraphQLValue Haxl NodeUnion
instance GraphQLType UNION Haxl NodeUnion where

  def = defineUnion "NodeUnion"
    $.. "A union of int and bool nodes"
    |.. "unlike an interface, this exposes no fields"
