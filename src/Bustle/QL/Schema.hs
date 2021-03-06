module Bustle.QL.Schema where

import qualified Data.ByteString as B

import Bustle.Env

import qualified Bustle.Data.Post as Post
import qualified Bustle.Data.User as User

import Bustle.QL.Interfaces
import Bustle.QL.Scalars
import Bustle.QL.API.Bustle (BustleAPI(..))

data RootQueryType = RootQueryType deriving (Eq, Show)

instance GraphQLValue Haxl RootQueryType
instance GraphQLType OBJECT Haxl RootQueryType where

  def = defineObject "RootQueryType" $ do

    describe
      $.. "Root query type or graph.bustle.com"
      |.. "Contains a field for each domain"

    introspection schema

    -- tests
    field "scalarTest" $ do
      describe "some silly test or another"
      x <- arg "id" |= ""
      return $ Id x

    field "nodes" $ do
      describe "an array of interface nodes"
      return nodes

    field "unionNodes" $ do
      describe "either or"
      return unionNodes

    field "intOrBoolNodes" $ return unionNodes'

    Post.mixin -- implements the root post fields
    User.mixin -- implements the root user fields

    -- stubs
    field "bustle" $ do
      describe ""
      return BustleAPI

    field "max" $ do
      describe ""
      return (2 :: Int)

    field "api" $ do
      deprecate "use the `max` field"
      describe "Mappings to the original api.bustle.com Grape app"
      return (3 :: Int)

    field "echo" $ do
      s <- arg "echoString" :: Arg Haxl RootQueryType B.ByteString
      return s

schema :: Schema Haxl
schema = defineSchema RootQueryType
