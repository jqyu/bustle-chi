{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
           , TypeSynonymInstances
           , OverloadedStrings #-}

module Bustle.QL.Schema where

import Haxl.Core

import Bustle.Env
import Rad.QL

data RootQueryType = RootQueryType deriving (Eq, Show)

instance GraphQLValue Haxl RootQueryType
instance GraphQLType OBJECT Haxl RootQueryType where

  def = defineObject "RootQueryType" $ do

    describe "Root query type or graph.bustle.com"
      |.. "Contains a field for each domain"

    -- introspection schema

    field "bustle" $ do
      describe ""
      resolve *~> (1 :: Int)

    field "max" $ do
      describe ""
      resolve *~> (2 :: Int)

    field "api" $ do
      deprecate "use the `max` field"
      describe "Mappings to the original api.bustle.com Grape app"
      resolve *~> (3 :: Int)

-- schema :: Schema Haxl RootQueryType
-- schema = defineSchema RootQueryType $ concat

