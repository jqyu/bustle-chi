{-# LANGUAGE OverloadedStrings #-}

module Rad.QL
  ( runQuery 

  -- * Type definitions
  , GraphQLNamed
  , docs
  , (|..)

  -- SCALAR
  , GraphQLScalar
  , toValue
  , fromValue
  , defineScalar
  , scalarDef
  -- helper
  , buildString

  -- ENUM
  , GraphQLEnum
  , defineEnum
  , enumDef

  -- OBJECT
  , GraphQLObject
  , objectDef

  -- OBJECT RESOLVER
  , ObjectDef
  , ObjectType
  , defineObject
  , implements
  , typeObject

  -- field resolver infix functions
  , ($->), ($-?>), ($->>), ($-?>>)
  , ($~>), ($~?>), ($~>>), ($~?>>)
  , (*->), (*-?>), (*->>), (*-?>>)
  , (*~>), (*~?>), (*~>>), (*~?>>)

  -- AUTOMATIC STRICT OBJECT RESOLVER
  , GraphQLObject'
  , resolveObject'
  , defineObject'
  , objectDef'
  , objectDocs'

  -- INTERFACE
  , GraphQLInterface
  , interfaceFields
  , defineInterface
  , interfaceDef

  -- UNION
  , GraphQLUnion
  , unionTypes
  , defineUnion
  , unionDef

  -- VALUE
  , GraphQLValue
  , resolve
  , typeDef
  , resolveScalar
  , resolveObject

  -- * Args
  , GraphQLArgs
  , NoArgs
  , argsDef
  , castArgs

  -- * AST Literals
  , Value(..)

  , Schema
  , defineSchema

  , RootQuery
  , RootMutation

  ) where

import qualified Data.Aeson as JSON
import Data.ByteString.Char8 as BC8
import Data.ByteString.Builder
import Data.Monoid      ((<>))

import Rad.QL.AST
import Rad.QL.Execute (execute, subst)
import Rad.QL.Parser (parseDocument)
import Rad.QL.Resolver.Arguments
import Rad.QL.Resolver.Field
import Rad.QL.Resolver.Object
import Rad.QL.Resolver.Schema
import Rad.QL.Resolver.Types

-- TODO: validating parser
-- TODO: real error messages
-- TODO: take query variables
runQuery :: (Monad m) => Schema m -> ByteString -> JSON.Value -> m Builder
runQuery s q v =
  case parseDocument q >>= subst v of
       Left e -> return
          ( byteString "{\"data\":null,\"errors\":"
          <> buildString (BC8.pack e)
          <> byteString "}"
          )
       Right op -> collectResult <$> execute s op

collectResult :: (Builder, [ ByteString ]) -> Builder
collectResult (d, []) = byteString "{\"data\":" <> d <> byteString "}"
collectResult (d, es) = byteString "{\"data\":"
                     <> d
                     <> byteString ", \"errors\": "
                     <> joinList [ buildString e | e <- es ]
                     <> byteString " }"
