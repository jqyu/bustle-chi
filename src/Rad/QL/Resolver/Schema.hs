{-# LANGUAGE ExistentialQuantification,
             OverloadedStrings,
             ScopedTypeVariables #-}

module Rad.QL.Resolver.Schema
  ( Schema(..)
  , defineSchema
  , RootQuery
  , RootMutation
  ) where

import           Control.Arrow           ((&&&))
import           Data.Function           (fix)
import           Data.Monoid             ((<>))
import qualified Data.Set                as Set
import qualified Data.Trie               as Trie
import           Data.Typeable

import Rad.QL.AST
import Rad.QL.Resolver.Field
import Rad.QL.Resolver.Introspection
import Rad.QL.Resolver.Object
import Rad.QL.Resolver.Types

data Schema m = (Monad m) => Schema (TypeDict m) (Resolver m) (Resolver m)

defineSchema :: (Monad m)
             => [FieldResolver m RootQuery]
             -> [MutationDef m]
             -> [ObjectType m]
             -> Schema m

defineSchema fs _ ts = Schema dict (resolve dict RootQuery) rm
  where dict = fix dict'
        dict' d = TypeDict
          { typeDefs        = Trie.fromList $ extractTypes $ ts' d
          , objectResolvers = Trie.fromList                $ ors d
          }
        -- Get all types
        ts' d = typeObject (rq d)             -- Include root query type
              : typeObject schemaDef          -- include schema in types
              : typeObject (schemaTypeDef  d)
              : typeObject (schemaFieldDef d)
              : typeObject schemaInputDef
              : typeObject enumValueDef
              : ts
        -- Get all resolvers
        ors d = [(n, r d) | ((ObjectTypeDef n _ _ _), r) <- ts' d ]
        -- RESOLVE ROOT QUERY TYPE, REQUIRES DICT TO EXIST
        rq td = defineObject $ fs
          <> [ "__schema"
               |.. "TODO: documentation for this"
               *~> SchemaRef      $ typeDefs td
             , "__type"
               |.. "TODO: documentation for this"
               *-> lookupTypeName $ typeDefs td
             ]
        -- TODO: resolve root mutation type from an array of mutation objects
        rm = undefined

extractTypes :: [ObjectType m] -> [(Name, TypeDef)]
extractTypes ots = objectTypes <> collectTypes Set.empty objectTypes
  where objectTypes = map (typeDefTuple . TypeDefObject . fst) ots
        -- iterate over a priority queue of types to look up
        collectTypes _ [] = []
        collectTypes seen (t@(n, def) : ts)
          | Set.member n seen = collectTypes seen ts
          | otherwise     = t : collectTypes (Set.insert n seen) (ts <> extractTypes' def)
        -- extract auxiliary types
        extractTypes' :: TypeDef -> [(Name, TypeDef)]
        extractTypes' (TypeDefUnion (UnionTypeDef _ _ ts)) =
            map (typeDefTuple . TypeDefObject) ts
        extractTypes' (TypeDefObject (ObjectTypeDef _ _ ifs fdefs)) =
            map typeDefTuple $ map TypeDefInterface ifs <> concatMap fieldTypeDefs fdefs
        extractTypes' _ = []
 
typeDefTuple :: TypeDef -> (Name, TypeDef)
typeDefTuple = typeDefName &&& id

fieldTypeDefs :: FieldDef -> [TypeDef]
fieldTypeDefs (FieldDef _ _ args _ d) = [d] <> map argValueDef args

argValueDef :: InputValueDef -> TypeDef
argValueDef (InputValueDef _ _ _ t _) = t
       
data RootQuery = RootQuery deriving (Eq, Show, Typeable)

instance GraphQLNamed RootQuery where
  docs _ = "RootQuery"
         |.. "Root query resolver"
instance GraphQLObject RootQuery
instance GraphQLValue RootQuery

data RootMutation = RootMutation deriving (Eq, Show, Typeable)

instance GraphQLNamed RootMutation where
  docs _ = "RootMutation"
         |.. "Root mutation resolver"
instance GraphQLObject RootMutation
instance GraphQLValue RootMutation

-- TODO: mutation defs

data MutationDef m = forall a. SomeMutation (m a)
