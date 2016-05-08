{-# LANGUAGE ScopedTypeVariables #-}

module Rad.QL.Define.Schema where

import           Data.Monoid     ((<>))
import qualified Data.Trie       as Trie

import Rad.QL.Internal.Types

import Rad.QL.AST
import Rad.QL.Types
import Rad.QL.Query

defineSchema :: forall m b. (GraphQLType OBJECT m b) => b -> Schema m
defineSchema root = Schema
    { typeDict      = collectTypes Trie.empty [tdef]
    , rootQuery     = res
    , rootQueryType = tdef
    }
  where rdef   = def :: GraphQLTypeDef OBJECT m b
        tdef   = gqlTypeDef rdef
        res ss = unpackSub $ (gqlResolve rdef) ss root
        unpackSub (SubResult  m) = return m
        unpackSub (SubResultM m) = m

collectTypes :: TypeDict -> [TypeDef] -> TypeDict
collectTypes seen [] = seen
collectTypes seen (t:ts)
    | Trie.member n seen = collectTypes                  seen  ts
    | otherwise          = collectTypes (Trie.insert n t seen) ts'
  where n = typeDefName t
        ts' = case t of
          TypeDefObject (ObjectTypeDef _ _ ifs fdefs) ->
            ts <> [ TypeDefInterface ifdef | ifdef <- ifs ]
               <> [ t' | FieldDef _ _ _       _ t' _ <- fdefs ]
               <> [ t' | FieldDef _ _ argdefs _ _  _ <- fdefs
                       , InputValueDef _ _ _ t' _    <- argdefs
                       ]
          TypeDefInterface (InterfaceTypeDef _ _ fdefs) ->
            ts <> [ t' | FieldDef _ _ _       _ t' _ <- fdefs ]
               <> [ t' | FieldDef _ _ argdefs _ _  _ <- fdefs
                       , InputValueDef _ _ _ t' _    <- argdefs
                       ]
          TypeDefUnion (UnionTypeDef _ _ odefs) ->
            ts <> [ TypeDefObject o | o <- odefs ]
          _ -> ts
