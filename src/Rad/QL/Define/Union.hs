{-# LANGUAGE ScopedTypeVariables #-}

module Rad.QL.Define.Union
  ( defineUnion
  , GraphQLUnion(..)
  ) where

import GHC.Generics

import Rad.QL.Internal.GUnion
import Rad.QL.Internal.Types

import Rad.QL.AST
import Rad.QL.Types

defineUnion :: forall m a. (IsUnion m a)
            => Name -> Description -> GraphQLTypeDef UNION m a
defineUnion n desc = emptyDef
    { gqlTypeDef = TypeDefUnion td
    , gqlResolve = unionResolve
    }
  where ts = unionTypes (undefined :: m a)
        td = UnionTypeDef n desc ts

-- definition-free wrapper
-- you still need to declare the GraphQLValue instance though
-- so you can decide whether or not this is worthwhile

newtype GraphQLUnion a = GraphQLUnion { unwrapUnion :: a }

instance (IsUnion m a) => GraphQLType UNION m (GraphQLUnion a) where

  def = subdef { gqlResolve = castResolve unwrapUnion subres
               , gqlFields  = []
               }
    where subdef  = defineUnion subname "" :: GraphQLTypeDef UNION m a
          subres  = gqlResolve subdef
          subname = unionName (undefined :: m a)
