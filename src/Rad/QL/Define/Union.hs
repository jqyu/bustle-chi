{-# LANGUAGE ScopedTypeVariables #-}

module Rad.QL.Define.Union where

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
