module Rad.QL.Define.Scalar
  ( defineScalar
  ) where

import Rad.QL.Internal.Types

import Rad.QL.AST
import Rad.QL.Types

defineScalar :: (GraphQLScalar a, Monad m) => Name -> Description -> GraphQLTypeDef SCALAR m a
defineScalar n desc = emptyDef
    { gqlTypeDef = TypeDefScalar td
    , gqlResolve = resolveScalar
    }
  where td = ScalarTypeDef n desc
