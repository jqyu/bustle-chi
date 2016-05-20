module Rad.QL.Define.Argument where

import Rad.QL.AST

class HasArgs d m a where
  argSingleton :: GraphQLArgDef m a b -> d m a b

arg :: forall d m a b. (HasArgs d m a, GraphQLScalar)
arg = undefined
