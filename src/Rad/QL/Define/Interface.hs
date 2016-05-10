{-# LANGUAGE EmptyDataDecls
           , ScopedTypeVariables #-}

module Rad.QL.Define.Interface
  ( defineInterface
  , resolveInterface
  , resolveObject
  ) where

import Data.Monoid ((<>))

import Rad.QL.Internal.Types

import Rad.QL.Define.Field
import Rad.QL.Define.Util

import Rad.QL.AST
import Rad.QL.Query
import Rad.QL.Types

defineInterface :: (Monad m) => Name -> InterfaceDefM m a InterfaceResolver -> GraphQLTypeDef INTERFACE m a
defineInterface n def = emptyDef
    { gqlTypeDef = TypeDefInterface td
    , gqlResolve = idResolver      def
    , gqlFields  = idFields        def
    }
  where td  = InterfaceTypeDef n (idDesc def) fds
        fds = [ fieldDef f | f <- idFields def ]

data InterfaceResolver = InterfaceResolver

resolveInterface :: (Monad m) => Resolver m a -> InterfaceDefM m a InterfaceResolver
resolveInterface r = unit { idResolver = r, unwrap = InterfaceResolver }

resolveObject :: forall m a. (Monad m, GraphQLType OBJECT m a) => Resolver m a
resolveObject args x = gqlResolve (def :: GraphQLTypeDef OBJECT m a) args x

data InterfaceDefM m a b = InterfaceDefM
  { idDesc     :: Description
  , idFields   :: [GraphQLFieldDef m a]
  , idResolver :: Resolver m a
  , unwrap     :: b
  }

instance Functor (InterfaceDefM m a) where
  fmap f x = pure f <*> x

instance Applicative (InterfaceDefM m a) where
  pure x = InterfaceDefM
    { idDesc     = ""
    , idFields   = []
    , idResolver = \_ _ -> errorMsg "No type resolver implemented"
    , unwrap     = x
    }
  f <*> x = x
    { idDesc   = idDesc   f <> idDesc   x
    , idFields = idFields f <> idFields x
    , unwrap   = unwrap   f $  unwrap   x
    }

instance Monad (InterfaceDefM m a) where
  m >>= k = m >> k (unwrap m)
  m >> k = m { unwrap = id } <*> k

instance Describable (InterfaceDefM m a) where
  describe d = unit { idDesc = d }

instance HasFields InterfaceDefM m a where
  fieldSingleton f = unit { idFields = [f] }
