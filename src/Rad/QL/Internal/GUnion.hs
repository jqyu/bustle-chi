{-# LANGUAGE ConstraintKinds
           , ScopedTypeVariables
           , TypeOperators #-}

module Rad.QL.Internal.GUnion where

import Data.Monoid ((<>))
import Data.String (fromString)
import GHC.Generics

import Rad.QL.Internal.Types

import Rad.QL.AST
import Rad.QL.Types

type IsUnion m a = (Generic a, GUnion m (Rep a), Monad m)

unionName :: forall m a. (IsUnion m a) => m a -> Name
unionName _ = gUnionName (undefined :: m ())
            $ from       (undefined :: a   )

unionTypes :: forall m a. (IsUnion m a) => m a -> [ObjectTypeDef]
unionTypes _ = gUnionTypes (undefined :: m ())
             $ from        (undefined :: a)

unionResolve :: forall m a. (IsUnion m a) => Resolver m a
unionResolve = castResolve from gUnionResolve

class (Monad m) => GUnion m f where
  gUnionName    :: m () -> f a -> Name
  gUnionName _ _ = "someFreshName"
  gUnionTypes   :: m () -> f a -> [ObjectTypeDef]
  gUnionResolve :: Resolver m (f a)

-- handles DataType
instance (GUnion m a, Datatype c) => GUnion m (M1 D c a) where
  gUnionName  _ _ = fromString $ datatypeName (undefined :: M1 i c a x)
  gUnionTypes _ _ = gUnionTypes (undefined :: m ()) (undefined :: a x)
  gUnionResolve   = castResolve unM1 gUnionResolve

-- handles Constructor
instance (GUnion m a, Constructor c) => GUnion m (M1 C c a) where
  gUnionName  _ _ = "UNDEFINED"
  gUnionTypes _ _ = gUnionTypes (undefined :: m ()) (undefined :: a x)
  gUnionResolve   = castResolve unM1 gUnionResolve

-- handles Selector
instance (GUnion m a, Selector c) => GUnion m (M1 S c a) where
  gUnionName  _ _ = "UNDEFINED"
  gUnionTypes _ _ = gUnionTypes (undefined :: m ()) (undefined :: a x)
  gUnionResolve   = castResolve unM1 gUnionResolve

-- handles ADT unions
instance (GUnion m a, GUnion m b) => GUnion m (a :+: b) where
  gUnionName  _ _ = "UNDEFINED"
  gUnionTypes _ _ = gUnionTypes (undefined :: m ()) (undefined :: a x)
                 <> gUnionTypes (undefined :: m ()) (undefined :: b y)
  gUnionResolve args (L1 x) = gUnionResolve args x
  gUnionResolve args (R1 x) = gUnionResolve args x

-- handles K1
instance (GraphQLType OBJECT m a) => GUnion m (K1 i a) where
  gUnionName  _ _ = "UNDEFINED"
  gUnionTypes _ _ =
    case (gqlTypeDef (def :: GraphQLTypeDef OBJECT m a)) of
         TypeDefObject x -> [x]
         _               -> [] -- this should never happen
  gUnionResolve = castResolve unK1
                $ gqlResolve (def :: GraphQLTypeDef OBJECT m a)
