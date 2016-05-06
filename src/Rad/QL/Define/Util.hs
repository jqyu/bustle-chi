{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
           , OverloadedStrings #-}

module Rad.QL.Define.Util where

import Data.Monoid ((<>))

import Rad.QL.AST

-- Basic definition monad, allows us to use do notation to build schemas
-- This is really a monoidal over the kind (independent of the type)

class DefinitionBuilder m where
  unit  :: m a
  merge :: m a -> m b -> m c

fmapDef :: (DefinitionBuilder m) => (a -> b) -> m a -> m b
fmapDef _ x = merge x unit

applyDef :: (DefinitionBuilder m) => m (a -> b) -> m a -> m b
applyDef = merge

-- note that regular definitions cannot use binds properly,
-- the subsequent results are disregarded entirely
bindDef :: (DefinitionBuilder m) => m a -> (a -> m b) -> m b
bindDef m _ = merge m unit

seqDef :: (DefinitionBuilder m) => m a -> m b -> m b
seqDef = merge

-- The describeable class, allows use to use the `describe` method to append a description

class (Monad m) => Describeable m where
  describe :: Description -> m ()

infixl 9 |..
(|..) :: (Describeable m) => m () -> Description -> m ()
a |.. d = a >> describe d

-- The deprecateable class, allows use of the `deprecate` method to append a description

class (Monad m) => Deprecateable m where
  deprecate :: Description -> m ()

-- useful placeholders for constructing builders

undefinedTypeDef :: TypeDef
undefinedTypeDef = TypeDefScalar $ ScalarTypeDef "UndefinedType"
   $ "Type which denotes an undefined value, "
  <> "if you see this in your schema then one of your fields is missing an implementation"
  <> "TODO: raise a warning automatically if this appears"

undefinedTypeRef :: Type
undefinedTypeRef = TypeNamed $ NamedType "UndefinedType"
