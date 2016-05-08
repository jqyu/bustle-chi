module Rad.QL.Define.Util where

import Data.Monoid ((<>))

import Rad.QL.Internal.Builders
import Rad.QL.Internal.Types

import Rad.QL.AST
import Rad.QL.Types

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

class (Monad m) => Describable m where
  describe :: Description -> m ()

infixr 8 $..
($..) :: (Description -> a) -> Description -> a
($..) = ($)

infixr 9 |.., |--
(|..) :: Description -> Description -> Description
d1 |.. d2 = d1 <> "\n" <> d2
(|--) :: Description -> Description -> Description
d1 |-- d2 = d1 <> "\n\n" <> d2

-- The deprecateable class, allows use of the `deprecate` method to append a description

class (Monad m) => Deprecatable m where
  deprecate :: Deprecation -> m ()

-- undefined object, useful for stubbing out methods

data UNDEFINED = UNDEFINED

instance GraphQLScalar UNDEFINED where
  serialize _ = buildString "UNDEFINED"
  deserialize _ = Nothing

instance (Monad m) => GraphQLValue m UNDEFINED
instance (Monad m) => GraphQLType SCALAR m UNDEFINED where
  def = emptyDef
