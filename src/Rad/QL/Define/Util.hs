module Rad.QL.Define.Util where

import Data.Monoid ((<>))
import Unsafe.Coerce

import Rad.QL.Internal.Builders
import Rad.QL.Internal.Types

import Rad.QL.AST
import Rad.QL.Types

unit :: (Applicative m) => m ()
unit = pure ()

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
