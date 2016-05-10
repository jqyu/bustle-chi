{-# LANGUAGE KindSignatures
           , ScopedTypeVariables #-}

module Rad.QL.Define.Enum where

import           Data.Monoid ((<>))
import qualified Data.Trie as Trie

import Rad.QL.Internal.GEnum
import Rad.QL.Internal.Types

import Rad.QL.Define.Util

import Rad.QL.AST
import Rad.QL.Types

defineEnum :: forall m a b. (Monad m, IsEnum a, GraphQLScalar a)
           => Name -> EnumDefM b -> GraphQLTypeDef ENUM m a
defineEnum n def = emptyDef
    { gqlTypeDef = TypeDefEnum td
    , gqlResolve = resolveScalar
    }
  where td   = EnumTypeDef n (edDesc def) (zipValueDefs vals $ edValues def)
        vals = enumValues (undefined :: a)

enumValue :: Name -> EnumValueDefM a -> EnumDefM ()
enumValue n def = unit { edValues = Trie.singleton n (evdDesc def, evdDepr def) }

zipValueDefs :: [EnumValueDef] -> Trie.Trie (Description, Description) -> [EnumValueDef]
zipValueDefs ds as = map zipValueDef ds
  where zipValueDef e@(EnumValueDef n _ _) =
          case Trie.lookup n as of
               Just (desc, depr) -> EnumValueDef n desc depr
               Nothing           -> e

-- builder monads
data EnumDefM a = EnumDefM
  { edDesc   :: Description
  , edValues :: Trie.Trie (Description, Description)
  , unwrap   :: a
  }

instance Functor EnumDefM where
  fmap f x = pure f <*> x
instance Applicative EnumDefM where
  pure x = EnumDefM
    { edDesc   = ""
    , edValues = Trie.empty
    , unwrap   = x
    }
  f <*> x = EnumDefM
    { edDesc   = edDesc   f <> edDesc   x
    , edValues = edValues f <> edValues x
    , unwrap   = unwrap   f $  unwrap   x
    }
instance Monad EnumDefM where
  m >>= k = m >> k (unwrap m)
  m >> k = m { unwrap = id } <*> k

instance Describable EnumDefM where
  describe d = unit { edDesc = d }

data EnumValueDefM a = EnumValueDefM
  { evdDesc :: Description
  , evdDepr :: Description
  , unwrapV :: a
  }

instance Functor EnumValueDefM where
  fmap f x = pure f <*> x
instance Applicative EnumValueDefM where
  pure x = EnumValueDefM
    { evdDesc = ""
    , evdDepr = ""
    , unwrapV = x
    }
  f <*> x = EnumValueDefM
    { evdDesc = evdDesc f <> evdDesc x
    , evdDepr = evdDepr f <> evdDepr x
    , unwrapV = unwrapV f $  unwrapV x
    }
instance Monad EnumValueDefM where
  m >>= k = m >> k (unwrapV m)
  m >> k  = m { unwrapV = id } <*> k

instance Describable EnumValueDefM
  where describe d = unit { evdDesc = d }

instance Deprecatable EnumValueDefM
  where deprecate d = unit { evdDepr = d }
