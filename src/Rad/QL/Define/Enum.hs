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

enumValue :: Name -> EnumValueDefM a -> EnumDefM b
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
  }

instance DefinitionBuilder EnumDefM where
  unit = EnumDefM { edDesc = "", edValues = Trie.empty }
  merge x y = EnumDefM
    { edDesc   = edDesc   x <> edDesc   y
    , edValues = edValues x <> edValues y
    }

instance Functor     EnumDefM where fmap  = fmapDef
instance Applicative EnumDefM where (<*>) = applyDef ; pure _ = unit
instance Monad       EnumDefM where (>>=) = bindDef  ; (>>)   = seqDef

instance Describable EnumDefM where
  describe d = unit { edDesc = d }

data EnumValueDefM a = EnumValueDefM
  { evdDesc :: Description
  , evdDepr :: Description
  }

instance DefinitionBuilder EnumValueDefM where
  unit = EnumValueDefM { evdDesc = "", evdDepr = "" }
  merge x y = EnumValueDefM
    { evdDesc = evdDesc x <> evdDesc y
    , evdDepr = evdDepr x <> evdDepr y
    }

instance Functor     EnumValueDefM where fmap  = fmapDef
instance Applicative EnumValueDefM where (<*>) = applyDef ; pure _ = unit
instance Monad       EnumValueDefM where (>>=) = bindDef  ; (>>)   = seqDef

instance Describable EnumValueDefM
  where describe d = unit { evdDesc = d }

instance Deprecatable EnumValueDefM
  where deprecate d = unit { evdDepr = d }
