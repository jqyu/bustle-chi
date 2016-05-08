module Rad.QL.Internal.Types where

import           Control.Arrow (first, second, (&&&))
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import           Data.Monoid ((<>))

import Rad.QL.AST
import Rad.QL.Internal.Builders
import Rad.QL.Query

-- | Subresult

data SubResult m a = SubResult     (a, [ B.ByteString ])
                   | SubResultM (m (a, [ B.ByteString ]))

instance (Functor m) => Functor (SubResult m) where
  fmap f (SubResult  (a, errs)) = SubResult (f a, errs)
  fmap f (SubResultM wrapped)   = SubResultM $ fmap (first f) wrapped

applyErrs :: ((a -> b), [d]) -> (a, [d]) -> (b, [d])
applyErrs (f, errs) (x, errs') = (f x, errs <> errs')

instance (Applicative m) => Applicative (SubResult m) where
  pure x = SubResult (x, [])
  (SubResult  f) <*> (SubResult  x) = SubResult  $ applyErrs     f     x
  (SubResult  f) <*> (SubResultM x) = SubResultM $ applyErrs     f <$> x
  (SubResultM f) <*> (SubResult  x) = SubResultM $ fmap ($ x) (applyErrs <$> f)
  (SubResultM f) <*> (SubResultM x) = SubResultM $ applyErrs <$> f <*> x

type Result m = SubResult m Builder

collectResults :: (Monad m) => [Result m] -> Result m
collectResults rs = joinList <$> sequenceA rs

-- subresult utils

nullResult :: Result m
nullResult = SubResult (buildNull, [])

errorMsg :: B.ByteString -> Result m
errorMsg e = SubResult (buildNull, [e])

-- | GraphQLFieldDef

type FieldRunner m a = QArgs -> a -> QSelectionSet -> Result m

data GraphQLFieldDef m a = GraphQLFieldDef
  { fieldDef      :: FieldDef
  , fieldResolver :: FieldRunner m a
  }

castField :: (a -> b) -> GraphQLFieldDef m b -> GraphQLFieldDef m a
castField fn f = f { fieldResolver = r' }
  where r = fieldResolver f
        r' args = r args . fn

-- | GraphQLTypeDef

type Resolver m a = QSelectionSet -> a -> Result m

data GraphQLTypeDef k m a = GraphQLTypeDef
  { gqlTypeDef :: TypeDef
  , gqlResolve :: QSelectionSet -> a -> Result m
  , gqlFields  :: [GraphQLFieldDef m a]
  }

castResolve :: (a -> b) -> Resolver m b -> Resolver m a
castResolve fn r args = r args . fn

undefinedTypeDef :: TypeDef
undefinedTypeDef = TypeDefScalar $ ScalarTypeDef "UndefinedType"
   $ "Type which denotes an undefined value, "
  <> "if you see this in your schema then one of your fields is missing an implementation"
  <> "\n TODO: raise a warning automatically if this appears"

undefinedTypeRef :: Type
undefinedTypeRef = TypeNamed $ NamedType "UndefinedType"

emptyDef :: GraphQLTypeDef k m a
emptyDef = GraphQLTypeDef
  { gqlTypeDef = undefinedTypeDef
  , gqlResolve = \_ _ -> errorMsg "UNDEFINED TYPE"
  , gqlFields  = []
  }
