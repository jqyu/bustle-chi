{-# LANGUAGE DefaultSignatures,
             FlexibleContexts,
             FlexibleInstances,
             MultiParamTypeClasses,
             OverloadedStrings,
             ScopedTypeVariables,
             TypeOperators #-}

module Rad.QL.Resolver.Arguments where

import qualified Data.ByteString.Char8 as BC8
import qualified Data.List             as List
import           Data.Maybe            (fromMaybe)
import           Data.Monoid           ((<>))
import           Data.Typeable
import           GHC.Generics

import Rad.QL.AST
import Rad.QL.Resolver.Types

-- TODO: descriptive error messages for argument mismatches
class (Typeable a) => GraphQLArgs a where

  -- args def takes an undefined instance and returns the schema representation
  argsDef  :: a -> ArgumentsDef
  default argsDef :: (Generic a, GIsArgs (Rep a)) => a -> ArgumentsDef
  argsDef _ = gargDef $ from (undefined :: a)

  castArgs :: [Argument] -> Maybe a
  default castArgs :: (Generic a, GIsArgs (Rep a)) => [Argument] -> Maybe a
  castArgs args = to <$> gcastArgs args

data NoArgs = NoArgs deriving (Eq, Show, Typeable)

instance GraphQLArgs NoArgs where
  argsDef  _  = []
  castArgs [] = Just NoArgs
  castArgs _  = Nothing

-- generic instances for scrapping boilerplate

class GIsArgs f where
  gargDef   :: f a -> ArgumentsDef
  gcastArgs :: [Argument] -> Maybe (f a)

-- delegates data type instance upwards
instance (GIsArgs a, Datatype c) => GIsArgs (M1 D c a) where
  gargDef _ = gargDef (undefined :: a b)
  gcastArgs args = M1 <$> gcastArgs args

-- delegates constructor instance upwards
instance (GIsArgs a, Constructor c) => GIsArgs (M1 C c a) where
  gargDef _ = gargDef (undefined :: a b)
  gcastArgs args = M1 <$> gcastArgs args

-- we don't allow unions, this isn't well defined in GraphQL
-- although the parser would be trivial
-- gcastArgs args = InR <$> gcastArgs args
--              <|> InL <$> gcastArgs args

-- combines arg definitions of products
instance (GIsArgs a, GIsArgs b) => GIsArgs (a :*: b) where
  gargDef _ = gargDef (undefined :: a x)
           <> gargDef (undefined :: b x)
  gcastArgs args = (:*:) <$> gcastArgs args <*> gcastArgs args

-- define 
instance (GIsArg k i a, Selector c) => GIsArgs (M1 S c (k i a)) where
  gargDef s = [InputValueDef n "" t d Nothing] -- TODO: descriptions
    where n = BC8.pack $ selName s
          t = gargType    (undefined :: k i a x)
          d = gargTypeDef (undefined :: k i a x)
  gcastArgs = applyArgVal
            . fromMaybe (Argument n NoValue)
            . List.find matchArgName
    where s = undefined :: M1 S c (k i a) x
          n = BC8.pack (selName s)
          matchArgName (Argument n' _) = n' == n
          applyArgVal (Argument _ v) = M1 <$> gcastArg v

-- parses a single argument from a selector


-- parses from a kind
class GIsArg k i a where
  gcastArg    :: Value -> Maybe (k i a b)
  gargType    :: k i a b -> Type
  gargTypeDef :: k i a b -> TypeDef

instance (GraphQLScalar a, GraphQLValue a) => GIsArg K1 i a where
  gcastArg    v = K1 <$> fromValue v
  gargType    _ = typeRef (undefined :: a)
  gargTypeDef _ = typeDef (undefined :: a)
