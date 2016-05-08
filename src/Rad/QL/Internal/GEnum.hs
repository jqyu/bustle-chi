{-# LANGUAGE ConstraintKinds
           , ScopedTypeVariables
           , TypeOperators #-}

module Rad.QL.Internal.GEnum where

import           Control.Applicative   ((<|>))
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC8
import           Data.Monoid           ((<>))
import           GHC.Generics

import Rad.QL.Internal.Builders

import Rad.QL.AST
import Rad.QL.Query

type IsEnum a = (Generic a, GEnum (Rep a))

enumValues :: forall a. (IsEnum a) => a -> [EnumValueDef]
enumValues _ = gEnumValues $ from (undefined :: a)

enumSerialize :: forall a. (IsEnum a) => a -> Builder
enumSerialize = gEnumSerialize . from

enumDeserialize :: forall a. (IsEnum a) => QValue -> Maybe a
enumDeserialize (QString v) = to <$> gEnumDeserialize v
enumDeserialize _           = Nothing

class GEnum f where
  gEnumValues      :: f a -> [EnumValueDef]
  gEnumSerialize   :: f a -> Builder
  gEnumDeserialize :: Name -> Maybe (f a)

-- delegates data type instances upwards
instance (GEnum a, Datatype c) => GEnum (M1 D c a) where
  gEnumValues _         = gEnumValues (undefined :: a x)
  gEnumSerialize (M1 x) = gEnumSerialize x
  gEnumDeserialize n    = M1 <$> gEnumDeserialize n

-- handles sum types
instance (GEnum a, GEnum b) => GEnum (a :+: b) where
  gEnumValues _ = gEnumValues (undefined :: a x)
               <> gEnumValues (undefined :: b x)
  gEnumSerialize (L1 x) = gEnumSerialize x
  gEnumSerialize (R1 x) = gEnumSerialize x
  gEnumDeserialize s = L1 <$> gEnumDeserialize s
                   <|> R1 <$> gEnumDeserialize s

-- unitary enum constructor
instance (Constructor c) => GEnum (M1 C c U1) where
  gEnumValues    _ = [EnumValueDef (BC8.pack $ conName (undefined :: M1 C c U1 x)) "" ""]
  gEnumSerialize _ = buildString  $ BC8.pack $ conName (undefined :: M1 C c U1 x)
  gEnumDeserialize s | s    ==     (BC8.pack $ conName (undefined :: M1 C c U1 x)) = Just $ M1 U1
                     | otherwise                                                   = Nothing
