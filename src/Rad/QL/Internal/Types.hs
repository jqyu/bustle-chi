{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables #-}

module Rad.QL.Internal.Types where

import           Control.Arrow (first, second, (&&&))
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import           Data.Monoid ((<>))
import           Data.Word8 (Word8)

import Rad.QL.AST

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

collectResults :: forall m. (Monad m) => [Result m] -> Result m
collectResults rs = joinList <$> sequenceA rs

-- subresult utils

nullResult :: Result m
nullResult = SubResult (buildNull, [])

errorMsg :: B.ByteString -> Result m
errorMsg e = SubResult (buildNull, [e])

buildNull :: Builder
buildNull = byteString "null"

buildString :: B.ByteString -> Builder
buildString x = charUtf8 '"' <> B.foldl escape mempty x <> charUtf8 '"'
  -- TODO: check for more rescapable character
  where escape :: Builder -> Word8 -> Builder
        escape b 0x22 = b <> charUtf8 '\\' <> charUtf8 '"'
        escape b 0x5c = b <> charUtf8 '\\' <> charUtf8 '\\'
        escape b 0x0d = b <> charUtf8 '\\' <> charUtf8 'r'
        escape b 0x0a = b <> charUtf8 '\\' <> charUtf8 'n'
        escape b c    = b <> word8 c

joinObject :: [ Builder ] -> Builder
joinObject vals = charUtf8 '{'
               <> joinComma vals
               <> charUtf8 '}'

joinList :: [ Builder ] -> Builder
joinList vals = charUtf8 '['
             <> joinComma vals
             <> charUtf8 ']'

joinComma :: [ Builder ] -> Builder
joinComma = joinBuilders $ charUtf8 ','

joinBuilders :: Builder -> [ Builder ] -> Builder
joinBuilders _    []  = mempty
joinBuilders j (x:xs) = x <> mconcat [ j <> x' | x' <- xs ]

-- | GraphQLTypeDef

data GraphQLTypeDef k m a = GraphQLTypeDef
  { gqlTypeDef :: TypeDef
  , gqlResolve :: SelectionSet -> a -> Result m
  }
