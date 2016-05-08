module Rad.QL.Internal.Builders
  ( buildNull
  , buildString
  , joinObject
  , joinList
  , module Data.ByteString.Builder
  ) where

import qualified Data.ByteString as B
import           Data.ByteString.Builder
import           Data.Monoid ((<>))
import           Data.Word8 (Word8)

buildNull :: Builder
buildNull = byteString "null"

buildString :: B.ByteString -> Builder
buildString x = charUtf8 '"' <> B.foldl escape mempty x <> charUtf8 '"'
  -- TODO: check for more rescapable character
  where escape b 0x22 = b <> charUtf8 '\\' <> charUtf8 '"'
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
