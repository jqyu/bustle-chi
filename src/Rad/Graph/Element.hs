module Rad.Graph.Element
  ( Id(..)
  ) where

import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import Data.Typeable
import Haxl.Core

newtype Id = Id ByteString

class (Eq a, Show a, Typeable a) => Element a where
  getId :: a -> Id

attr :: (Element l) => ByteString -> l -> GenHaxl u (Maybe ByteString)
attr a l = return $ Just a

attr' :: (Element l) => ByteString -> l -> GenHaxl u ByteString
attr' a = attr a >=> liftRequire

attrInt :: (Element l) => ByteString -> l -> GenHaxl u (Maybe Int)
attrInt a = fmap parseInt . attr a

-- decorators

liftRequire :: Maybe a -> GenHaxl u a
liftRequire (Just x) = return x
liftRequire Nothing  = undefined -- TODO: throw error

-- parsers

parseInt :: Maybe ByteString -> Maybe Int
parseInt = undefined
