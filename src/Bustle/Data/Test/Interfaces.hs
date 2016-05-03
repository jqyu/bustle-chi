{-# LANGUAGE ExistentialQuantification,
             OverloadedStrings,
             ScopedTypeVariables #-}

module Bustle.Data.Test.Interfaces where

import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Typeable

import Rad.QL

class (Typeable a) => IsNamed a where
  getName :: a -> ByteString

class (Typeable a) => IsDoubleNamed a where
  getFirstName  :: a -> ByteString
  getSecondName :: a -> ByteString
  getFullName   :: a -> ByteString
  getFullName a = getFirstName a <> " " <> getSecondName a
