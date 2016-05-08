module Bustle.QL
  ( run
  , initBustleEnv
  , Stage(..)
  ) where

import qualified Data.Aeson           as JSON
import           Data.ByteString      as B
import           Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as L
import           Rad.QL

import Bustle.Env
import Bustle.QL.Schema

run :: B.ByteString -> JSON.Value -> Haxl L.ByteString
run q v = fmap toLazyByteString $ runQuery schema q v
