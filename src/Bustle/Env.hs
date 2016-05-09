module Bustle.Env
  ( Stage(..)
  , Session(..)
  , BustleEnv(..)
  , Haxl

  -- acts as a library prelude
  , module GHC.Generics
  , module Haxl.Core
  , module Rad.QL

  ) where

import Data.ByteString (ByteString)
import GHC.Generics
import Haxl.Core

import Rad.QL

data Stage = Production
           | Beta
           | Development

data Session = Unauthed
             | Authed Int ByteString

data BustleEnv = BustleEnv
  { stage   :: Stage
  , session :: Session
  }

type Haxl = GenHaxl BustleEnv
