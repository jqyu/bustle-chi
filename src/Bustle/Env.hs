{-# LANGUAGE OverloadedStrings #-}

module Bustle.Env where

import Data.ByteString (ByteString)
import Haxl.Core

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

initBustleEnv :: Stage -> IO (Env BustleEnv)
initBustleEnv st = initEnv stateEmpty bustleEnv
  where bustleEnv = BustleEnv { stage   = st
                              , session = Unauthed
                              }
