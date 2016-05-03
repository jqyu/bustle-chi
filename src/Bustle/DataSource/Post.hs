{-# LANGUAGE EmptyDataDecls,
             FlexibleInstances,
             GADTs,
             MultiParamTypeClasses,
             OverloadedStrings,
             TypeFamilies,
             TypeSynonymInstances #-}

module Bustle.DataSource.Post
  ( Req
  , initState
  , exists
  , index
  , range
  , attr
  , intAttr
--  , jsonAttr
  ) where

import Prelude hiding (max, min)

import           Data.ByteString      (ByteString)
import qualified Database.Redis       as Redis
import           Haxl.Core
import qualified Rad.Redis.DataSource as RadRedis

import           Bustle.Env (BustleEnv, Haxl)

data Post

type Req = RadRedis.Req Post

-- Haxl Instances

instance StateKey Req where
  data State Req = State
    { conn      :: Redis.Connection
    , keyPrefix :: ByteString
    }

instance DataSourceName Req where
  dataSourceName _ = "Post Source (Radredis)"

instance DataSource BustleEnv Req where
  fetch st _ _ = RadRedis.fetchReqs c kp
    where c  = conn      st
          kp = keyPrefix st

-- Init a global state instance from connection deets

initState :: Redis.ConnectInfo -> ByteString -> IO (State Req)
initState cinfo kp = fromConn <$> Redis.connect cinfo
  where fromConn c = State { conn      = c
                           , keyPrefix = kp
                           }

-- Fetchers

exists :: Int -> Haxl Bool
exists id = dataFetch req
  where req = RadRedis.Exists id :: Req Bool

index :: ByteString -> Int -> Int -> Haxl [Int]
index idx ofs lmt = dataFetch req
  where req = RadRedis.Index idx ofs lmt :: Req [Int]

range :: ByteString -> Double -> Double -> Int -> Int -> Haxl [Int]
range idx max min ofs lmt = dataFetch req
  where req = RadRedis.Range idx max min ofs lmt :: Req [Int]

attr :: ByteString -> Int -> Haxl (Maybe ByteString)
attr a id = dataFetch req
  where req = RadRedis.Attr a id :: Req (Maybe ByteString)

intAttr :: ByteString -> Int -> Haxl (Maybe Int)
intAttr a = RadRedis.attrToInt . attr a
