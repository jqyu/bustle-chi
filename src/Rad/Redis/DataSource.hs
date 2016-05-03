{-# LANGUAGE DeriveDataTypeable,
             FlexibleContexts,
             GADTs,
             MultiParamTypeClasses,
             OverloadedStrings,
             StandaloneDeriving #-}

module Rad.Redis.DataSource
  ( Req(..)
  , fetchReqs
  , attrToInt
  ) where

import Prelude hiding (id, max, min)

import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad.IO.Class  (liftIO)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as BC8
import           Data.Foldable           (traverse_)
import           Data.Hashable
import           Data.List               (groupBy, partition)
import           Data.Maybe
import           Data.Monoid             ((<>))
import           Data.Text.Encoding      (decodeUtf8)
import           Data.Typeable
import           Database.Redis
import           Haxl.Core

-- debugging stuff
import           Text.Printf
import           System.CPUTime

-- Req GADT

data Req a b where
  Exists :: Int -> Req a Bool
  Index  :: ByteString -> Int -> Int -> Req a [Int]
  Range  :: ByteString -> Double -> Double -> Int -> Int -> Req a [Int]
  Attr   :: ByteString -> Int -> Req a (Maybe ByteString)
  deriving (Typeable)

deriving instance Eq   (Req a b)
deriving instance Show (Req a b)

instance Show1 (Req a) where show1 = show

-- Hashable instances

instance Hashable (Req a b) where
  hashWithSalt s (Exists id                 ) = hashWithSalt s (0 :: Int, id                     )
  hashWithSalt s (Index  idx         ofs lmt) = hashWithSalt s (1 :: Int, idx,           ofs, lmt)
  hashWithSalt s (Range  idx max min ofs lmt) = hashWithSalt s (2 :: Int, idx, max, min, ofs, lmt)
  hashWithSalt s (Attr   attr id            ) = hashWithSalt s (3 :: Int, attr, id               )

-- Haxl fetcher

fetchReqs :: Connection
          -> ByteString
          -> [BlockedFetch (Req a)]
          -> PerformFetch

fetchReqs c kp bfs = AsyncFetch $ \inner -> do
  printf "Staring Radredis Request...\n"
  start <- getCPUTime
  asyncReq <- async         -- run asyncrnously
    $ runRedis c            -- execute redis pipeline
    $ buildPipeline kp bfs  -- construct pipeline
  inner
  wait asyncReq
  end <- getCPUTime
  let d = fromIntegral (end - start) / 10^12 :: Double
  printf "... request took %0.4f sec\n" d

buildPipeline :: ByteString -> [BlockedFetch (Req a)] -> Redis ()
buildPipeline kp bfs = batchhget kp hgets
                    *> traverse_ (fetchDirect kp) rest
  where (hgets, rest) = partition isHGET bfs

isHGET :: BlockedFetch (Req a) -> Bool
isHGET (BlockedFetch (Attr _ _) _) = True
isHGET _                           = False

-- directly resolve a BlockedFetch

fetchDirect :: ByteString -> BlockedFetch (Req a) -> Redis ()
-- resolve an EXISTS requests
fetchDirect kp (BlockedFetch (Exists id) rvar) =
    exists (keyOf kp id) >>= putRes rvar
-- resolve an INDEX request
fetchDirect kp (BlockedFetch (Index idx ofs lmt) rvar) =
    fmap (mapMaybe toInt) <$> zrevrange
                              (kp <> "indexes:" <> idx)
                              start stop
    >>= putRes rvar
  where start = toInteger   ofs
        stop  = toInteger $ ofs + lmt - 1
-- resolve a RANGE request
fetchDirect kp (BlockedFetch (Range idx max min ofs lmt) rvar) =
    fmap (mapMaybe toInt) <$> zrevrangebyscoreLimit
                              (kp <> "indexes:" <> idx)
                              max min offset count
    >>= putRes rvar
  where offset = toInteger ofs
        count  = toInteger lmt
-- this should never happen
fetchDirect _ (BlockedFetch _ rvar) = putRes rvar $ Left (Error "Job partitioning failure")

-- Batch a group of of HGETs into HMGETs

batchhget :: ByteString -> [BlockedFetch (Req a)] -> Redis ()
batchhget kp bfs = traverse_ (\xs -> fetchhmget kp (fst $ head xs) (map snd xs))
                 $ groupBy (\x y -> fst x == fst y)
                 $ map extractAction bfs

type AttrResult = Either Reply (Maybe ByteString)
type AttrAction = AttrResult -> Redis ()

extractAction :: BlockedFetch (Req a) -> (Int, (ByteString, AttrAction))
extractAction (BlockedFetch (Attr attr id) rvar) = (id, (attr, \x -> putRes rvar x))
extractAction (BlockedFetch _ rvar) = (0, ("", \_ -> putRes rvar $ Left (Error "Job partitioning failure")))

fetchhmget :: ByteString -> Int -> [(ByteString, AttrAction)] -> Redis ()
fetchhmget kp id jobs = res >>= resolver
  where res     = hmget (keyOf kp id) attrs
        attrs   = map fst jobs
        actions = map snd jobs
        resolver :: Either Reply [Maybe ByteString] -> Redis ()
        resolver (Right a) = traverse_ (\x -> fst x $ snd x)
                           $ zip actions
                           $ map Right a
        resolver (Left ex) = traverse_ ($ Left ex) actions

-- Utility functions

attrToInt :: GenHaxl u (Maybe ByteString) -> GenHaxl u (Maybe Int)
attrToInt = fmap (>>= toInt)

putRes :: ResultVar a -> Either Reply a -> Redis ()
putRes rvar (Left ex) = liftIO $ putFailure rvar (replyToException ex)
putRes rvar (Right a) = liftIO $ putSuccess rvar a

replyToException :: Reply -> SomeException
replyToException (Error e) = SomeException $ FetchError $ decodeUtf8 e
replyToException _         = SomeException $ FetchError "what the flying fuck"

toInt :: ByteString -> Maybe Int
toInt n = case BC8.readInt n of
               Just (n', _) -> Just n'
               Nothing      -> Nothing

keyOf :: ByteString -> Int -> ByteString
keyOf kp i = kp <> intStr <> ":attributes"
  where intStr :: ByteString
        intStr = BC8.pack $ show i
