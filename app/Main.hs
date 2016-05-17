{-# LANGUAGE DeriveGeneric
           , OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson as JSON
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text.Encoding      as TE
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           GHC.Generics
import           Haxl.Core
import qualified Network.Wai.Middleware.RequestLogger as Logger
import qualified Web.Scotty as S

-- debugging stuff
import Text.Printf
import System.CPUTime

-- application
import Bustle.QL as Bustle

data Payload = Payload
  { query     :: Text
  , variables :: Maybe Value
  } deriving (Generic, Show)
instance JSON.FromJSON Payload

app :: StateStore -> S.ScottyM ()
app state = do

  S.middleware Logger.logStdoutDev

  -- GraphiQL
  S.get "/" $ do
    S.setHeader "Content-Type" "text/html"
    S.file "static/graphiql.html"

  -- GraphQL Response
  S.post "/" $ do
    payload <- S.jsonData :: S.ActionM Payload
    let q = query     payload
        v = variables payload
    e <- liftIO $ Bustle.initBustleEnv state Bustle.Development
    r <- liftIO $ runHaxl e $
      Bustle.run (TE.encodeUtf8 q) (fromMaybe JSON.Null v)
    S.setHeader "Content-Type" "application/json; charset=utf-8"
    S.raw r


main :: IO ()
main = do
  printf "Initializing state..."
  start <- getCPUTime
  state <- initState
  end <- getCPUTime
  let d = fromIntegral (end - start :: Integer) / 10^12 :: Double
  printf " ...state init took %0.4f sec\n" d
  S.scotty 3000 (app state)
