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

import Bustle.QL as Bustle


data Payload = Payload
  { query     :: Text
  , variables :: Maybe Value
  } deriving (Generic, Show)
instance JSON.FromJSON Payload

app :: S.ScottyM ()
app = do

  S.middleware Logger.logStdoutDev

  -- GraphiQL
  S.get "/" $ do
    S.setHeader "Content-Type" "text/html"
    S.file "static/index.html"

  -- GraphQL Response
  S.post "/" $ do
    payload <- S.jsonData :: S.ActionM Payload
    let q = query     payload
        v = variables payload
    e <- liftIO $ Bustle.initBustleEnv Bustle.Development
    r <- liftIO $ runHaxl e $
      Bustle.run (TE.encodeUtf8 q) (fromMaybe JSON.Null v)
    S.setHeader "Content-Type" "application/json; charset=utf-8"
    S.raw r

main :: IO ()
main = S.scotty 3000 app
