{-# LANGUAGE DeriveGeneric
           , OverloadedStrings #-} 

module Main where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (Value(..), FromJSON)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text.Encoding as TE
import           GHC.Generics
import           Haxl.Core
import qualified Network.Wai.Middleware.RequestLogger as Logger
import qualified Web.Scotty as S

import Bustle.Env
import Bustle.QL.Schema


data Payload = Payload
  { query     :: Text
  , variables :: Maybe Value
  } deriving (Generic, Show)
instance FromJSON Payload

app :: S.ScottyM ()
app = do

  S.middleware Logger.logStdoutDev

  -- GraphiQL
  S.get "/" $ do
    S.setHeader "Content-Type" "text/html"
    S.file "static/index.html"

  -- GraphQL Response
  S.post "/" $ do
    -- b <- S.jsonData :: S.ActionM Payload
    -- e <- liftIO $ Bustle.initBustleEnv Bustle.Development
    -- r <- liftIO $
    --   runHaxl e $
    --     Bustle.run (TE.encodeUtf8 $ query b) (fromMaybe Null $ variables b)
    S.setHeader "Content-Type" "application/json; charset=utf-8"
    S.raw "ok"

main :: IO ()
main = S.scotty 3000 app
