module Rad.QL
  ( module Rad.QL.Types
  , module Rad.QL.Define

  , Schema
  , runQuery

  ) where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC8
import           Data.Monoid           ((<>))
import qualified Data.Aeson            as JSON

import Rad.QL.Internal.Builders
import Rad.QL.Internal.Types

import Rad.QL.AST
import Rad.QL.Parser (parseDocument)
import Rad.QL.Types
import Rad.QL.Query (Schema, execute)
import Rad.QL.Define


runQuery :: (Monad m) => Schema m -> B.ByteString -> JSON.Value -> m Builder
runQuery s q v =  collect
              <$> either (\e -> return (byteString "null", [ BC8.pack e ]))
                         (execute s v)
                         (parseDocument q)
  where collect (res, []) = byteString "{\"data\":" <> res <> byteString "}"
        collect (res, es) = byteString "{\"data\":"
                         <> res
                         <> byteString ", \"errors\": "
                         <> joinList [ buildString e | e <- es ]
                         <> byteString "}"
