{-# LANGUAGE LambdaCase,
             OverloadedStrings #-}

module Rad.QL.Error where

import Control.Arrow ((&&&))
import Control.Monad.Except
import Data.ByteString (ByteString)
import Data.ByteString.Builder

-- see Data.GraphQL.Error in graphql-haskell for a more general implementation

type RadQLError = ByteString
type RadQLErr   = Either  RadQLError
type RadQLErrT  = ExceptT RadQLError
type CollectErrs m a = m (a, [RadQLError])

collectErrs :: (Monad m, Traversable t)
            => m (t (a, [b]))
            -> m (t a, [b])
collectErrs = fmap (fmap fst &&& concatMap snd)

collectList :: (Monad m)
            => [CollectErrs m a]
            -> CollectErrs m [a]
collectList = collectErrs
            . sequenceA

errMsg :: (Monad m) => ByteString -> CollectErrs m Builder
errMsg m = return (byteString "null", [m])
