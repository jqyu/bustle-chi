module Bustle.QL.API.Bustle
  ( BustleAPI(..)
  ) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import           Data.Foldable

import Bustle.Env

data BustleAPI = BustleAPI deriving (Eq, Show)

instance GraphQLValue Haxl BustleAPI
instance GraphQLType OBJECT Haxl BustleAPI where

  def = defineObject "Bustle" $ do

    describe "Core Bustle API"

    field "hello" $ resolve *~> ("world" :: B.ByteString)

    field "monadic" $ do
      test <- arg "test" @> do
        describe
          $.. "monadic code is really neat because instead of memorizing functions"
          |.. "and their parameters, you everything is a generalization of function composition."
          |-- "you can think of it as the builder pattern on steroids"
          |-- "this input is only valid if it's a positive prime"
        validate $ greaterThan 0
        validate $ lessThan 1000
      resolve *->> \args -> do
        let n = test args
        return [1..n]

greaterThan :: Int -> Int -> Validation
greaterThan m n | n > m     = OK
                | otherwise = ERR "number too small!"

lessThan :: Int -> Int -> Validation
lessThan m n | n < m     = OK
             | otherwise = ERR "number too big!"
