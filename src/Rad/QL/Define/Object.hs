{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , OverloadedStrings
           , ScopedTypeVariables #-}

module Rad.QL.Define.Object where

import Data.Monoid ((<>))

import Rad.QL.AST
import Rad.QL.Define.Util
import Rad.QL.Define.Field
import Rad.QL.Internal.Types
import Rad.QL.Types

defineObject :: Name -> ObjectDefM m a b -> GraphQLTypeDef OBJECT m a
defineObject n def = GraphQLTypeDef { gqlTypeDef = td
                                    , gqlResolve = res
                                    }
  where td = TypeDefObject $ ObjectTypeDef n "" [] []
        res = undefined

implements :: (GraphQLType INTERFACE m b) => (a -> b) -> ObjectDefM m a ()
implements = undefined

-- example:

data Bar = Bar Int

instance (Monad m) => GraphQLValue m Bar
instance (Monad m) => GraphQLType OBJECT m Bar where

  def = defineObject "Bar" $ do

    describe "some filler type"

    field "bar" $ do
      describe "a horse walks into one"
      resolve $-> \_ _ ->
        Foo 1 2


data Foo = Foo Int Int

instance (Monad m) => GraphQLValue m Foo
instance (Monad m) => GraphQLType OBJECT m Foo where

  def = defineObject "Foo" $ do

    describe "This is a code sample showing how to use the object definition monad"
      |.. "Here we demonstrate a describe call with multiple fields."
      |.. "Because this is a (trivial) monad, we can use do notation to easily chain"
      |.. "in a way that's much cleaner than your traditional JS/Java builder pattern"

    describe "In the future we this will be used to annotate types with middlewares"
      |.. "and property annotations which can be used to automatically generate tests"

    field "myFoo" $ do
      describe "description of myFoo"
      limit  <- arg "limit" |= 30
        @> describe "Number of elements to fetch"
              |.. "i love playing with syntax"
              |.. "it tickles my private parts"
        @> validate positiveInt
      cursor <- arg "cursor" |= "0"
        @> describe "last element cursor"
        :: Arg m Foo Name
      includeDepr <- arg "includeDeprecated" |= False
      resolve $-> \_ _ ->
        Bar 1

positiveInt :: Int -> Validation
positiveInt x | x > 0     = OK
              | otherwise = ERR "int is non-positive"

-- object definition monad, used to trick the do notation into doing what we want
-- rebindable syntax seemed like overkill

data ObjectDefM m a b = ObjectDefM
  { objectDefDesc       :: Description
  , objectDefFields     :: [GraphQLFieldDef m a]
  , objectDefInterfaces :: Interfaces
  }

instance DefinitionBuilder (ObjectDefM m a) where
  unit = ObjectDefM
    { objectDefDesc       = ""
    , objectDefFields     = []
    , objectDefInterfaces = []
    }
  merge x y = ObjectDefM
    { objectDefDesc       = objectDefDesc       x <> objectDefDesc       y
    , objectDefFields     = objectDefFields     y <> objectDefFields     x -- reversed for shadowing
    , objectDefInterfaces = objectDefInterfaces x <> objectDefInterfaces y
    }

instance Functor     (ObjectDefM m a) where fmap  = fmapDef
instance Applicative (ObjectDefM m a) where (<*>) = applyDef ; pure _ = unit
instance Monad       (ObjectDefM m a) where (>>=) = bindDef  ; (>>)   = seqDef

instance Describeable (ObjectDefM m a) where
  describe d = unit { objectDefDesc = d }

instance HasFields ObjectDefM m a b where
  fieldSingleton f = unit { objectDefFields = [f] }
