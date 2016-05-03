{-# LANGUAGE ConstraintKinds,
             ExistentialQuantification,
             OverloadedStrings,
             ScopedTypeVariables #-}

module Rad.QL.Resolver.Field where

import Control.Monad.Except

import Rad.QL.AST
import Rad.QL.Error
import Rad.QL.Resolver.Arguments
import Rad.QL.Resolver.Types

-- Field constructor

type FieldResolver' m a = TypeDict m -> a -> [Argument] -> Resolver m

data FieldResolver m a = (Monad m) => FieldResolver FieldDef (FieldResolver' m a)

-- Infix operators for defining fields

-- definition binding operator
-- a definition bind consists of
-- Context Argument Error? Bind
-- ($|*) -> function depends on context,   $ means yes, * means no
-- (-|~) -> function depends on arguments, - means yes, ~ means no
-- ?     -> function might return an error
-- >     -> function returns a pure value
-- >>    -> function returns a monad
-- e.g. $~?>> means a function which requires context (parent value)
--            takes no arguments, and returns Either RadQLErr b
--      $~?>> (a -> Either RadQLErr b)
-- e.g. *-?>  means a function which requires no context
--            takes arguments, and returns ExceptT RadQLErr m b
--      *-?>  (args -> RadQLErrT m b)
-- e.g. $~>   means a function which requires context (parent value)
--            takes no arguments, and returns m b
--      $~>   (a -> m b)
infixr 0 $->, $-?>, $->>, $-?>>,
         $~>, $~?>, $~>>, $~?>>,
         *->, *-?>, *->>, *-?>>,
         *~>, *~?>, *~>>, *~?>>

type Valid m a b args = (Monad m, GraphQLNamed a, GraphQLValue b, GraphQLArgs args)

defineField :: forall m a b args. (Valid m a b args)
            => Doc -> b -> args -> FieldResolver' m a -> FieldResolver m a
defineField (Doc n d) _ _ = FieldResolver def
  where args = undefined :: args
        b    = undefined :: b
        def  = FieldDef n d (argsDef args) (typeRef b) (typeDef b)

-- Define a field that takes arguments

($->) :: forall m a b args. (Valid m a b args)
      => Doc -> (a -> args -> b) -> FieldResolver m a
f $-> fn = defineField f (undefined :: b) (undefined :: args) resolver
  where resolver dict a args =
          let resolve' = resolve dict
              in case fn a <$> castArgs args of
                      Just b -> resolve' b
                      Nothing -> \_ -> errMsg "Incorrect arguments"

($-?>) :: forall m a b args. (Valid m a b args)
       => Doc -> (a -> args -> RadQLErr b) -> FieldResolver m a
f $-?> fn = defineField f (undefined :: b) (undefined :: args) resolver
  where resolver dict a args =
          let resolve' = resolve dict
              in case fn a <$> castArgs args of
                      Just (Right b) -> resolve' b
                      Just (Left e)  -> \_ -> errMsg e
                      Nothing        -> \_ -> errMsg "Incorrect arguments"

($->>) :: forall m a b args. (Valid m a b args)
       => Doc -> (a -> args -> m b) -> FieldResolver m a
f $->> fn = defineField f (undefined :: b) (undefined :: args) resolver
  where resolver dict a args =
          let resolve' b ss = join $ resolve dict <$> b <*> pure ss
              in maybe (\_ -> errMsg "Incorrect arguments") -- INEFFICIENT: FIX
                 resolve'
                 (fn a <$> castArgs args)

($-?>>) :: forall m a b args. (Valid m a b args)
        => Doc -> (a -> args -> RadQLErrT m b) -> FieldResolver m a
f $-?>> fn = defineField f (undefined :: b) (undefined :: args) resolver
  where resolver dict a args =
          let resolve' b ss = join $ resolve'' <$> runExceptT b <*> pure ss
              resolve'' (Left e ) = \_ -> errMsg e
              resolve'' (Right b) = resolve dict b
              in maybe (\_ -> errMsg "Incorrect Arguments")
                       resolve'
                       (fn a <$> castArgs args)


-- A field that takes arguments, but no context

(*->) :: forall m a b args. (Valid m a b args)
       => Doc -> (args -> b) -> FieldResolver m a
f *-> fn = f $-> \_ -> fn

(*-?>) :: forall m a b args. (Valid m a b args)
        => Doc -> (args -> RadQLErr b) -> FieldResolver m a
f *-?> fn = f $-?> \_ -> fn

(*->>) :: forall m a b args. (Valid m a b args)
       => Doc -> (args -> m b) -> FieldResolver m a
f *->> fn = f $->> \_ -> fn

(*-?>>) :: forall m a b args. (Valid m a b args)
        => Doc -> (args -> RadQLErrT m b) -> FieldResolver m a
f *-?>> fn = f $-?>> \_ -> fn


type Valid' m a b = (Monad m, GraphQLObject a, GraphQLValue b)

-- A field that takes no arguments

($~>) :: forall m a b. (Valid' m a b)
      => Doc -> (a -> b) -> FieldResolver m a
f $~> fn = f $-> fn'
  where fn' :: a -> NoArgs -> b
        fn' x _ = fn x

($~?>) :: forall m a b. (Valid' m a b)
       => Doc -> (a -> RadQLErr b) -> FieldResolver m a
f $~?> fn = f $-?> fn'
  where fn' :: a -> NoArgs -> RadQLErr b
        fn' x _ = fn x

($~>>) :: forall m a b. (Valid' m a b)
       => Doc -> (a -> m b) -> FieldResolver m a
f $~>> fn = f $->> fn'
  where fn' :: a -> NoArgs -> m b
        fn' x _ = fn x

($~?>>) :: forall m a b. (Valid' m a b)
        => Doc -> (a -> RadQLErrT m b) -> FieldResolver m a
f $~?>> fn = f $-?>> fn'
  where fn' :: a -> NoArgs -> RadQLErrT m b
        fn' x _ = fn x

-- A field that takes no arguments or context

(*~>) :: forall m a b. (Valid' m a b)
       => Doc -> b -> FieldResolver m a
f *~> b = f $~> \_ -> b

-- this should never actually be used
(*~?>) :: forall m a b. (Valid' m a b)
       => Doc -> (RadQLErr b) -> FieldResolver m a
f *~?> b = f $~?> \_ -> b

(*~>>) :: forall m a b. (Valid' m a b)
       => Doc -> m b -> FieldResolver m a
f *~>> b = f $~>> \_ -> b

(*~?>>) :: forall m a b. (Valid' m a b)
       => Doc -> RadQLErrT m b -> FieldResolver m a
f *~?>> b = f $~?>> \_ -> b


