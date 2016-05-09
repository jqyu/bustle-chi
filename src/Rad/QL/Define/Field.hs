{-# LANGUAGE ExistentialQuantification
           , ScopedTypeVariables #-}

module Rad.QL.Define.Field where

import qualified Data.ByteString as B
import           Data.ByteString.Builder
import           Data.Maybe (fromJust, fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Trie       as Trie

import Rad.QL.Internal.Builders
import Rad.QL.Internal.Types

import Rad.QL.AST
import Rad.QL.Define.Util
import Rad.QL.Query
import Rad.QL.Types

class HasFields d m a u where
  fieldSingleton :: GraphQLFieldDef m a -> d m a u

data FieldResolver m a b = (Monad m) => FieldResolver
  { runField :: FieldRunner m a }

field :: forall d m a b u. (HasFields d m a u, GraphQLValue m b)
      => Name -> FieldDefM m a (FieldResolver m a b) -> d m a u
field n fdef = fieldSingleton fdef'
  where fdef' = GraphQLFieldDef { fieldDef = def, fieldResolver = res }
        def = FieldDef n (fdDesc fdef)
                         (fdArgs fdef)
                         (graphQLValueTypeRef (undefined :: m b))
                         (graphQLValueTypeDef (undefined :: m b))
                         (fdDepr fdef)
        res args x = res' (performValidations args x $ fdVals fdef)
          where res' (ERR e) = \_ -> errorMsg e
                res' _       = runField (unwrap fdef) args x

-- special type name resolver
resolveTypeName :: (Monad m) => Name -> FieldRunner m a
resolveTypeName tn _ _ _ = pure $ buildString tn

performValidations :: a -> b -> [a -> b -> Validation] -> Validation
performValidations x y = go
  where go [] = OK
        go (v:vs) | val == OK = go vs
                  | otherwise = val
          where val = v x y

resolve :: Resolve m a b (FieldDefM m a (FieldResolver m a b))
resolve = pure

resolveStub :: (Monad m) => FieldDefM m a (FieldResolver m a UNDEFINED)
resolveStub = resolve *~> UNDEFINED

-- infixl 1 $->, $->?, $->>, $-?>>,
--          $~>, $~>?, $~>>, $~>>?,
--          *->, *->?, *->>, *->>?,
--          *~>, *~>?, *~>>, *~>>?

infixl 1 $->, $->>,
         $~>, $~>>,
         *->, *->>,
         *~>, *~>>

type Resolve m a b r = FieldResolver m a b -> r

-- TODO: ERROR HANDLING 

($->)  :: (GraphQLValue m b) => Resolve m a b r -> (QArgs -> a -> b) -> r
f $->  fn = f $ FieldResolver res
  where res args x ss = graphQLValueResolve ss $ fn args x

($->>) :: (GraphQLValue m b) => Resolve m a b r -> (QArgs -> a -> m b) -> r
f $->> fn = f $ FieldResolver res
  where res args x ss = resultM $ graphQLValueResolve ss <$> fn args x

($~>)  :: (GraphQLValue m b) => Resolve m a b r -> (a -> b) -> r
f $~>  fn = f $->  \_ -> fn

($~>>) :: (GraphQLValue m b) => Resolve m a b r -> (a -> m b) -> r
f $~>> fn = f $->> \_ -> fn

(*->)  :: (GraphQLValue m b) => Resolve m a b r -> (QArgs -> b) -> r
f *->  fn = f $->  const . fn

(*->>) :: (GraphQLValue m b) => Resolve m a b r -> (QArgs -> m b) -> r
f *->> fn = f $->> const . fn

(*~>)  :: (GraphQLValue m b) => Resolve m a b r -> b -> r
f *~>  fn = f *->  \_ -> fn

(*~>>) :: (GraphQLValue m b) => Resolve m a b r -> m b -> r
f *~>> fn = f *->> \_ -> fn

resultM :: (Monad m) => m (SubResult m a) -> SubResult m a
resultM m = SubResultM (m >>= liftInner)
  where liftInner (SubResult  x) = return x
        liftInner (SubResultM x) = x

-- field definition monad
-- THIS IS INCONSISTENT...
data FieldDefM m a b = FieldDefM
  { fdDesc    :: Description
  , fdDepr    :: Description
  , fdArgs    :: ArgumentsDef
  , fdVals    :: [QArgs -> a -> Validation]
  , fdMids    :: [QArgs -> a -> m Validation]
  , unwrap    :: b
  }

instance Functor (FieldDefM m a) where
  fmap f x = x { unwrap = f $ unwrap x }

instance Applicative (FieldDefM m a) where
  f <*> x = x
    { fdDesc = fdDesc f <> fdDesc x
    , fdDepr = fdDepr f <> fdDepr x
    , fdArgs = fdArgs f <> fdArgs x
    , fdVals = fdVals f <> fdVals x
    , fdMids = fdMids f <> fdMids x
    , unwrap = unwrap f $ unwrap x
    }
  pure x = FieldDefM
    { fdDesc    = ""
    , fdDepr    = ""
    , fdArgs    = []
    , fdVals    = []
    , fdMids    = []
    , unwrap    = x
    }

instance Monad (FieldDefM m a) where
  m >>= k = c { fdDesc = fdDesc m <> fdDesc c
              , fdDepr = fdDepr m <> fdDepr c
              , fdArgs = fdArgs m <> fdArgs c
              , fdVals = fdVals m <> fdVals c
              , fdMids = fdMids m <> fdMids c
              }
    where c = k $ unwrap m
  m >> k = k { fdDesc = fdDesc m <> fdDesc k
             , fdDepr = fdDepr m <> fdDepr k
             , fdArgs = fdArgs m <> fdArgs k
             , fdVals = fdVals m <> fdVals k
             , fdMids = fdMids m <> fdMids k
             }

instance Describable (FieldDefM m a) where
  describe d = (pure ()) { fdDesc = d }

instance Deprecatable (FieldDefM m a) where
  deprecate d = (pure ()) { fdDepr = d }

-- | ARGUMENTS
-- TODO: refactor this into its own thing so we can do mutations

-- arg builders
type ArgLens b = QArgs -> b
type Arg m a b = FieldDefM m a (ArgLens b)

getArg :: (GraphQLScalar b) => Name -> QArgs -> Maybe b
getArg n = deserialize      -- deserialize result
         . fromMaybe QEmpty -- create empty entry for nullable case
         . qArgsLookup n    -- try to get the value

arg :: forall m a b. (GraphQLValue m b, GraphQLScalar b) => Name -> FieldDefM m a (ArgLens b)
arg n = lens { fdArgs = [def], fdVals = [val] }
  where lens  = pure $ fromJust . getArg n
        def   = InputValueDef n "" t td Nothing
        td    = graphQLValueTypeDef (undefined :: m b)
        t     = graphQLValueTypeRef (undefined :: m b)
        val   = const
              . assertJust "Invalid argument"
              . (getArg n :: QArgs -> Maybe b)

-- assert that a value exists, otherwise throw an error
assertJust :: B.ByteString -> Maybe a -> Validation
assertJust _ (Just _) = OK
assertJust e Nothing  = ERR e

-- argument combinators
infixl 8 |=

(|=) :: (GraphQLValue m b, GraphQLScalar b)
     => FieldDefM m a (ArgLens (Maybe b)) -> b -> FieldDefM m a (ArgLens b)
a |= v = lens { fdArgs = [def] }
  where lens = withDefault v <$> a
        withDefault def f = fromMaybe def . f
        def = case head (fdArgs a) of
                   InputValueDef n d t td _ -> InputValueDef n d t td $ Just $ serialize v

infixl 7 @>

(@>) :: (GraphQLValue m b, GraphQLScalar b)
     => FieldDefM m a (ArgLens b) -> ArgDefM b u -> FieldDefM m a (ArgLens b)
a @> d = a { fdArgs = [def $ head (fdArgs a)]
           , fdVals = fdVals a <> [ const . f . unwrap a | f <- adVals d ]
           }
  where def (InputValueDef n desc t td def) = InputValueDef n (desc <> (adDesc d)) t td def

-- argument definition monad

validate :: (b -> Validation) -> ArgDefM b ()
validate v = ArgDefM { adDesc = "", adVals = [v] }

data Validation = OK | ERR B.ByteString deriving (Eq, Show)

data ArgDefM b a = ArgDefM
  { adDesc :: Description
  , adVals :: [b -> Validation]
  }

instance DefinitionBuilder (ArgDefM b) where
  unit = ArgDefM { adDesc = "", adVals = [] }
  merge x y = ArgDefM { adDesc = adDesc x <> adDesc y
                      , adVals = adVals x <> adVals y
                      }

instance Functor     (ArgDefM b) where fmap  = fmapDef
instance Applicative (ArgDefM b) where (<*>) = applyDef ; pure _ = unit
instance Monad       (ArgDefM b) where (>>=) = bindDef  ; (>>)   = seqDef

instance Describable (ArgDefM b) where
  describe d = ArgDefM { adDesc = d, adVals = [] }
