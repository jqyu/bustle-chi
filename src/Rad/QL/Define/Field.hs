{-# LANGUAGE ExistentialQuantification
           , ScopedTypeVariables #-}

module Rad.QL.Define.Field where

import           Control.Monad.Trans.Class
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

class HasFields d m a where
  fieldSingleton :: GraphQLFieldDef m a -> d m a ()

field :: forall d m a b. (HasFields d m a, GraphQLValue m b)
      => Name -> FieldDefM a m b -> d m a ()
field n fdef = fieldSingleton fdef'
  where fdef' = GraphQLFieldDef
          { fieldDef = def
          , fieldResolver = res
          }
        def = FieldDef n (fdDesc fdef)
                         (fdArgs fdef)
                         (graphQLValueTypeRef (undefined :: m b))
                         (graphQLValueTypeDef (undefined :: m b))
                         (fdDepr fdef)
        res :: QArgs -> a -> QSelectionSet -> Result m
        res args = resultM . fdFunc fdef args

resultM :: (Monad m, GraphQLValue m a) => FieldResult m a -> QSelectionSet -> Result m
resultM (Intercept x y) _ = SubResult (x, y)
resultM (Result    x  ) s = graphQLValueResolve s x
resultM (Cont      c  ) s = SubResultM (c >>= withErrs (graphQLValueResolve s))
  where withErrs fn (Left err) = return err
        withErrs fn (Right  x) = case fn x of
                                      SubResult  a -> return a
                                      SubResultM m -> m

-- special type name resolver
resolveTypeName :: (Monad m) => Name -> FieldRunner m a
resolveTypeName tn _ _ _ = pure $ buildString tn

-- filler token
resolve :: ()
resolve = ()

infixl 1 $->, $->>

($->) :: (Applicative m) => () -> (a -> b) -> FieldDefM a m b
_ $-> f = f <$> self

($->>) :: (Functor m) => () -> (a -> m b) -> FieldDefM a m b
_ $->> f = fieldDefM $ \_ x -> Cont $ Right <$> f x

-- this should be a monad transformer...

-- Field definition monad

data FieldResult m a = Intercept Builder [ B.ByteString ]
                     | Result    a
                     | Cont      (m (Either (Builder, [B.ByteString]) a))

interceptErr :: B.ByteString -> FieldResult m a
interceptErr err = Intercept buildNull [err]

instance (Functor m) => Functor (FieldResult m) where
  fmap f (Intercept x y) = Intercept x y
  fmap f (Result      x) = Result (f x)
  fmap f (Cont        c) = Cont $ fmap f <$> c

instance (Applicative m) => Applicative (FieldResult m) where
  pure = Result
  -- intercepts cancel
  (Intercept x y) <*> _ = Intercept x y
  _ <*> (Intercept x y) = Intercept x y
  -- field results are immediately resolved
  (Result f) <*> x = f <$> x
  f <*> (Result x) = ($ x) <$> f
  -- both sides blocked, cry yourself to sleep
  (Cont f) <*> (Cont x) = Cont $ applyConts <$> f <*> x
    where applyConts (Left k)  _        = Left k
          applyConts _         (Left  k) = Left k
          applyConts (Right f) (Right x) = Right (f x)

instance (Monad m) => Monad (FieldResult m) where
  (Intercept x y) >>= _ = Intercept x y
  (Result    x  ) >>= k = k x
  (Cont      x  ) >>= k = Cont $ ((fmap k <$> x) >>= unwrapCont)
    where unwrapCont (Left   err           ) = return $ Left err
          unwrapCont (Right (Intercept x y)) = return $ Left (x, y)
          unwrapCont (Right (Result    x  )) = return $ Right x
          unwrapCont (Right (Cont      m  )) = m

fieldErr :: B.ByteString -> FieldDefM a m b
fieldErr err = fieldDefM $ \_ _ -> interceptErr err

data FieldDefM a m b = FieldDefM
  { fdDesc :: Description
  , fdDepr :: Description
  , fdArgs :: ArgumentsDef
  , fdFunc :: QArgs -> a -> FieldResult m b
  }

infixl 5 <.>
(<.>) :: (Applicative m)
      => (a -> b -> FieldResult m (c -> d))
      -> (a -> b -> FieldResult m c)
      -> (a -> b -> FieldResult m d)
f1 <.> f2 = \x y -> f1 x y <*> f2 x y

instance (Functor m) => Functor (FieldDefM a m) where
  fmap f x = x { fdFunc = \y -> fmap f . fdFunc x y }

fieldDefM :: (QArgs -> a -> FieldResult m b) -> FieldDefM a m b
fieldDefM f = FieldDefM
  { fdDesc = ""
  , fdDepr = ""
  , fdArgs = []
  , fdFunc = f
  }

instance (Applicative m) => Applicative (FieldDefM a m) where
  f <*> x = x
    { fdDesc = fdDesc f <>  fdDesc x
    , fdDepr = fdDepr f <>  fdDepr x
    , fdArgs = fdArgs f <>  fdArgs x
    , fdFunc = fdFunc f <.> fdFunc x
    }
  pure x = fieldDefM $ \_ _ -> pure x

instance (Monad m) => Monad (FieldDefM a m) where
  m >>= k = m { fdFunc = f' }
    where f = fdFunc m
          f' x y = (k <$> f x y) >>= \k' -> fdFunc k' x y
  m >> k = k { fdDesc = fdDesc m <> fdDesc k
             , fdDepr = fdDepr m <> fdDepr k
             , fdArgs = fdArgs m <> fdArgs k
             }

instance MonadTrans (FieldDefM a) where
  lift m = fieldDefM $ \_ _ -> Cont $ Right <$> m

instance (Monad m) => Describable (FieldDefM a m) where
  describe d = (pure ()) { fdDesc = d }

instance (Monad m) => Deprecatable (FieldDefM a m) where
  deprecate d = (pure ()) { fdDepr = d }

data Validation = OK | ERR B.ByteString deriving (Eq, Show)

assert :: (Applicative m) => B.ByteString -> Bool -> FieldDefM a m ()
assert message cond | cond      = pure ()
                    | otherwise = fieldErr message

validate :: (Applicative m) => b -> (b -> Validation) -> FieldDefM a m ()
validate x f | ERR e <- f x = fieldErr e
             | otherwise    = pure ()

-- | ARGUMENTS
-- TODO: refactor this into its own thing so we can do mutations

-- arg builders
type Arg m a b = FieldDefM a m b

getArg :: (GraphQLScalar b) => Name -> QArgs -> Maybe b
getArg n = deserialize      -- deserialize result
         . fromMaybe QEmpty -- create empty entry for nullable case
         . qArgsLookup n    -- try to get the value

self :: (Applicative m) => FieldDefM a m a
self = fieldDefM $ \_ -> pure . id

arg :: forall m a b. (GraphQLValue m b, GraphQLScalar b) => Name -> FieldDefM a m b
arg n = getter { fdArgs = [def] }
  where getter = fieldDefM $ \args _ ->
                    maybe (interceptErr "Invalid argument")
                          pure
                        $ getArg n args
        def    = InputValueDef n "" t td Nothing
        td     = graphQLValueTypeDef (undefined :: m b)
        t      = graphQLValueTypeRef (undefined :: m b)

-- argument combinators
infixl 8 |=

(|=) :: (GraphQLValue m b, GraphQLScalar b)
     => FieldDefM a m (Maybe b) -> b -> FieldDefM a m b
a |= v = fromMaybe v <$> a

infixl 7 @>

(@>) :: (GraphQLValue m b, GraphQLScalar b)
     => FieldDefM a m b -> B.ByteString -> FieldDefM a m b
a @> d = a { fdArgs = [def $ head (fdArgs a)] }
  where def (InputValueDef n desc t td def) = InputValueDef n (desc <> d) t td def
