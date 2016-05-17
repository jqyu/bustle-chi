{-# LANGUAGE DefaultSignatures
           , EmptyDataDecls
           , FunctionalDependencies
           , ScopedTypeVariables #-}

module Rad.QL.Types

  ( OBJECT
  , INTERFACE
  , UNION
  , SCALAR
  , ENUM

  , GraphQLScalar(..)
  , GraphQLType(..)
  , GraphQLValue(..)

  -- utilities
  , resolveScalar

  ) where

import qualified Data.Aeson              as JSON
import qualified Data.Aeson.Encode       as JSON
import qualified Data.ByteString         as B
import qualified Data.ByteString.Char8   as BC8
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import qualified Data.Trie               as Trie
import           GHC.Generics

import Rad.QL.Internal.Builders
import Rad.QL.Internal.GEnum
import Rad.QL.Internal.Types

import Rad.QL.AST
import Rad.QL.Query

-- | GraphQL Kinds
-- Internal GraphQL kinds, note that we assume non-null by default
data OBJECT
data SCALAR
data ENUM
data INTERFACE
data UNION
data INPUT_OBJECT
data LIST_OF
data NULLABLE

-- | GraphQLType
-- A GraphQLType says that a type can be resolved against a given monad

class (Monad m) => GraphQLType kind m a | a -> kind where
  def :: GraphQLTypeDef kind m a

-- | GraphQL Scalar

-- A GraphQLScalar can be read off of an input value, and serialized directly to a result
class GraphQLScalar a where
  serialize   :: a      -> Builder
  deserialize :: QValue -> Maybe a

  default serialize   :: (IsEnum a) => a      -> Builder
  serialize   = enumSerialize
  default deserialize :: (IsEnum a) => QValue -> Maybe a
  deserialize = enumDeserialize

instance (GraphQLScalar a) => GraphQLScalar (Maybe a) where
  serialize Nothing = buildNull
  serialize (Just x) = serialize x
  deserialize = Just . deserialize

instance (GraphQLScalar a) => GraphQLScalar [a] where
  serialize vs = joinList [ serialize v | v <- vs ]
  deserialize (QList vs) = traverse deserialize vs
  deserialize _          = Nothing

-- Built-in Scalar instances

resolveScalar :: (GraphQLScalar a, Monad m) => QSelectionSet -> a -> Result m
resolveScalar [] = pure . serialize
resolveScalar _  = \_ -> errorMsg "Scalar cannot take a subselection"

defineScalar :: (Monad m, GraphQLScalar a) => Name -> Description -> GraphQLTypeDef SCALAR m a
defineScalar n d = emptyDef { gqlTypeDef = td, gqlResolve = resolveScalar }
  where td = TypeDefScalar $ ScalarTypeDef n d

instance GraphQLScalar Int where
  serialize            = intDec
  deserialize (QInt v) = Just v
  deserialize _        = Nothing
instance (Monad m) => GraphQLValue m Int
instance (Monad m) => GraphQLType SCALAR m Int where
  def = defineScalar "Int" $
    "TODO: copy and paste description"

-- TODO: other Integral types, Int16, Int32, etc.

instance GraphQLScalar Double where
  serialize                  = doubleDec -- NOTE: currently slow, hopefully this will be fixed upstream
  deserialize (QInt   v) = Just $ fromIntegral v
  deserialize (QFloat v) = Just v
  deserialize _              = Nothing
instance (Monad m) => GraphQLValue m Double
instance (Monad m) => GraphQLType SCALAR m Double where
  def = defineScalar "Double" $
    "TODO: copy and paste description"

-- TODO: more fractionals, e.g. Float, Real...

instance GraphQLScalar B.ByteString where
  serialize = buildString
  deserialize (QInt    v) = Just $ BC8.pack $ show v
  deserialize (QFloat  v) = Just $ BC8.pack $ show v
  deserialize (QBool   v) = Just $ BC8.pack $ show v
  deserialize (QString v) = Just v
  deserialize _                = Nothing
instance (Monad m) => GraphQLValue m B.ByteString
instance (Monad m) => GraphQLType SCALAR m B.ByteString where
  def = defineScalar "String" $
    "TODO: copy and paste description"

instance GraphQLScalar T.Text where
  serialize                    = buildString . TE.encodeUtf8
  deserialize (QInt    v) = Just $ T.pack $ show v
  deserialize (QFloat  v) = Just $ T.pack $ show v
  deserialize (QBool   v) = Just $ T.pack $ show v
  deserialize (QString v) = Just $ TE.decodeUtf8 v
  deserialize _                = Nothing
instance (Monad m) => GraphQLValue m T.Text
instance (Monad m) => GraphQLType SCALAR m T.Text where
  def = defineScalar "String" $
    "TODO: copy and paste description"

-- convenience instance for schema query
instance GraphQLScalar Builder where
  serialize = id
  deserialize _ = Nothing
instance (Monad m) => GraphQLValue m Builder
instance (Monad m) => GraphQLType SCALAR m Builder where
  def = defineScalar "String" $
    "TODO: copy and paste description"

instance GraphQLScalar Bool where
  serialize True               = byteString "true"
  serialize False              = byteString "false"
  deserialize (QBool v) = Just v
  deserialize _                = Nothing
instance (Monad m) => GraphQLValue m Bool
instance (Monad m) => GraphQLType SCALAR m Bool where
  def = defineScalar "Boolean" $
    "TODO: copy and paste description"

instance GraphQLScalar JSON.Value where
  serialize                   = JSON.encodeToBuilder
  deserialize (QString v) = JSON.decodeStrict v
  deserialize _               = Nothing
instance (Monad m) => GraphQLValue m JSON.Value
instance (Monad m) => GraphQLType SCALAR m JSON.Value where
  def = defineScalar "JSON" $
    "TODO: write a description"

-- A GraphQLValue denotes some value which can be resolve,
--   i.e. some type instance, its nullable (Maybe a), or list ([a])

class (Monad m) => GraphQLValue m a where
  graphQLValueTypeDef :: m a -> TypeDef
  graphQLValueTypeRef :: m a -> Type
  graphQLValueResolve :: QSelectionSet -> a -> Result m

  default graphQLValueTypeDef :: (GraphQLType kind m a) => m a -> TypeDef
  graphQLValueTypeDef = graphQLValueTypeDef'

  default graphQLValueTypeRef :: (GraphQLType kind m a) => m a -> Type
  graphQLValueTypeRef = graphQLValueTypeRef'

  default graphQLValueResolve :: (GraphQLType kind m a) => QSelectionSet -> a -> Result m
  graphQLValueResolve = graphQLValueResolve'

graphQLValueTypeDef' :: forall m a kind. (GraphQLType kind m a) => m a -> TypeDef
graphQLValueTypeDef' _ = gqlTypeDef (def :: GraphQLTypeDef kind m a)

graphQLValueTypeRef' :: forall m a kind. (GraphQLType kind m a) => m a -> Type
graphQLValueTypeRef' _ = TypeNonNull
                       $ NonNullTypeNamed
                       $ NamedType
                       $ typeDefName
                       $ gqlTypeDef (def :: GraphQLTypeDef kind m a)

graphQLValueResolve' :: forall m a kind. (GraphQLType kind m a) => QSelectionSet -> a -> Result m
graphQLValueResolve' = gqlResolve (def :: GraphQLTypeDef kind m a)


instance (GraphQLValue m a) => GraphQLValue m [a] where
  graphQLValueTypeDef _ = graphQLValueTypeDef (undefined :: m a)
  graphQLValueTypeRef _ = TypeList
                        $ ListType
                        $ graphQLValueTypeRef (undefined :: m a)
  graphQLValueResolve s = collectResults
                        . map (graphQLValueResolve s)

instance (GraphQLValue m a) => GraphQLValue m (Maybe a) where
  graphQLValueTypeDef _ = graphQLValueTypeDef (undefined :: m a)
  graphQLValueTypeRef _ = unwrapNonNull $ graphQLValueTypeRef (undefined :: m a)
    where unwrapNonNull (TypeNonNull (NonNullTypeList  l)) = TypeList  l
          unwrapNonNull (TypeNonNull (NonNullTypeNamed n)) = TypeNamed n
          unwrapNonNull t                                  = t
  graphQLValueResolve _  Nothing  = nullResult
  graphQLValueResolve ss (Just x) = graphQLValueResolve ss x

-- GENERIC ENUM DERIVING MAGIC
