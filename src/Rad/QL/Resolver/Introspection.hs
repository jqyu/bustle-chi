{-# LANGUAGE DeriveGeneric,
             FlexibleInstances,
             LambdaCase,
             OverloadedStrings #-}

module Rad.QL.Resolver.Introspection where

import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as B
import           Data.Maybe              (fromJust, fromMaybe)
import           Data.Trie               (Trie)
import qualified Data.Trie               as Trie
import           Data.Typeable
import           GHC.Generics

import Rad.QL.AST
import Rad.QL.Resolver.Arguments
import Rad.QL.Resolver.Field
import Rad.QL.Resolver.Object
import Rad.QL.Resolver.Types

-- TODO: Directives
-- TODO: Deprecations

-- | __Schema

newtype SchemaRef = SchemaRef (Trie TypeDef) deriving (Eq, Show, Typeable)

extractSchema :: SchemaRef -> Trie TypeDef
extractSchema (SchemaRef x) = x

instance GraphQLNamed SchemaRef where
  docs _ = "__Schema"
         |.. "Representation of the GraphQL Schema"
instance GraphQLObject SchemaRef
instance GraphQLValue SchemaRef

schemaDef :: (Monad m) => ObjectDef m SchemaRef
schemaDef = defineObject
  [ "types"
    |.. "Needs Documentation"
    $~> map freeTypeRef
      . filter (visible . typeDefName)
      . Trie.elems
      . extractSchema

  , "queryType"
    |.. "Needs Documentation"
    $~> freeTypeRef
      . fromJust
      . Trie.lookup "RootQuery"
      . extractSchema

  , "mutationType"
    |.. "TODO"
    *~> (Nothing :: Maybe TypeRef)

  , "subscriptionType"
    |.. "TODO"
    *~> (Nothing :: Maybe TypeRef)

  -- DIRECTIVES ARE NOT CURRENTLY SUPPORTED
  , "directives"
    |.. "TODO"
    *~> ([] :: [TypeRef])

  ]

-- | __Type

newtype TypeRef = TypeRef (Type, TypeDef) deriving (Eq, Show, Typeable)

instance GraphQLNamed TypeRef where
  docs _ = "__Type"
         |.. "Representation of an arbitrary GraphQLType with some modifier"
instance GraphQLObject TypeRef
instance GraphQLValue TypeRef

applyDef :: (TypeDef -> a) -> (TypeRef -> a)
applyDef f (TypeRef (_, d)) = f d

schemaTypeDef :: (Monad m) => TypeDict m -> ObjectDef m TypeRef
schemaTypeDef td = defineObject

  [ "kind"
    |.. "Determines what kind of type this is"
    $~> typeKind

  , "name"
    $~> \case
      TypeRef (TypeNonNull _, _) -> Nothing
      TypeRef (TypeList    _, _) -> Nothing
      TypeRef (TypeNamed   _, t) -> Just $ typeDefName t

  , "description"
    $~> applyDef $ \case
      TypeDefInterface   (InterfaceTypeDef   _ d _  ) -> d
      TypeDefUnion       (UnionTypeDef       _ d _  ) -> d
      TypeDefObject      (ObjectTypeDef      _ d _ _) -> d
      TypeDefScalar      (ScalarTypeDef      _ d    ) -> d
      TypeDefEnum        (EnumTypeDef        _ d _  ) -> d
      TypeDefInputObject (InputObjectTypeDef _ d _  ) -> d

  , "fields"
    $-> \t a -> let _ = inclDepr a in
      case t of
           TypeRef (_, TypeDefObject (ObjectTypeDef _ _ _ fs)) ->
             Just [FieldRef f | f@(FieldDef n _ _ _ _) <- fs, visible n]
           TypeRef (_, TypeDefInterface (InterfaceTypeDef _ _ fs)) ->
             Just $ map FieldRef fs
           _ -> Nothing

  , "interfaces"
    $~> \case
      TypeRef (_, TypeDefObject (ObjectTypeDef _ _ ifs _)) ->
        Just $ [ TypeRef (TypeNamed $ NamedType n, TypeDefInterface i)
               | i@(InterfaceTypeDef n _ _) <- ifs
               ]
      _ -> Nothing

  , "possibleTypes"
    $~> \case
      -- TODO:
      TypeRef (_, TypeDefInterface i) ->
        Just [ TypeRef (TypeNamed $ NamedType n, TypeDefObject o)
             | (n, TypeDefObject o@(ObjectTypeDef _ _ ifs _)) <- Trie.toList $ typeDefs td
             , i `elem` ifs
             ]
      TypeRef (_, TypeDefUnion (UnionTypeDef _ _ ts)) ->
        Just $ map (freeTypeRef . TypeDefObject) ts
      _ -> Nothing

  , "enumValues"
    $-> \t a -> let _ = inclDepr a in
      case t of
           TypeRef (_, TypeDefEnum (EnumTypeDef _ _ vs)) ->
             Just $ map EnumValueRef vs
           _ -> Nothing

  , "inputFields"
    $~> \case
      TypeRef (_, TypeDefInputObject (InputObjectTypeDef _ _ vs)) ->
        Just $ map InputValueRef vs
      _ -> Nothing

  , "ofType"
    |.. "Decorated type for NON-NULL and LISTS"
    $~> \case
      TypeRef (TypeNonNull (NonNullTypeNamed n), t) ->
        Just $ TypeRef (TypeNamed n, t)
      TypeRef (TypeNonNull (NonNullTypeList  l), t) ->
        Just $ TypeRef (TypeList  l, t)
      TypeRef (TypeList (ListType l), t) ->
        Just $ TypeRef (l, t)
      _ -> Nothing
  ]

-- | __Field

newtype FieldRef = FieldRef FieldDef deriving (Eq, Show, Typeable)

instance GraphQLNamed FieldRef where
  docs _ = "__Field"
         |.. "A field in the schema"
instance GraphQLObject FieldRef
instance GraphQLValue FieldRef

schemaFieldDef :: (Monad m) => TypeDict m -> ObjectDef m FieldRef
schemaFieldDef td = defineObject

  [ "name"
    $~> \case
      FieldRef (FieldDef n _ _ _ _) -> n

  , "description"
    |.. "Field Description"
    $~> \case
      FieldRef (FieldDef _ d _ _ _) -> d

  , "args"
    |.. "Field arguments"
    $~> \case
      FieldRef (FieldDef _ _ as _ _) -> map InputValueRef as

  , "type"
    |.. "Types"
    $~> \case
      FieldRef (FieldDef _ _ _ t _) ->
        TypeRef (t, lookupTypeDef (typeDefs td) t)

  , "isDeprecated"
    |.. "TODO"
    *~> False

  , "deprecationReason"
    |.. "TODO"
    *~> (Nothing :: Maybe Description)
  ]

-- | __InputValue

newtype InputValueRef = InputValueRef InputValueDef deriving (Eq, Show, Typeable)

instance GraphQLNamed InputValueRef where
  docs _ = "__InputValue"
         |.. "Input Value for field and directive arguments"
instance GraphQLObject InputValueRef
instance GraphQLValue InputValueRef

schemaInputDef :: (Monad m) => ObjectDef m InputValueRef
schemaInputDef = defineObject

  [ "name"
    $~> \case
      InputValueRef (InputValueDef n _ _ _ _) -> n

  , "description"
    $~> \case
      InputValueRef (InputValueDef _ d _ _ _) -> d

  , "type"
    $~> \case
      InputValueRef (InputValueDef _ _ t d _) ->
        TypeRef (t, d)

  , "defaultValue"
    |.. "TODO"
    *~> (Nothing :: Maybe ByteString)
  ]

-- | __EnumValue

newtype EnumValueRef = EnumValueRef EnumValueDef deriving (Eq, Show, Typeable)

instance GraphQLNamed EnumValueRef where
  docs _ = "__EnumValue"
         |.. "Enum Value"
instance GraphQLObject EnumValueRef
instance GraphQLValue EnumValueRef

enumValueDef :: (Monad m) => ObjectDef m EnumValueRef
enumValueDef = defineObject

  [ "name"
    $~> \case
      EnumValueRef (EnumValueDef n _) -> n

  , "description"
    $~> \case
      EnumValueRef (EnumValueDef _ d) -> d

  -- TODO: deprecations
  , "isDeprecated"
    *~> False

  , "deprecationReason"
    *~> (Nothing :: Maybe Description)
  ]

-- UTILITIES:

visible :: Name -> Bool
visible n = B.take 2 n /= "__"

data InclDepr = InclDepr
  { includeDeprecated :: Maybe Bool
  } deriving (Eq, Show, Typeable, Generic)
instance GraphQLArgs InclDepr

inclDepr :: InclDepr -> Bool
inclDepr = fromMaybe False . includeDeprecated

data TypeKind = SCALAR
              | OBJECT
              | INTERFACE
              | UNION
              | ENUM
              | INPUT_OBJECT
              | LIST
              | NON_NULL
              deriving (Eq, Show, Generic)
instance GraphQLNamed TypeKind where
  docs _ = "__TypeKind"
         |.. "Representation of which type variant we're dealing with"
instance GraphQLEnum TypeKind
instance GraphQLScalar TypeKind
instance GraphQLValue TypeKind where
  typeDef = defineEnum
  resolve = resolveScalar

typeKind :: TypeRef -> TypeKind
typeKind (TypeRef (TypeList    _, _)) = LIST 
typeKind (TypeRef (TypeNonNull _, _)) = NON_NULL
typeKind (TypeRef (TypeNamed   _, t)) =
  case t of
       TypeDefInterface   _ -> INTERFACE
       TypeDefUnion       _ -> UNION
       TypeDefObject      _ -> OBJECT
       TypeDefScalar      _ -> SCALAR
       TypeDefEnum        _ -> ENUM
       TypeDefInputObject _ -> INPUT_OBJECT


data TypeNameArgs = TypeNameArgs
  { name :: ByteString
  } deriving (Eq, Show, Typeable, Generic)
instance GraphQLArgs TypeNameArgs

freeTypeRef :: TypeDef -> TypeRef
freeTypeRef td = TypeRef (TypeNamed $ NamedType $ typeDefName td, td)

lookupTypeName :: Trie TypeDef -> TypeNameArgs -> Maybe TypeRef
lookupTypeName tds a = freeTypeRef <$> Trie.lookup (name a) tds

lookupTypeDef :: Trie TypeDef -> Type -> TypeDef
lookupTypeDef tds (TypeNonNull (NonNullTypeNamed (NamedType n))) =
  fromJust $ Trie.lookup n tds
lookupTypeDef tds (TypeNamed (NamedType n)) =
  fromJust $ Trie.lookup n tds
lookupTypeDef tds (TypeNonNull (NonNullTypeList  (ListType t))) =
  lookupTypeDef tds t
lookupTypeDef tds (TypeList (ListType t)) =
  lookupTypeDef tds t
