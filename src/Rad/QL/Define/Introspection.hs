{-# LANGUAGE LambdaCase
           , NamedFieldPuns
           , ScopedTypeVariables #-}

module Rad.QL.Define.Introspection where

import qualified Data.ByteString as B
import           Data.Monoid     ((<>))
import qualified Data.Trie       as Trie
import           GHC.Generics

import Rad.QL.Internal.Types

import Rad.QL.AST
import Rad.QL.Types hiding (SCALAR, OBJECT, INTERFACE, UNION, ENUM, INPUT_OBJECT, LIST)
import qualified Rad.QL.Types as R
import Rad.QL.Query

import Rad.QL.Define.Enum
import Rad.QL.Define.Field
import Rad.QL.Define.Object
import Rad.QL.Define.Util

-- | Schema Introspection Mixin

introspection :: forall m a b. (Monad m) => Schema m -> ObjectDefM m a b
introspection schema = let dict = typeDict schema in do

  field "__schema" $ do
    describe "Schema introspection object"
    resolve *~> schema

  field "__type" $ do
    describe "Retrieves a type definition by name"
    name <- arg "name"
    resolve *-> \args ->
      freeType__ dict <$> Trie.lookup (name args) dict

-- | __Schema

instance (Monad m) => GraphQLValue m (Schema m)
instance (Monad m) => GraphQLType R.OBJECT m (Schema m) where

  def = defineObject "__Schema" $ do

    describe "A schema introspection object"

    field "types" $ do
      describe "A list of all types supported by the current schema"
      resolve $~> \schema ->
        let dict = typeDict schema in
          [ freeType__ dict t
          | t <- Trie.elems dict
          , B.take 2 (typeDefName t) /= "__"
          ]

    field "queryType" $ do
      describe "reference to the root query type"
      resolve $~> \schema ->
        freeType__ (typeDict schema) (rootQueryType schema)

    field "mutationType" $ do
      describe "reference to the root mutation type"
      resolve *~> (Nothing :: Maybe Type__)

    field "directives" $ do
      describe "directives supported by the executor"
      resolve $~> \schema ->
        [ includeDirective $ typeDict schema
        , skipDirective    $ typeDict schema
        ]

-- | __Type

data Type__ = Type__ { type__Ref  :: Type
                     , type__Def  :: TypeDef
                     , type__Dict :: TypeDict
                     }

freeType__ :: TypeDict -> TypeDef -> Type__
freeType__ dict t = Type__ (TypeNamed $ NamedType $ typeDefName t) t dict

instance (Monad m) => GraphQLValue m Type__
instance (Monad m) => GraphQLType R.OBJECT m Type__ where

   -- type __Type {
  def = defineObject "__Type" $ do

    describe "A type introspection object"

    -- kind: __TypeKind!
    field "kind" $ resolve $~> \case
      Type__   (TypeList    _)     _  _ -> LIST
      Type__   (TypeNonNull _)     _  _ -> NON_NULL
      Type__ _ (TypeDefObject      _) _ -> OBJECT
      Type__ _ (TypeDefScalar      _) _ -> SCALAR
      Type__ _ (TypeDefInterface   _) _ -> INTERFACE
      Type__ _ (TypeDefUnion       _) _ -> UNION
      Type__ _ (TypeDefEnum        _) _ -> ENUM
      Type__ _ (TypeDefInputObject _) _ -> INPUT_OBJECT

    -- name: String!
    field "name"        $ resolve $~> typeDefName . type__Def

    -- description: String!
    field "description" $ resolve $~> typeDefDesc . type__Def

    -- fields(includeDeprecated: Boolean = false): [__Field!]
    field "fields" $ do
      describe "OBJECT and INTERFACE only"
      inclDepr <- arg "includeDeprecated" |= False @>
        describe
        $.. "Flag that determines whether or not to include deprecated fields."
        |.. "Defaults to false."
      resolve $-> \args v -> case v of
        Type__ _ (TypeDefObject (ObjectTypeDef _ _ _ fs)) dict
          | inclDepr args -> Just [Field__ f dict | f <- fs]
          | otherwise     -> Just [Field__ f dict | f@(FieldDef _ _ _ _ _ "") <- fs]
        Type__ _ (TypeDefInterface (InterfaceTypeDef _ _ fs)) dict
          | inclDepr args -> Just [Field__ f dict | f <- fs]
          | otherwise     -> Just [Field__ f dict | f@(FieldDef _ _ _ _ _ "") <- fs]
        _                 -> Nothing

    -- interfaces: [__Type!]
    field "interfaces" $ do
      describe "OBJECT only"
      resolve $~> \case
        Type__ _ (TypeDefObject (ObjectTypeDef _ _ ifs _)) dict ->
          Just [freeType__ dict (TypeDefInterface i) | i <- ifs]
        _ -> Nothing

    -- possibleTypes: [__Type!]
    field "possibleTypes" $ do
      describe "INTERFACE and UNION only"
      resolve $~> \case
        Type__ _ (TypeDefInterface i@(InterfaceTypeDef n _ _)) dict ->
          Just [ freeType__ dict (TypeDefObject o)
               | TypeDefObject o@(ObjectTypeDef _ _ ifs _) <- Trie.elems dict
               , i `elem` ifs
               ]
        Type__ _ (TypeDefUnion (UnionTypeDef _ _ odefs)) dict ->
          Just [ freeType__ dict (TypeDefObject o) | o <- odefs ]
        _ -> Nothing

    -- enumValues(includeDeprecated: Boolean = false): [__EnumValue!]
    field "enumValues" $ do
      describe "ENUM only"
      inclDepr <- arg "includeDeprecated" |= False @>
        describe
        $.. "Flag that determines whether or not to include deprecated enum values"
        |.. "Defaults to false."
      resolve $-> \args v -> case v of
        Type__ _ (TypeDefEnum (EnumTypeDef _ _ vals)) _ 
          | inclDepr args -> Just [EnumValue__ val | val <- vals]
          | otherwise     -> Just [EnumValue__ val | val@(EnumValueDef _ _ "") <- vals]
        _ -> Nothing

    -- inputFields: [__InputValue!]
    field "inputFields" $ do
      describe "INPUT_OBJECT only"
      resolve $~> \case
        Type__ _ (TypeDefInputObject (InputObjectTypeDef _ _ vals)) dict ->
          Just [InputValue__ val dict | val <- vals]
        _ -> Nothing

    -- ofType: __Type
    field "ofType" $ do
      describe "NON_NULL and LIST only"
      resolve $~> \case
        Type__ (TypeList    (ListType         t)) tdef dict -> Just $ Type__ t             tdef dict
        Type__ (TypeNonNull (NonNullTypeList  l)) tdef dict -> Just $ Type__ (TypeList  l) tdef dict
        Type__ (TypeNonNull (NonNullTypeNamed n)) tdef dict -> Just $ Type__ (TypeNamed n) tdef dict
        _ -> Nothing

-- | __Field

data Field__ = Field__ FieldDef TypeDict

instance (Monad m) => GraphQLValue m Field__
instance (Monad m) => GraphQLType R.OBJECT m Field__ where

  -- type __Field {
  def = defineObject "__Field" $ do

    describe "A field introspection object"
    
    -- name: String!
    field "name" $ resolve $~>
      \(Field__ (FieldDef n _ _ _ _ _) _) -> n

    -- description: String
    field "description" $ resolve $~>
      \(Field__ (FieldDef _ d _ _ _ _) _) -> descMaybe d

    -- args: [__InputValue!]!
    field "args" $ resolve $~>
      \(Field__ (FieldDef _ _ args _ _ _) dict) ->
        [ InputValue__ a dict | a <- args ]

    -- type: __Type!
    field "type" $ resolve $~>
      \(Field__ (FieldDef _ _ _ t td _) dict) -> Type__ t td dict

    -- isDeprecated: Boolean!
    field "isDeprecated" $ resolve $~>
      \(Field__ (FieldDef _ _ _ _ _ d) _) -> d /= ""

    -- deprecationReason: String
    field "deprecationReason" $ resolve $~>
      \(Field__ (FieldDef _ _ _ _ _ d) _) -> descMaybe d

-- | __InputValue

data InputValue__ = InputValue__ InputValueDef TypeDict

instance (Monad m) => GraphQLValue m InputValue__
instance (Monad m) => GraphQLType R.OBJECT m InputValue__ where

  -- type __InputValue {
  def = defineObject "__InputValue" $ do

    describe "An input value introspection object"

    -- name: String!
    field "name" $ resolve $~>
      \(InputValue__ (InputValueDef n _ _ _ _) _) -> n

    -- description: String
    field "description" $ resolve $~>
      \(InputValue__ (InputValueDef _ d _ _ _) _) -> descMaybe d

    -- type: __Type!
    field "type" $ resolve $~>
      \(InputValue__ (InputValueDef _ _ t td _) dict) -> Type__ t td dict

    -- defaultValue: String
    field "defaultValue" $ resolve $~>
      \(InputValue__ (InputValueDef _ _ _ _ d) _) -> d

-- | __EnumValue

newtype EnumValue__ = EnumValue__ EnumValueDef

instance (Monad m) => GraphQLValue m EnumValue__
instance (Monad m) => GraphQLType R.OBJECT m EnumValue__ where

  -- type __EnumValue {
  def = defineObject "__EnumValue" $ do

    describe "Introspection object for possible type of an ENUM"

    -- name: String!
    field "name" $ resolve $~>
      \(EnumValue__ (EnumValueDef n _ _)) -> n

    -- description: String
    field "description" $ resolve $~>
      \(EnumValue__ (EnumValueDef _ d _)) -> descMaybe d

    -- isDeprecated: Boolean!
    field "isDeprecated" $ resolve $~>
      \(EnumValue__ (EnumValueDef _ _ d)) -> d /= ""

    -- deprecationReason: String
    field "deprecationReason" $ resolve $~>
      \(EnumValue__ (EnumValueDef _ _ d)) -> descMaybe d

-- | __TypeKind
data TypeKind__
  = SCALAR
  | OBJECT
  | INTERFACE
  | UNION
  | ENUM
  | INPUT_OBJECT
  | LIST
  | NON_NULL
  deriving (Eq, Show, Generic)

instance GraphQLScalar TypeKind__
instance (Monad m) => GraphQLValue m TypeKind__
instance (Monad m) => GraphQLType R.ENUM m TypeKind__ where

  def = defineEnum "__TypeKind" $
    describe "Enum representing type kind"

-- | __Directive

data Directive__ = Directive__
  { directive__Name        :: Name
  , directive__Description :: Description
  , directive__Locations   :: [DirectiveLocation__]
  , directive__Args        :: [InputValueDef]
  , directive__dict        :: TypeDict
  }

instance (Monad m) => GraphQLValue m Directive__
instance (Monad m) => GraphQLType R.OBJECT m Directive__ where

  def = defineObject "__Directive" $ do

    describe "A directive provides a way to describe.."

    field "name"        $ resolve $~> directive__Name
    field "description" $ resolve $~> descMaybe . directive__Description
    field "locations"   $ resolve $~> directive__Locations
    field "args"        $ resolve $~> \v->
      [ InputValue__ val (directive__dict v) | val <- directive__Args v ]
    field "onOperation"
      $ resolve $~> elems [ QUERY, MUTATION ]
                  . directive__Locations
    field "onFragment"
      $ resolve $~> elems [ FRAGMENT_SPREAD, INLINE_FRAGMENT ]
                  . directive__Locations
    field "onField"
      $ resolve $~> elem FIELD
                  . directive__Locations

includeDirective :: TypeDict -> Directive__
includeDirective dict = Directive__
  { directive__Name        = "if"
  , directive__Description = "Tells the executor to include a field or fragment if true"
  , directive__Locations   = [FIELD, FRAGMENT_SPREAD, INLINE_FRAGMENT]
  , directive__Args        = [ InputValueDef
                                "if"
                                ""
                                (TypeNamed $ NamedType "Boolean")
                                (gqlTypeDef (def :: GraphQLTypeDef R.SCALAR Maybe Bool))
                                Nothing
                             ]
  , directive__dict        = dict
  }

skipDirective :: TypeDict -> Directive__
skipDirective dict = (includeDirective dict)
  { directive__Name        = "skip"
  , directive__Description = "Tells the executor to skip a field if fragment is true"
  }

-- | __DirectiveLocation
data DirectiveLocation__
  = QUERY
  | MUTATION
  | FIELD
  | FRAGMENT_DEFINITION
  | FRAGMENT_SPREAD
  | INLINE_FRAGMENT
  deriving (Eq, Show, Generic)

instance GraphQLScalar DirectiveLocation__
instance (Monad m) => GraphQLValue m DirectiveLocation__
instance (Monad m) => GraphQLType R.ENUM m DirectiveLocation__ where

  def = defineEnum "__DirectiveLocation" $
    describe "A place where you can put a directive"

-- | Helpers
descMaybe :: Description -> Maybe Description
descMaybe "" = Nothing
descMaybe d  = Just d

elems :: (Eq a) => [a] -> [a] -> Bool
elems xs ys = foldl (\acc x -> acc && x `elem` ys) True xs
