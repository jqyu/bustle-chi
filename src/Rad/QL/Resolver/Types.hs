{-# LANGUAGE DefaultSignatures,
             ExistentialQuantification,
             FlexibleContexts,
             FlexibleInstances,
             OverloadedStrings,
             ScopedTypeVariables,
             StandaloneDeriving,
             TypeOperators #-}

module Rad.QL.Resolver.Types where

import           Control.Applicative     ((<|>))
import           Control.Arrow           (first)
import qualified Data.Aeson              as JSON
import qualified Data.Aeson.Encode       as JSON
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as B
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8   as BC8
import           Data.Monoid             ((<>))
import           Data.String             (IsString(fromString))
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import           Data.Trie               (Trie)
import qualified Data.Trie               as Trie
import           Data.Typeable           (Typeable, cast)
import           Data.Word8              (Word8)
import           GHC.Generics

import Rad.QL.AST
import Rad.QL.Error

type SubResult m = CollectErrs m Builder

nullResult :: (Monad m) => SubResult m
nullResult = return (buildNull, [])

-- A resolver is a function which evaluates a SelectionSet
type Resolver m = SelectionSet -> SubResult m
-- A resolver' is a constructor for a resolver
type Resolver' m a = a -> Resolver m

-- A Doc is a construct for documenting a type
data Doc = Doc { docName :: Name
               , docDesc :: Description
               } deriving (Eq, Show)

instance IsString Doc where
  fromString s = Doc (fromString s) ""

-- description concatenation operator
-- TODO: create more of these for markup
-- i.e. |..* for lists
--      |..^ for headings

infixl 1 |..
(|..) :: Doc -> Description -> Doc
(Doc n d) |.. d' = Doc n (d <> d' <> "\n")

-- A type dictionary maintains all the information necessary for dynamic type resolution

data TypeDict m = (Monad m) => TypeDict
  { typeDefs           :: Trie TypeDef
  , objectResolvers    :: Trie (ObjectResolver m)
  }

-- A GraphQL Named Type is anything with a name.
-- This is used for inferring schema info
class (Typeable a, Eq a, Show a) => GraphQLNamed a where
  docs :: a -> Doc

instance (GraphQLNamed a) => GraphQLNamed [a] where
  docs _ = docs (undefined :: a)

instance (GraphQLNamed a) => GraphQLNamed (Maybe a) where
  docs _ = docs (undefined :: a)

typeName :: (GraphQLNamed a) => a -> Name
typeName = docName . docs

typeDesc :: (GraphQLNamed a) => a -> Description
typeDesc = docDesc . docs

-- A GraphQL value is anything that can be returned
-- This includes scalars, objects, interfaces, unions, etc.
class (GraphQLNamed a) => GraphQLValue a where
  -- the typeRef reflects the full AST representation of the type, used for introspection
  -- and validation
  typeRef :: a -> Type
  typeRef _ = TypeNonNull $ NonNullTypeNamed $ NamedType $ typeName (undefined :: a)

  -- the resolve function describes how to construct a Resolver' once the full type dictionary
  -- is retrieved
  resolve :: (Monad m) => TypeDict m -> Resolver' m a
  default resolve :: (GraphQLObject a, Monad m) => TypeDict m -> Resolver' m a
  resolve = resolveObject

  -- TODO: revisit this shitty cruft
  typeDef :: a -> TypeDef
  default typeDef :: (GraphQLObject a) => a -> TypeDef
  typeDef = TypeDefObject . objectDef

-- For any GraphQL Value, the Nullable and List versions are valid types
instance (GraphQLValue a) => GraphQLValue [a] where
  typeRef _ = TypeNonNull $ NonNullTypeList $ ListType $ typeRef (undefined :: a)
  typeDef _ = typeDef  (undefined :: a)
  resolve d = \xs ss ->
      first joinList <$> collectList [ resolve' x ss | x <- xs ]
    where resolve' = resolve d

instance (GraphQLValue a) => GraphQLValue (Maybe a) where
  typeRef _ = unwrapNonNull $ typeRef (undefined :: a)
  typeDef _ = typeDef  (undefined :: a)
  resolve d = \x ss -> case x of
                            Just x' -> resolve' x' ss
                            Nothing -> nullResult
    where resolve' = resolve d

unwrapNonNull :: Type -> Type
unwrapNonNull (TypeNonNull (NonNullTypeNamed t)) = TypeNamed t
unwrapNonNull (TypeNonNull (NonNullTypeList  t)) = TypeList  t
unwrapNonNull t = t -- If a type is already nullable, don't touch it


-- SCALARS
-- A scalar is any value which may resolve directly to an encoding
-- and can read from a GraphQL Literal

class (GraphQLNamed a) => GraphQLScalar a where
  toValue   :: a -> Builder
  default toValue :: (GraphQLEnum a) => a -> Builder
  toValue = enumToValue

  fromValue :: Value -> Maybe a
  default fromValue :: (GraphQLEnum a) => Value -> Maybe a
  fromValue (ValueEnum e) = enumFromValue e
  fromValue _             = Nothing

defineScalar :: forall a. (GraphQLScalar a) => a -> TypeDef
defineScalar = TypeDefScalar . scalarDef

scalarDef :: forall a. (GraphQLScalar a) => a -> ScalarTypeDef
scalarDef _ = ScalarTypeDef (typeName a) (typeDesc a)
  where a = undefined :: a

resolveScalar :: (GraphQLScalar a, Monad m) => TypeDict m -> Resolver' m a
resolveScalar _ x [] = return (toValue x, [])
resolveScalar _ _ _  = errMsg "Cannot apply subselection to a scalar"

instance (GraphQLScalar a) => GraphQLScalar (Maybe a) where
  toValue Nothing  = buildNull
  toValue (Just x) = toValue x
  fromValue = Just . fromValue

instance (GraphQLScalar a) => GraphQLScalar [a] where
  toValue vals = joinList [ toValue v | v <- vals ]
  fromValue (ValueList (ListValue vs)) = traverse fromValue vs
  fromValue _ = Nothing

-- Note that the mapping from Haskell scalars to GraphQL scalars is surjective, not injective
-- There is nothing wrong with having two types point to the same GraphQL scalar
-- All applications can infer the proper type from context

instance GraphQLNamed Int where
  docs _ = "Int"
instance GraphQLScalar Int where
  toValue                  = intDec
  fromValue (ValueInt int) = Just int
  fromValue _              = Nothing
instance GraphQLValue Int where
  typeDef = defineScalar
  resolve = resolveScalar

-- TODO: more instances, like Int16, Int32, etc.

instance GraphQLNamed Double where
  docs _ = "Float"
instance GraphQLScalar Double where
  toValue = doubleDec -- NOTE: currently slow, hopefully this will be fixed upstream
  fromValue (ValueInt   int  ) = Just $ fromIntegral int
  fromValue (ValueFloat float) = Just float
  fromValue _                  = Nothing
instance GraphQLValue Double where
  typeDef = defineScalar
  resolve = resolveScalar

-- TODO: more instances, like Fractional, Float, etc.

instance GraphQLNamed ByteString where
  docs _ = "String"
instance GraphQLScalar ByteString where
  toValue = buildString
  fromValue (ValueInt     int   ) = Just $ BC8.pack $ show int
  fromValue (ValueFloat   float ) = Just $ BC8.pack $ show float
  fromValue (ValueBoolean bool  ) = Just $ BC8.pack $ show bool
  fromValue (ValueString  string) = Just string
  fromValue _                     = Nothing
instance GraphQLValue ByteString where
  typeDef = defineScalar
  resolve = resolveScalar

instance GraphQLNamed Text where
  docs _ = "String"
instance GraphQLScalar Text where
  toValue = buildString . TE.encodeUtf8
  fromValue (ValueInt     int   ) = Just $ T.pack $ show int
  fromValue (ValueFloat   float ) = Just $ T.pack $ show float
  fromValue (ValueBoolean bool  ) = Just $ T.pack $ show bool
  fromValue (ValueString  string) = Just $ TE.decodeUtf8 string
  fromValue _                     = Nothing
instance GraphQLValue Text where
  typeDef = defineScalar
  resolve = resolveScalar

-- String implementations are a pain in the ass
-- really you should be using bytestrings anyways

instance GraphQLNamed Bool where
  docs _ = "Boolean"
instance GraphQLScalar Bool where
  toValue True  = byteString "true"
  toValue False = byteString "false"
  fromValue (ValueBoolean bool) = Just bool
  fromValue _                   = Nothing
instance GraphQLValue Bool where
  typeDef = defineScalar
  resolve = resolveScalar

instance GraphQLNamed JSON.Value where
  docs _ = "JSON"
         |.. "JSON object that's processed at runtime"
         |.. "For a more efficient implementation, use RawJSON"
instance GraphQLScalar JSON.Value where
  toValue = JSON.encodeToBuilder
  fromValue (ValueString string) = JSON.decodeStrict string
  fromValue _ = Nothing
instance GraphQLValue JSON.Value where
  typeDef = defineScalar
  resolve = resolveScalar

-- OBJECTS
-- An object is any value which contains a resolver in the type dictionary

data InterfaceImpl a
  = forall b. (GraphQLInterface b) => InterfaceImpl InterfaceTypeDef (a -> b)

implements :: forall a b. (GraphQLObject a, GraphQLInterface b) => (a -> b) -> InterfaceImpl a
implements f = InterfaceImpl def f
  where def = interfaceDef (undefined :: b)

type ObjectDef m a = (ObjectTypeDef, TypeDict m -> Resolver' m a)

class (GraphQLNamed a) => GraphQLObject a where
  objectInterfaces :: a -> [InterfaceImpl a]
  objectInterfaces _ = []
  objectDefinition :: a -> ObjectDef m a
  objectDefinition _ = (ObjectTypeDef (typeName a) (typeDesc a) ifs [], undefined)
    where a   = undefined :: a
          ifs = [ d | InterfaceImpl d _ <- objectInterfaces a ]

objectDef :: forall a. (GraphQLObject a) => a -> ObjectTypeDef
objectDef _ = fst $ objectDefinition (undefined :: a)

-- An object resolver contains information on how to 
data ObjectResolver m = forall a. (GraphQLObject a, Monad m) => ObjectResolver (Resolver' m a)

resolveObject :: forall m a. (GraphQLObject a, Monad m) => TypeDict m -> a -> Resolver m
resolveObject td = maybe err applyRes ores
  where err _ _ = errMsg "Could not find resolver for type"
        ores    = Trie.lookup (typeName (undefined :: a)) (objectResolvers td)
        applyRes (ObjectResolver r) x = maybe (\_ -> errMsg "Runtype Type Mismatch") r (cast x)

-- ENUMS
-- An enum is an ADT with a fixed set of unitary constructors
-- e.g. data Publication = BUSTLE | ROMPER | APP deriving (Eq, Show, Typeable, Generic)

class (GraphQLNamed a) => GraphQLEnum a where
  enumValues :: a -> [EnumValueDef]
  default enumValues :: (Generic a, GEnum (Rep a)) => a -> [EnumValueDef]
  enumValues _ = gEnumValues $ from (undefined :: a)

  enumToValue :: a -> Builder
  default enumToValue :: (Generic a, GEnum (Rep a)) => a -> Builder
  enumToValue = gEnumToValue . from

  enumFromValue :: Name -> Maybe a
  default enumFromValue :: (Generic a, GEnum (Rep a)) => Name -> Maybe a
  enumFromValue = fmap to . gEnumFromValue

defineEnum :: forall a. (GraphQLEnum a) => a -> TypeDef
defineEnum = TypeDefEnum . enumDef

enumDef :: forall a. (GraphQLEnum a) => a -> EnumTypeDef
enumDef _ = EnumTypeDef (typeName a) (typeDesc a) (enumValues a)
  where a = undefined :: a

class GEnum f where
  gEnumValues :: f a -> [EnumValueDef]
  gEnumToValue :: f a -> Builder
  gEnumFromValue :: Name -> Maybe (f a)

-- delegates data type instances upwards
instance (GEnum a, Datatype c) => GEnum (M1 D c a) where
  gEnumValues _ = gEnumValues (undefined :: a b)
  gEnumToValue (M1 x) = gEnumToValue x
  gEnumFromValue n = M1 <$> gEnumFromValue n

instance (GEnum a, GEnum b) => GEnum (a :+: b) where
  gEnumValues _ = gEnumValues (undefined :: a x)
               <> gEnumValues (undefined :: b x)
  gEnumToValue (L1 x) = gEnumToValue x
  gEnumToValue (R1 x) = gEnumToValue x
  gEnumFromValue s = L1 <$> gEnumFromValue s
                 <|> R1 <$> gEnumFromValue s

instance (Constructor c) => GEnum (M1 C c U1) where
  gEnumValues    _ = [EnumValueDef (BC8.pack $ conName (undefined :: M1 C c U1 y)) ""]
  gEnumToValue   _ = buildString $ BC8.pack $ conName (undefined :: M1 C c U1 y)
  gEnumFromValue s = if s == (BC8.pack $ conName (undefined :: M1 C c U1 y))
                        then Just $ M1 U1
                        else Nothing

-- TODO: explore options for default implementations to SYB

class (GraphQLNamed a) => GraphQLInterface a where
  interfaceFields :: a -> [FieldDef]

defineInterface :: forall a. (GraphQLInterface a) => a -> TypeDef
defineInterface = TypeDefInterface . interfaceDef

interfaceDef :: forall a. (GraphQLInterface a) => a -> InterfaceTypeDef
interfaceDef _ = InterfaceTypeDef (typeName a) (typeDesc a) (interfaceFields a)
  where a = undefined :: a

class (GraphQLNamed a) => GraphQLUnion a where
  unionTypes :: a -> [ObjectTypeDef]

defineUnion :: forall a. (GraphQLUnion a) => a -> TypeDef
defineUnion = TypeDefUnion . unionDef
unionDef :: forall a. (GraphQLUnion a) => a -> UnionTypeDef
unionDef _ = UnionTypeDef (typeName a) (typeDesc a) (unionTypes a)
  where a = undefined :: a

-- encoding utilities:

buildNull :: Builder
buildNull = byteString "null"

buildString :: ByteString -> Builder
buildString x = charUtf8 '"' <> B.foldl escape mempty x <> charUtf8 '"'
  -- TODO: check for more rescapable character
  where escape :: Builder -> Word8 -> Builder
        escape b c | c == 0x22 = b <> charUtf8 '\\' <> charUtf8 '"'
                   | c == 0x5c = b <> charUtf8 '\\' <> charUtf8 '\\'
                   | c == 0x0d = b <> charUtf8 '\\' <> charUtf8 'r'
                   | c == 0x0a = b <> charUtf8 '\\' <> charUtf8 'n'
                   | otherwise = b <>                 word8 c

joinObject :: [ Builder ] -> Builder
joinObject vals = charUtf8 '{' <> joinComma vals <> charUtf8 '}'

joinList :: [ Builder ] -> Builder
joinList vals = charUtf8 '[' <> joinComma vals <> charUtf8 ']'

joinComma :: [ Builder ] -> Builder
joinComma = joinBuilders $ charUtf8 ','

joinBuilders :: Builder -> [ Builder ] -> Builder
joinBuilders _    []  = mempty
joinBuilders j (x:xs) = x <> mconcat [ j <> x' | x' <- xs ]
