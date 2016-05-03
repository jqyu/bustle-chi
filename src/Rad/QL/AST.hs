{-# LANGUAGE OverloadedStrings #-}
-- Full credit to https://github.com/jdnavarro/graphql-haskell
-- Only had to make some really minor tweaks

module Rad.QL.AST where

import Data.ByteString (ByteString)
import Data.String (IsString(fromString))

type Name        = ByteString
type Alias       = ByteString
type Description = ByteString

-- * Document

data Document = Document [Definition] deriving (Eq, Show)

data Definition = DefOperation OperationDef
                | DefFragment  FragmentDef
                deriving (Eq, Show)

data OperationDef = Query    Node
                  | Mutation Node
                  deriving (Eq, Show)

data Node = Node Name [VariableDef] [Directive] SelectionSet
          deriving (Eq, Show)

data VariableDef = VariableDef Variable Type (Maybe DefaultValue)
                 deriving (Eq, Show)

newtype Variable = Variable Name deriving (Eq, Show)

instance IsString Variable where
  fromString = Variable . fromString

-- * SelectionSet

type SelectionSet = [Selection]

data Selection = SelectionField Field
               | SelectionFragmentSpread FragmentSpread
               | SelectionInlineFragment InlineFragment
               deriving (Eq, Show)

data Field = Field Alias Name [Argument] [Directive] SelectionSet
           deriving (Show)

-- create an Ord instance to allow sorting and deduping
instance Eq Field where
  (Field a n _ _ _) == (Field a' n' _ _ _) = fKey a n == fKey a' n'
instance Ord Field where
  (Field a n _ _ _) <= (Field a' n' _ _ _) = fKey a n <= fKey a' n'

fKey :: ByteString -> ByteString -> ByteString
fKey "" n = n
fKey a  _ = a

data Argument = Argument Name Value deriving (Eq, Show)

-- * Fragment 

data FragmentSpread = FragmentSpread Name [Directive]
                    deriving (Eq, Show)

data InlineFragment = InlineFragment TypeCondition [Directive] SelectionSet
                    deriving (Eq, Show)

data FragmentDef = FragmentDef Name TypeCondition [Directive] SelectionSet
                 deriving (Eq, Show)

type TypeCondition = NamedType

-- * Values

data Value = ValueVariable Variable
           | ValueInt      Int
           | ValueFloat    Double
           | ValueBoolean  Bool
           | ValueString   ByteString
           | ValueEnum     Name
           | ValueList     ListValue
           | ValueObject   ObjectValue
           | NoValue       -- disgusting hack
           deriving (Eq, Show)

newtype ListValue = ListValue [Value] deriving (Eq, Show)

newtype ObjectValue = ObjectValue [ObjectField] deriving (Eq, Show)

data ObjectField = ObjectField Name Value deriving (Eq, Show)

type DefaultValue = Value

-- * Type References

data Type = TypeNamed   NamedType
          | TypeList    ListType
          | TypeNonNull NonNullType
          deriving (Eq, Show)

newtype NamedType = NamedType Name deriving (Eq, Show)

newtype ListType = ListType Type deriving (Eq, Show)

data NonNullType = NonNullTypeNamed NamedType
                 | NonNullTypeList  ListType
                 deriving (Eq, Show)

-- * Directives

data Directive = Directive Name [Argument] deriving (Eq, Show)

-- * Type Definitions

data TypeDef = TypeDefObject      ObjectTypeDef
             | TypeDefInterface   InterfaceTypeDef
             | TypeDefUnion       UnionTypeDef
             | TypeDefScalar      ScalarTypeDef
             | TypeDefEnum        EnumTypeDef
             | TypeDefInputObject InputObjectTypeDef
             deriving (Show)

-- because I don't really understand lenses yet
typeDefName :: TypeDef -> Name
typeDefName (TypeDefInterface   (InterfaceTypeDef   n _ _  )) = n
typeDefName (TypeDefUnion       (UnionTypeDef       n _ _  )) = n
typeDefName (TypeDefObject      (ObjectTypeDef      n _ _ _)) = n
typeDefName (TypeDefScalar      (ScalarTypeDef      n _    )) = n
typeDefName (TypeDefEnum        (EnumTypeDef        n _ _  )) = n
typeDefName (TypeDefInputObject (InputObjectTypeDef n _ _  )) = n

instance Eq TypeDef where
  t == t' = typeDefName t == typeDefName t'

data ObjectTypeDef = ObjectTypeDef Name Description Interfaces [FieldDef]
                   deriving (Eq, Show)

-- Same goes for interfaces
type Interfaces = [InterfaceTypeDef]

-- RadQL Field Defs store a TypeDef reference for resolving type directories
data FieldDef = FieldDef Name Description ArgumentsDef Type TypeDef
              deriving (Eq, Show)

type ArgumentsDef = [InputValueDef]

data InputValueDef = InputValueDef Name Description Type TypeDef (Maybe DefaultValue)
                   deriving (Eq, Show)

data InterfaceTypeDef = InterfaceTypeDef Name Description [FieldDef]
                      deriving (Eq, Show)

data UnionTypeDef = UnionTypeDef Name Description [ObjectTypeDef]
                  deriving (Eq, Show)

data ScalarTypeDef = ScalarTypeDef Name Description
                   deriving (Eq, Show)

data EnumTypeDef = EnumTypeDef Name Description [EnumValueDef]
                 deriving (Eq, Show)

data EnumValueDef = EnumValueDef Name Description
                  deriving (Eq, Show)

data InputObjectTypeDef = InputObjectTypeDef Name Description [InputValueDef]
                        deriving (Eq, Show)
