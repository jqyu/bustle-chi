{-# LANGUAGE DefaultSignatures,
             FlexibleContexts,
             FlexibleInstances,
             MultiParamTypeClasses,
             OverloadedStrings,
             ScopedTypeVariables,
             TypeOperators #-}

module Rad.QL.Resolver.Object where

import           Control.Arrow           (first)
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8   as BC8
import           Data.Foldable           (foldl')
import qualified Data.List               as List
import           Data.Monoid             ((<>))
import qualified Data.Set                as Set
import           Data.Trie               (Trie)
import qualified Data.Trie               as Trie
import           GHC.Generics

import Rad.QL.AST
import Rad.QL.Error
import Rad.QL.Resolver.Field
import Rad.QL.Resolver.Types

-- define an object type

-- type ObjectDef m a = (ObjectTypeDef, TypeDict m -> Resolver' m a)

defineObject :: forall m a. (GraphQLObject a, Monad m)
             => [FieldResolver m a]
             -> ObjectDef m a
defineObject fs = (def, resolver)
  where a = undefined :: a
        def = ObjectTypeDef (typeName a)
                            (typeDesc a)
                            [ d | InterfaceImpl d _ <- objectInterfaces a ]
                            [ f | FieldResolver f _ <- fs ]
        resolver td = objectResolver fieldResolvers typeChecker
          where fieldResolvers :: Trie (a -> [Argument] -> Resolver m)
                fieldResolvers = Trie.fromList
                  [(n, f td) | FieldResolver (FieldDef n _ _ _ _) f <- fs]
                typeChecker = makeTypeChecker (typeDefs td) def

objectResolver :: forall m a. (GraphQLNamed a, Monad m)
               => Trie (a -> [Argument] -> Resolver m)
               -> TypeChecker
               -> Resolver' m a
objectResolver fs tc x ss =
    first joinObject <$> collectErrs (traverse fVal fields)
  where fVal (Field a n args _ ss') = first (fKey' a n)
                                  <$> fVal' n args ss'
        fVal' "__typename" _ _ =
          return (toValue $ typeName (undefined :: a), [])
        fVal' n args ss' =
          case Trie.lookup n fs of
               Just f  -> f x args ss'
               Nothing -> errMsg "Field not defined"
        -- determine which inline fragment spreads to expand
        fields = Set.toList $ collapse' ss
        collapse (SelectionField f) = Set.singleton f
        collapse (SelectionInlineFragment (InlineFragment t _ ss'))
          | tc t      = collapse' ss'
          | otherwise = Set.empty
        collapse _    = Set.empty -- We should have expanded all fragment spreads by now
        collapse'     = foldl' (\acc -> Set.union acc . collapse) Set.empty

type TypeChecker = NamedType -> Bool

makeTypeChecker :: Trie TypeDef -> ObjectTypeDef -> TypeCondition -> Bool
makeTypeChecker tds o@(ObjectTypeDef n _ ifs _) (NamedType n') =
  case Trie.lookup n' tds of
       Just (TypeDefObject    _  ) -> n == n'
       Just (TypeDefInterface def) -> def `elem` ifs
       Just (TypeDefUnion (UnionTypeDef _ _ types)) -> o `elem` types
       _ -> False

fKey' :: Alias -> Name -> Builder -> Builder
fKey' a n b = buildString (fKey a n) <> charUtf8 ':' <> b

type ObjectType m = (ObjectTypeDef, TypeDict m -> ObjectResolver m)
typeObject :: (GraphQLObject a, Monad m) => ObjectDef m a -> ObjectType m
typeObject (def, r) = (def, \d -> ObjectResolver (r d))

-- Generic Object Resolver

-- If a GraphQLObject consists of a record of resolveable objects
-- we can infer its resolver
-- We can't always do this if circular dependencies get in the way
-- but this saves a lot of boilerplate for writing mutations

class (GraphQLNamed a) => GraphQLObject' a where
  objectInterfaces' :: a -> [InterfaceTypeDef]
  objectInterfaces' _ = []
  objectDocs' :: a -> [Doc]
  objectDocs' _ = []

  objectFieldDefs'  :: a -> [FieldDef]
  default objectFieldDefs' :: (Generic a, GIsObject' (Rep a)) => a -> [FieldDef]
  objectFieldDefs' _ = gobjectFieldDefs' $ from (undefined :: a)

  objectResolvers'  :: (Monad m) => a -> [FieldResolver m a]
  default objectResolvers' :: (Generic a, GIsObject' (Rep a), Monad m) => a -> [FieldResolver m a]
  objectResolvers' _ = map liftGen $ gobjectResolvers' $ from (undefined :: a)
    where liftGen :: (Monad m) => FieldResolver m ((Rep a) x) -> FieldResolver m a
          liftGen (FieldResolver f r) = FieldResolver f r'
            where r' d = r d . from

resolveObject' :: forall m a. (GraphQLObject' a, Monad m) => TypeDict m -> Resolver' m a
resolveObject' td = objectResolver frs tc
  where a = undefined :: a
        tc = makeTypeChecker (typeDefs td) $ objectDef' a
        frs :: Trie (a -> [Argument] -> Resolver m)
        frs = Trie.fromList
          [ (n, r td) | FieldResolver (FieldDef n _ _ _ _) r <- objectResolvers' a ]

defineObject' :: (GraphQLObject' a) => a -> TypeDef
defineObject' = TypeDefObject . objectDef'

objectDef' :: forall a. (GraphQLObject' a) => a -> ObjectTypeDef
objectDef' _  = ObjectTypeDef (typeName a) (typeDesc a) ifs fdefs
  where a     = undefined :: a
        ifs   = objectInterfaces' a
        fdefs = map desc' $ objectFieldDefs' a
        -- agument field definitions with provided descriptions
        descs = objectDocs' a
        desc' (FieldDef n d args t td) =
          case docDesc <$> List.find ((== n) . docName) descs of
               Just d' -> FieldDef n d' args t td
               Nothing -> FieldDef n d  args t td

class GIsObject' f where
  gobjectFieldDefs' :: f a -> [FieldDef]
  gobjectResolvers' :: (Monad m) => f a -> [FieldResolver m (f a)]

-- delegate resolution upwards
instance (GIsObject' a, Datatype c) => GIsObject' (M1 D c a) where
  gobjectFieldDefs' _ = gobjectFieldDefs' (undefined :: a x)
  gobjectResolvers' _ = map liftCon $ gobjectResolvers' (undefined :: a b)
    where liftCon :: (Monad m) => FieldResolver m (a x) -> FieldResolver m (M1 D c a x)
          liftCon (FieldResolver f r) = FieldResolver f r'
            where r' d (M1 x) = r d x

instance (GIsObject' a, Constructor c) => GIsObject' (M1 C c a) where
  gobjectFieldDefs' _ = gobjectFieldDefs' (undefined :: a x)
  gobjectResolvers' _ = map liftCon $ gobjectResolvers' (undefined :: a b)
    where liftCon :: (Monad m) => FieldResolver m (a x) -> FieldResolver m (M1 C c a x)
          liftCon (FieldResolver f r) = FieldResolver f r'
            where r' d (M1 x) = r d x

-- transform a resolver to its product resolver
instance (GIsObject' a, GIsObject' b) => GIsObject' (a :*: b) where
  gobjectFieldDefs' _ = gobjectFieldDefs' (undefined :: a x)
                     <> gobjectFieldDefs' (undefined :: b x)
  gobjectResolvers' _ = map liftLeft  (gobjectResolvers' (undefined :: a x))
                     <> map liftRight (gobjectResolvers' (undefined :: b x))
    where liftLeft :: (Monad m) => FieldResolver m (a x) -> FieldResolver m ((a :*: b) x)
          liftLeft (FieldResolver f r) = FieldResolver f r'
            where r' d (a :*: _) = r d a
          liftRight :: (Monad m) => FieldResolver m (b x) -> FieldResolver m ((a :*: b) x)
          liftRight (FieldResolver f r) = FieldResolver f r'
            where r' d (_ :*: b) = r d b

-- fuck.
instance (GIsField k i a, Selector c) => GIsObject' (M1 S c (k i a)) where
  gobjectFieldDefs' _ = [FieldDef n "" [] (gfieldType k) (gfieldTypeDef k)]
    where s = undefined :: M1 S c (k i a) x
          k = undefined :: k i a x
          n = BC8.pack $ selName s
  gobjectResolvers' _ = [FieldResolver fdef fres]
    where s = undefined :: M1 S c (k i a) x
          fdef = head $ gobjectFieldDefs' s
          fres :: (Monad m) => TypeDict m -> (M1 S c (k i a) x) -> [Argument] -> Resolver m
          fres d (M1 x) [] = gfieldRes d x
          fres _ _      _  = \_ -> errMsg "Incorrect arguments"

class GIsField k i a where
  gfieldType    :: k i a x -> Type
  gfieldTypeDef :: k i a x -> TypeDef
  gfieldRes     :: (Monad m) => TypeDict m -> Resolver' m (k i a x)

instance (GraphQLValue a) => GIsField K1 i a where
  gfieldType    _    = typeRef (undefined :: a)
  gfieldTypeDef _    = typeDef (undefined :: a)
  gfieldRes d (K1 a) = resolve d a
