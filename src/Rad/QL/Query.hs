{-# LANGUAGE ExistentialQuantification #-}

module Rad.QL.Query where

import qualified Data.Aeson          as JSON
import qualified Data.ByteString     as B
import           Data.ByteString.Builder
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe          (fromMaybe, mapMaybe)
import           Data.Monoid         ((<>))
import qualified Data.Scientific     as Scientific
import qualified Data.Trie           as Trie
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as TE

import Rad.QL.AST

-- A processed query must be evaluated with respect to a schema

type TypeDict = Trie.Trie TypeDef
data Schema m = (Monad m) => Schema
  { typeDict      :: TypeDict
  , rootQuery     :: QSelectionSet -> m (Builder, [ B.ByteString ])
  , rootQueryType :: TypeDef
  }

-- Processed query, note that directives and fragment definitions are compiled away

data QNode = QQuery    QSelectionSet
           | QMutation QSelectionSet

type QSelectionSet = [ QSelection ]

data QSelection = QSelectionField  QField
                | QSelectionSpread QCond QSelectionSet

data QField  = QField  Alias Name QArgs QSelectionSet

type QCond = ObjectTypeDef -> Bool

newtype QArgs = QArgs (Trie.Trie QValue) deriving (Eq, Show)

qArgsEmpty :: QArgs
qArgsEmpty = QArgs Trie.empty

qArgs :: [(Name, QValue)] -> QArgs
qArgs = QArgs . Trie.fromList

qArgsLookup :: Name -> QArgs -> Maybe QValue
qArgsLookup n (QArgs trie) = Trie.lookup n trie

data QValue = QInt    Int
            | QFloat  Double
            | QBool   Bool
            | QString B.ByteString -- enums will check for exact bytestring matches
            | QList   [QValue]
            | QObj    QArgs
            | QEmpty
            deriving (Eq, Show)

type ValidationErr = B.ByteString

process :: Schema m -> JSON.Value -> Document -> Either ValidationErr QNode
process schema values (Document defs) = do
    -- extract singular operation
    op <- case [o | DefOperation o <- defs] of
               [o] -> Right o
               _   -> Left "Rad.QL wants exactly one operation"
    -- extract value definitions
    vals <- case values of
                 JSON.Null      -> Right qArgsEmpty
                 JSON.Object o  -> Right $ processObject o
                 JSON.String "" -> Right qArgsEmpty
                 _              -> Left "Invalid variable definitions provided"
   -- extract node from operation
    let Node _ vds _ ss = -- for now we ignore query directives
          case op of
               Query n    -> n
               Mutation n -> n
    -- process variables
    vars <- qArgs <$> traverse (readVal vals) vds
    -- walk tree
    return $ packOp op $ walk vars ss
  where tdict = typeDict schema
        frags = Trie.fromList [ (n, f) | DefFragment f@(FragmentDef n _ _ _) <- defs ]
        -- NOTE: this is currently incorrect!
        -- validations should be performed here, not deferred until field evaluation
        readVal vals (VariableDef (Variable n) t d) =
          case Trie.lookup (typeRefName t) tdict of
               Nothing -> Left  $ "Variable \"$" <> n <> "\" is of unknown type: \"" <> typeRefName t <> "\""
               _       -> Right $ (n, qArgsLookup n vals |?| processLiteral <$> d)
        getTypeCondition (NamedType n) =
          case Trie.lookup n tdict of
               Just (TypeDefObject    o) -> Just (o ==)
               Just (TypeDefInterface i) -> Just $ \(ObjectTypeDef _ _ ifs _) -> i `elem` ifs
               Just (TypeDefUnion (UnionTypeDef _ _ odefs)) -> Just $ \o      -> o `elem` odefs
               _ -> Nothing
        -- walk and process the trie
        -- TODO: handle errors instead of just ignoring them
        walk vals = mapMaybe visit
          where visit (SelectionField (Field a n args ds ss)) = applyDirectives ds
                  (QSelectionField (QField a n (processArgs vals args) (mapMaybe visit ss)))
                visit (SelectionFragmentSpread (FragmentSpread n ds)) = do
                  FragmentDef n tc ds' ss <- Trie.lookup n frags
                  typeCondition <- getTypeCondition tc
                  applyDirectives (ds <> ds') $
                    QSelectionSpread typeCondition (mapMaybe visit ss)
                visit (SelectionInlineFragment (InlineFragment tc ds ss)) = do
                  typeCondition <- getTypeCondition tc
                  applyDirectives ds $
                    QSelectionSpread typeCondition (mapMaybe visit ss)
                applyDirectives [] = Just
                applyDirectives (Directive "include" [Argument "if" v] : ds)
                  | processValue vals v == QBool True  = applyDirectives ds
                  | otherwise                          = \_ -> Nothing
                applyDirectives (Directive "skip"    [Argument "if" v] : ds)
                  | processValue vals v == QBool False = applyDirectives ds
                  | otherwise                          = \_ -> Nothing
                applyDirectives _ = \_ -> Nothing
        -- repack operation
        packOp (Query    _) ss = QQuery    ss
        packOp (Mutation _) ss = QMutation ss

execute :: (Monad m) => Schema m -> JSON.Value -> Document -> m (Builder, [ B.ByteString ])
execute s v d =
  case process s v d of
       Right (QQuery    n) -> rootQuery s n
       Right (QMutation n) -> pure (byteString "null", [ "not defined" ])
       Left  e             -> pure (byteString "null", [e])

-- helpers
processLiteral :: Value -> QValue
processLiteral (ValueVariable v) = QEmpty
processLiteral (ValueInt      v) = QInt    v
processLiteral (ValueFloat    v) = QFloat  v
processLiteral (ValueBoolean  v) = QBool   v
processLiteral (ValueString   v) = QString v
processLiteral (ValueEnum     v) = QString v
processLiteral (ValueList (ListValue vs))     = QList $ map processLiteral  vs
processLiteral (ValueObject (ObjectValue fs)) = QObj $ qArgs
  [ (n, processLiteral v) | ObjectField n v <- fs ]

processValue :: QArgs -> Value -> QValue
processValue _ (ValueInt      v) = QInt    v
processValue _ (ValueFloat    v) = QFloat  v
processValue _ (ValueBoolean  v) = QBool   v
processValue _ (ValueString   v) = QString v
processValue _ (ValueEnum     v) = QString v
processValue dict (ValueList (ListValue vs)) = QList $ map (processValue dict) vs
processValue dict (ValueObject (ObjectValue fs)) = QObj . QArgs $ Trie.fromList
  [ (n, processValue dict v) | ObjectField n v <- fs ]
processValue dict (ValueVariable (Variable n)) = fromMaybe QEmpty $ qArgsLookup n dict

processArgs :: QArgs -> [Argument] -> QArgs
processArgs dict args = qArgs [ (n, processValue dict v) | Argument n v <- args ]

processVar :: JSON.Value -> QValue
processVar  JSON.Null      = QEmpty
processVar (JSON.String t) = QString $ TE.encodeUtf8 t
processVar (JSON.Bool   b) = QBool b
processVar (JSON.Number n) = either QFloat QInt $ Scientific.floatingOrInteger n
processVar (JSON.Array  a) = QList []
processVar (JSON.Object o) = QObj $ processObject o

processObject :: HashMap.HashMap T.Text JSON.Value -> QArgs
processObject o = qArgs [(TE.encodeUtf8 k, processVar v) | (k, v) <- HashMap.toList o]

infixl 3 |?|
(|?|) :: Maybe QValue -> Maybe QValue -> QValue
(Just QEmpty) |?| v = fromMaybe QEmpty v
(Just v     ) |?| _ = v
Nothing       |?| v = fromMaybe QEmpty v
