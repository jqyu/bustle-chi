{-# LANGUAGE OverloadedStrings #-}

module Rad.QL.Execute
  ( execute
  , subst
  ) where

-- import           Control.Arrow (first)
import qualified Data.Aeson              as JSON
import           Data.ByteString.Builder
-- import qualified Data.ByteString.Char8   as BC8
import           Data.Monoid             ((<>))
import           Data.Trie               (Trie)
import qualified Data.Trie               as Trie

import Rad.QL.AST
import Rad.QL.Error
import Rad.QL.Resolver.Schema

execute :: (Monad m) => Schema m -> OperationDef -> CollectErrs m Builder
execute (Schema _ rq _) (Query    (Node _ _ _ ss)) = rq ss
    -- concatShit b = b
    --             <> byteString ", \"query\": "
    --             <> buildString (BC8.pack $ show op)
execute (Schema _ _ rm) (Mutation (Node _ _ _ ss)) = rm ss

subst :: JSON.Value -> Document -> Either String OperationDef
subst vars (Document defs) =
    case ops of
         [op] -> subst' vars' op frags
         _    -> Left "Rad.QL wants exactly one operation"
  where ops = [ op | DefOperation op <- defs ]
        frags = Trie.fromList
          [ (n, f) | DefFragment f@(FragmentDef n _ _ _) <- defs ]
        -- TODO: parse vars from JSON
        vars' = Trie.fromList []

subst' :: Trie Value -> OperationDef -> Trie FragmentDef -> Either String OperationDef
subst' v op frags = applyOp (substVars v) op
                >>= applyOp (substFrags frags'')
  where frags'  = traverse (applyFrag $ substVars v) frags
        -- NOTE: this does not take into account recursive fragment spreads yet
        -- let me know if you can figure out a nice fix,
        -- using Data.Function.fix yields an infinite loop in this case
        frags'' = frags' >>= traverse (applyFrag $ substFrags frags')

substVars :: Trie Value -> SelectionSet -> Either String SelectionSet
substVars vs = traverse substSel
  where substSel (SelectionField (Field a n as ds ss)) = do
          args <- substArgs as
          ss'  <- substVars vs ss
          return $ SelectionField $ Field a n args ds ss'
        substSel (SelectionInlineFragment (InlineFragment tc ds ss))
           =  SelectionInlineFragment
           .  InlineFragment tc ds
          <$> substVars vs ss
        substSel s = Right s
        substArgs = traverse substArg
        substArg (Argument n (ValueVariable (Variable v))) =
          case Trie.lookup v vs of
               Just v' -> Right $ Argument n v'
               Nothing -> Left "Variable not defined"
        substArg a = Right a

substFrags :: Either String (Trie FragmentDef) -> SelectionSet -> Either String SelectionSet
substFrags   (Left   e) = \_ -> Left e
substFrags f@(Right fs) = traverse substSel
  where substSel (SelectionFragmentSpread (FragmentSpread n ds)) =
          case Trie.lookup n fs of
               Just (FragmentDef _ tc ds' ss) ->
                 SelectionInlineFragment . InlineFragment tc (ds <> ds') <$> substFrags f ss
               Nothing -> Left "Fragment not found"
        substSel (SelectionField (Field a n as ds ss)) =
          SelectionField . Field a n as ds <$> substFrags f ss
        substSel inline = Right inline

applyOp :: (SelectionSet -> Either String SelectionSet) -> OperationDef -> Either String OperationDef
applyOp f (Query    (Node n vds ds ss)) = Query    . Node n vds ds <$> f ss
applyOp f (Mutation (Node n vds ds ss)) = Mutation . Node n vds ds <$> f ss

applyFrag :: (SelectionSet -> Either String SelectionSet) -> FragmentDef -> Either String FragmentDef
applyFrag f (FragmentDef n tc ds ss) = FragmentDef n tc ds <$> f ss

