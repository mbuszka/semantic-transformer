{-# LANGUAGE UndecidableInstances #-}

module AbsInt
  ( lookup,
    run,
    Function (..),
    Result,
  )
where

import Import
import qualified AbsInt.Types as T
import AbsInt.Types (AbsInt (..), Store (..), Value)
import AbsInt.Runner
import qualified Data.Map as Map
import qualified Data.Set as Set
import Common
import Polysemy.Error
import Syntax
import Polysemy.State
import Util.Pretty
import qualified Pipeline.Scope as Scope

type Result = Map Label (Set Function)

data Function
  = Lambda Label
  | Global Var
  | PrimOp PrimOp
  deriving (Eq, Ord)

instance Pretty Function where
  pretty (Lambda l) = parens $ "lambda" <+> pretty l
  pretty (Global v) = parens $ "global" <+> pretty v
  pretty (PrimOp o) = parens $ "primop" <+> pretty o

fmt :: Value -> [Function]
fmt value = case value of
  T.Closure _ lbl -> [Lambda lbl]
  T.Global var -> [Global var]
  T.PrimOp op -> [PrimOp op]
  _ -> []

lookup :: Member (Error Err) r => Label -> Result -> Sem r (Set Function)
lookup l res = case Map.lookup l res of
  Nothing -> throwLabeled l $ "No analysis result"
  Just fs -> pure fs

run :: Members '[Error Err, FreshLabel, Embed IO] r => Program Term -> Sem r Result
run program = do
  Scope.Result {free = absIntFvs} <- Scope.analyseProgram program
  let absIntGlobals = Map.empty
      absIntTerms = Map.empty
  evalState AbsInt {..} do
    Store values <- runEffs program
    pure $ values <&> (\vs -> vs & Set.toList >>= fmt & Set.fromList)