{-# LANGUAGE TupleSections #-}

module Syntax.Denormalized where

import Control.Monad.State
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Map (Map)
import Syntax.Anf hiding (M, expr)
import Syntax.Base
import qualified Syntax.Surface as S

type M a = State (Map ELabel Expr) a

type Expr = ExprF ELabel

insert :: ELabel -> Expr -> M ELabel
insert l e = do
  modify' (Map.insert l e)
  return l

expr :: Anf -> M ELabel
expr (Anf l e) = do
  b <- traverse expr e
  insert l b

data Denormalized
  = Denormalized {exprs :: Map ELabel Expr, defs :: Map Var (Scope ELabel)}

program :: Program Anf -> Denormalized
program (Program ds md) =
  let (ds', exprs) = runState (traverse aux ds) Map.empty
      aux (Def name s) = (Global name,) <$> traverse expr s
   in Denormalized exprs (Map.fromList ds')

getExpr :: ELabel -> Denormalized -> Expr
getExpr l p = exprs p Map.! l
