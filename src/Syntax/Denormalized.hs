{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Syntax.Denormalized where

import Control.Lens
import Control.Monad.State
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)
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
  = Denormalized
      { _dExpressions :: Map ELabel Expr,
        _dProcedures :: Map String (Scope ELabel)
      }

$(makeLenses ''Denormalized)

program :: Program Anf -> Denormalized
program pgm =
  let (defs, exprs) = runState (traverse aux (pgm ^. definitions)) Map.empty
      aux (Def name body) = (name,) <$> traverse expr body
   in Denormalized exprs (Map.fromList defs)

getExpr :: ELabel -> Denormalized -> Expr
getExpr l pgm =
  fromMaybe (error "Corrupted program") (pgm ^? dExpressions . ix l)
