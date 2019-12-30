{-# LANGUAGE TupleSections #-}
module Syntax.Denormalized where

import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE
import           Syntax.Base hiding ( Program(..) )
import qualified Syntax.Base                   as S
import qualified Syntax.Surface                as S
import qualified Syntax.Anf                    as A
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Control.Monad.State

type M a = State (Map ELabel Expr) a

insert :: ELabel -> Expr -> M ELabel
insert l e = do
  modify' (Map.insert l e)
  return l

data Expr
  = Err String
  | App Atom (NonEmpty Atom)
  | Match Atom [Pattern ELabel]
  | Let ELabel (Scope ELabel)
  | Atom Atom

data Atom
  = Var Var
  | Const Constant
  | Lambda (Scope ELabel)
  | CApp Tag (NonEmpty Atom)

scope :: Scope (A.Expr) -> M (Scope ELabel)
scope s = traverse denormalize s

atom :: A.Atom -> M Atom
atom (A.Var    v ) = pure $ Var v
atom (A.Constant  c ) = pure $ Const c
atom (A.Lambda s ) = Lambda <$> scope s
atom (A.CApp c xs) = CApp c <$> traverse atom xs

denormalize :: A.Expr -> M ELabel
denormalize (A.Err l s   ) = insert l (Err s)
denormalize (A.App l a as) = App <$> atom a <*> traverse atom as >>= insert l
denormalize (A.Match l a ps) =
  Match <$> atom a <*> traverse pattern ps >>= insert l
denormalize (A.Let l e s) = Let <$> denormalize e <*> scope s >>= insert l
denormalize (A.Atom l a ) = Atom <$> atom a >>= insert l

pattern :: Pattern A.Expr -> M (Pattern ELabel)
pattern (PatWild e   ) = PatWild <$> denormalize e
pattern (PatConst c e) = PatConst c <$> denormalize e
pattern (PatConstructor  t s) = PatConstructor t <$> scope s

data Program = Program
  { exprs :: Map ELabel Expr
  , defs :: Map Var (Scope ELabel)
  }

program :: S.Program A.Expr -> Program
program (S.Program ds md) =
  let (ds', exprs) = runState (traverse aux ds) Map.empty
      aux (Def name s) = (Global name,) <$> scope s
  in  Program exprs (Map.fromList ds')

getExpr :: ELabel -> Program -> Expr
getExpr l p = exprs p Map.! l
