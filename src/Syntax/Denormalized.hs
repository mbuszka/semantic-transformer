{-# LANGUAGE TupleSections #-}
module Syntax.Denormalized where

import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE
import           Syntax.Base                    ( Cons(..)
                                                , Const(..)
                                                , Label(..)
                                                )
import qualified Syntax.Base                   as S
import           Syntax.Scope                   ( ScopeLabel(..) )
import qualified Syntax.Scope                  as S
import qualified Syntax.Surface                as S
import qualified Syntax.Anf                    as A
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Control.Monad.State

type M a = State (Map Label Expr) a

insert :: Label -> Expr -> M Label
insert l e = do
  modify' (Map.insert l e)
  return l

data Expr
  = Err String
  | App Atom (NonEmpty Atom)
  | Match Atom [Pattern]
  | Let Label Scope
  | Atom Atom

data Atom
  = Var Var
  | Const Const
  | Lambda Scope
  | CApp Cons (NonEmpty Atom)

data Pattern
  = PatWild Label
  | PatConst Const Label
  | PatCons Cons Scope

data Var = Global String | Local ScopeLabel Int
  deriving (Eq, Ord, Show)

data Scope = Scope ScopeLabel Int Label
  deriving (Eq, Ord, Show)

scope :: S.Scope A.Expr Var -> M Scope
scope s@(S.Scope ls cnt e) = do
  let e' = S.unscope s (Local ls) id
  l <- denormalize e'
  return $ Scope ls cnt l

atom :: A.Atom Var -> M Atom
atom (A.Var    v ) = pure $ Var v
atom (A.Const  c ) = pure $ Const c
atom (A.Lambda s ) = Lambda <$> scope s
atom (A.CApp c xs) = CApp c <$> traverse atom xs

denormalize :: A.Expr Var -> M Label
denormalize (A.Err l s   ) = insert l (Err s)
denormalize (A.App l a as) = App <$> atom a <*> traverse atom as >>= insert l
denormalize (A.Match l a ps) =
  Match <$> atom a <*> traverse pattern ps >>= insert l
denormalize (A.Let l e s) = Let <$> denormalize e <*> scope s >>= insert l
denormalize (A.Atom l a ) = Atom <$> atom a >>= insert l

pattern :: S.Pattern A.Expr Var -> M Pattern
pattern (S.PatWild e   ) = PatWild <$> denormalize e
pattern (S.PatConst c e) = PatConst c <$> denormalize e
pattern (S.PatCons  t s) = PatCons t <$> scope s

data Program = Program
  { exprs :: Map Label Expr
  , defs :: Map Var Scope
  }

program :: S.Program A.Expr -> Program
program (S.Program ds md) =
  let (ds', exprs) = runState (traverse aux ds) Map.empty
      aux (S.Def name s) = (Global name,) <$> scope (Global <$> s)
  in  Program exprs (Map.fromList ds')

getExpr :: Label -> Program -> Expr
getExpr l p = exprs p Map.! l
