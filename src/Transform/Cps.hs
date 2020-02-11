{-# LANGUAGE RankNTypes #-}

module Transform.Cps where

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))
import Syntax.Anf hiding
  ( expr,
    program,
    scope,
  )
import Syntax.Base

expr :: Atom Anf -> Anf -> Anf
expr k (Anf l e) = case e of
  (Err s) -> Anf l $ Err s
  (App f xs) -> Anf l $ App f (xs <> (k :| []))
  (Match a ps) -> Anf l $ Match (atom a) $ fmap (expr k) <$> ps
  (Let e s) -> expr (Lambda (expr k <$> s)) e
  (Atom a) -> Anf l $ App k (atom a :| [])

scope :: Scope Anf -> Scope Anf
scope s = let (s', v) = appendFreshVar s in expr (Var v) <$> s'

atom :: Atom Anf -> Atom Anf
atom v@Var {} = v
atom c@Constant {} = c
atom (Lambda s) = Lambda (scope s)

program :: Program Anf -> Program Anf
program (Program defs md) = Program (aux <$> defs) md
  where
    aux (Def n s) = Def n (scope s)
