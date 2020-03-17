module Syntax.Labeled
  ( Label(..),
    Labeled (..),
    Term,
    nextLabel,
    toLabeled,
    unLabel,
  )
where

import qualified Data.Map as Map
import MyPrelude
import Polysemy
import Polysemy.State
import Syntax
import qualified Syntax.Term as Tm

nextLabel :: Member (State Label) r => Sem r Label
nextLabel = do
  lbl@(Label x) <- get
  put (Label (x + 1))
  return lbl

newtype Label = Label {unLabel :: Int} deriving (Eq, Ord, Pretty)

type Effs r = Members '[State Label, State (Map Label Term)] r

type Term = TermF Label

label :: Effs r => Tm.Term -> Sem r Label
label (Tm.Term t) = do
  lbl <- nextLabel
  t' <- traverse label t
  modify (Map.insert lbl t')
  pure lbl

data Labeled = Labeled (Map Var (Scope Label)) (Map Label Term) DataDecl

toLabeled :: Member (State Label) r => Program (Tm.Term) -> Sem r Labeled
toLabeled program = do
  (tms, Program defs decl) <- program & traverse label & runState Map.empty
  let aux (Def _ name scope) = (name, scope)
  pure $ Labeled (Map.fromList . map aux $ defs) tms decl
