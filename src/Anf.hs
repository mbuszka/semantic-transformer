module Anf (Anf (..), fromSource, toSource) where

import Syntax

data Anf
  = Atom (TermF Anf)
  | Expr (TermF Anf)

atomic :: Member FreshVar r => Term -> (Anf -> Sem r Anf) -> Sem r Anf
atomic term k = toAnf term >>= \case
  Expr t -> do
    v <- freshVar
    body <- k (Atom $ Var v)
    pure . Expr . Let (Expr t) $ Scope [(v, Nothing)] body
  Atom t -> k (Atom t)

toAnf :: Member FreshVar r => Term -> Sem r Anf
toAnf term = case unTerm term of
  Var v -> pure . Atom $ Var v
  Abs s -> Atom . Abs <$> traverse toAnf s
  App f ts -> atomic f (\f' -> seqAnf ts [] (pure . Expr . App f'))
  Let t s -> Expr <$> liftA2 Let (toAnf t) (traverse toAnf s)
  Cons (Record c ts) -> seqAnf ts [] (pure . Atom . Cons . Record c)
  Case t cs ->
    atomic t (\t' -> Expr . Case t' <$> traverse toAnf cs)
  Panic -> pure $ Expr Panic

seqAnf ::
  Member FreshVar r => [Term] -> [Anf] -> ([Anf] -> Sem r Anf) -> Sem r Anf
seqAnf [] acc k = k (reverse acc)
seqAnf (t : ts) acc k = atomic t (\t' -> seqAnf ts (t' : acc) k)

fromSource :: Member FreshVar r => Program Term -> Sem r (Program Anf)
fromSource = traverse toAnf

toSource :: Anf -> Term
toSource (Atom a) = Term (fmap toSource a)
toSource (Expr e) = Term (fmap toSource e)

instance Pretty Anf where
  pretty (Atom t) = pretty t
  pretty (Expr t) = pretty t
