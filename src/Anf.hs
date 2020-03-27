module Anf (Anf (..), fromSource, toSource) where

import Syntax

data Anf
  = Atom (TermF Anf)
  | Expr (TermF Anf)

atom :: TermF Anf -> Sem r Anf
atom = pure . Atom

atomic :: Member FreshVar r => Term -> (Anf -> Sem r Anf) -> Sem r Anf
atomic term k = toAnf term >>= \case
  Expr t -> do
    v <- freshVar
    body <- k (Atom $ Var v)
    pure . Expr . Let (Expr t) $ Scope [(v, Nothing)] body
  Atom t -> k (Atom t)

toAnfValue :: Member FreshVar r => ValueF Term -> Sem r Anf
toAnfValue v = case v of
  Number n -> atom . Cons $ Number n
  String t -> atom . Cons $ String t
  Boolean b -> atom . Cons $ Boolean b
  Record c ts -> seqAnf ts [] (pure . Atom . Cons . Record c)

toAnf :: Member FreshVar r => Term -> Sem r Anf
toAnf term = case unTerm term of
  Var v -> pure . Atom $ Var v
  Abs s -> Atom . Abs <$> traverse toAnf s
  App f ts -> atomic f (\f' -> seqAnf ts [] (pure . Expr . App f'))
  Prim op ts -> seqAnf ts [] (atom . Prim op)
  Let t s -> Expr <$> liftA2 Let (toAnf t) (traverse toAnf s)
  Cons v -> toAnfValue v
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
