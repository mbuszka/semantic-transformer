module Pipeline.Anf
  ( Anf (..),
    transform,
  )
where

import qualified Data.Map as Map
import Syntax
import Syntax.Scoped as Scoped

data Anf
  = Atom (TermF Anf)
  | Expr (TermF Anf)

atom :: TermF Anf -> Sem r Anf
atom = pure . Atom

expr :: TermF Anf -> Sem r Anf
expr = pure . Expr

atomic :: Member FreshVar r => Term -> (Anf -> Sem r Anf) -> Sem r Anf
atomic term k = toAnf term >>= \case
  Expr t -> do
    v <- freshVar "var"
    body <- k (Atom $ Var v)
    expr . Let (Expr t) $ Scope [(v, Nothing)] body
  Atom t -> k (Atom t)

toAnfValue :: Member FreshVar r => ValueF Term -> Sem r Anf
toAnfValue v = case v of
  Number n -> atom . Cons $ Number n
  String t -> atom . Cons $ String t
  Boolean b -> atom . Cons $ Boolean b
  Record c ts -> seqAnf ts [] (atom . Cons . Record c)

toAnf :: Member FreshVar r => Term -> Sem r Anf
toAnf term = case Scoped.term term of
  Var v -> atom $ Var v
  Abs s -> Atom . Abs <$> traverse toAnf s
  App f ts -> case f of
    Term { term = Var v, ..} | fvs Map.!? v == Just PrimOp ->
      seqAnf ts [] (atom . App (Atom $ Var v))
    _ -> atomic f (\f' -> seqAnf ts [] (expr . App f'))
  Let t s -> Expr <$> liftA2 Let (toAnf t) (traverse toAnf s)
  Cons v -> toAnfValue v
  Case t cs ->
    atomic t (\t' -> Expr . Case t' <$> traverse toAnf cs)
  Panic -> pure $ Expr Panic

seqAnf ::
  Member FreshVar r => [Term] -> [Anf] -> ([Anf] -> Sem r Anf) -> Sem r Anf
seqAnf [] acc k = k (reverse acc)
seqAnf (t : ts) acc k = atomic t (\t' -> seqAnf ts (t' : acc) k)

transform :: Member FreshVar r => Program Term -> Sem r (Program Anf)
transform = traverse toAnf

instance Pretty Anf where
  pretty (Atom t) = pretty t
  pretty (Expr t) = pretty t
