module Anf (fromSource) where

import Syntax

data Anf = Atom Term | Expr Term

unwrap :: Anf -> Term
unwrap (Atom t) = t
unwrap (Expr t) = t

atomic :: MonadStx m => Term -> (Term -> m Anf) -> m Anf
atomic t k = toAnf t >>= \case
  Expr t -> do
    v <- freshVar
    body <- k =<< mkTerm (Var v)
    Expr <$> mkTerm (Let t (Scope [v] $ unwrap body))
  Atom t -> k t

atom :: MonadStx m => TermF Term -> m Anf
atom t = Atom <$> mkTerm t

expr :: MonadStx m => TermF Term -> m Anf
expr t = Expr <$> mkTerm t

toAnf' :: MonadStx m => Term -> m Term
toAnf' t = unwrap <$> toAnf t

toAnf :: MonadStx m => Term -> m Anf
toAnf t = case unTerm t of
  v@Var {} -> atom v
  Abs s -> atom . Abs =<< toAnfS s
  App f ts -> atomic f (\f -> seqAnf ts [] (expr . App f))
  Let t s -> expr =<< liftA2 Let (toAnf' t) (toAnfS s)
  Cons c ts -> seqAnf ts [] (atom . Cons c)
  Case t cs ->
    atomic t (\t -> expr . Case t =<< traverse toAnf' cs)

toAnfS :: MonadStx m => Scope Term -> m (Scope Term)
toAnfS (Scope xs t) = Scope xs <$> toAnf' t

seqAnf :: MonadStx m => [Term] -> [Term] -> ([Term] -> m Anf) -> m Anf
seqAnf [] acc k = k (reverse acc)
seqAnf (t : ts) acc k = atomic t (\t -> seqAnf ts (t : acc) k)

fromSource :: MonadStx m => Program Term -> m (Program Term)
fromSource (Program defs) = Program <$> traverse (traverse toAnf') defs
