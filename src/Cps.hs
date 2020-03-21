module Cps where

import Syntax
import Syntax.Term
import Anf
import Polysemy

term :: TermF Term -> Sem r Term
term = pure . Term

toCps :: Member FreshVar r => Anf -> TermF Term -> Sem r Term
toCps (Atom t) k = (\v -> Term $ App (Term k) [v]) <$> atomic t
toCps (Expr tm) k = case tm of
  App (Atom f) ts -> do
    f' <- atomic f
    ts' <- traverse atomic' ts
    term $ App f' (ts' <> [Term k])
  Let t (Scope [x] b) -> do
    b' <- toCps b k
    toCps t (Abs $ Scope [x] b')
  Let _ _ -> error "Let should bind only a single variable"
  Case (Atom t) ps -> do
    t' <- atomic t
    case k of
      Var{} -> Term . Case t' <$> traverse (flip toCps k) ps
      _ -> do
        k' <- freshVar
        ps' <- traverse (flip toCps (Var k')) ps
        term $ Let (Term k) (Scope [k'] (Term $ Case t' ps'))
  Error -> term Error

atomic :: Member FreshVar r => TermF Anf -> Sem r Term
atomic (Var v) = pure . Term $ Var v
atomic (Abs (Scope xs t)) = do
  k' <- freshVar
  t' <- toCps t (Var k')
  pure . Term . Abs $ Scope (xs <> [k']) t'
atomic (Cons r) = Term . Cons <$> traverse atomic' r
atomic _ = error "Unexpected term after Anf"

atomic' :: Member FreshVar r => Anf -> Sem r Term
atomic' (Atom t) = atomic t
atomic' _ = error "Expected subexpression to be an atom"

fromAnf :: Member FreshVar r => Program Anf -> Sem r (Program Term)
fromAnf (Program defs dt) = Program <$> traverse aux defs <*> pure dt
  where
    aux :: Member FreshVar r => Def Anf -> Sem r (Def Term)
    aux (Def as x (Scope xs t))
      | x == mkVar "main" = do
        a <- freshVar
        let k = Abs . Scope [a] . Term $ Var a
        t' <- toCps t k
        pure (Def as x (Scope xs t'))
      | otherwise = do
        k <- freshVar
        t' <- toCps t (Var k)
        pure (Def as x (Scope (xs <> [k]) t'))