module Cps where

import Syntax
import Anf

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
        term $ Let (Term k) (Scope [(k', Nothing)] (Term $ Case t' ps'))
  Panic -> term Panic
  _ -> error "Unexpected term inside Expr"

atomic :: Member FreshVar r => TermF Anf -> Sem r Term
atomic (Var v) = pure . Term $ Var v
atomic (Abs (Scope xs t)) = do
  k' <- freshVar
  t' <- toCps t (Var k')
  pure . Term . Abs $ Scope (xs <> [(k', Nothing)]) t'
atomic (Cons r) = Term . Cons <$> traverse atomic' r
atomic _ = error "Unexpected term after Anf"

atomic' :: Member FreshVar r => Anf -> Sem r Term
atomic' (Atom t) = atomic t
atomic' _ = error "Expected subexpression to be an atom"

fromAnf :: Member FreshVar r => Program Anf -> Sem r (Program Term)
fromAnf Program {..} = do
  defs <- traverse aux (programDefinitions)
  main <- do
    a <- freshVar
    let k = Abs . Scope [(a, Nothing)] . Term $ Var a
    traverse (flip toCps k) (programMain)
  pure $ Program { programDefinitions = defs, programMain = main, .. }
  where
    aux :: Member FreshVar r => Def Anf -> Sem r (Def Term)
    aux (Def as (Scope xs t)) = do
        k <- freshVar
        t' <- toCps t (Var k)
        pure (Def as (Scope (xs <> [(k, Nothing)]) t'))