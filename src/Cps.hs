module Cps where

import Syntax

toCps :: MonadStx m => Term -> TermF Term -> m Term
toCps t k = case unTerm t of
  v@Var {} -> mkTerm =<< liftA2 App (mkTerm k) (traverse mkTerm [v])
  Abs (Scope xs t) -> do
    k' <- freshVar
    t' <- toCps t (Var k')
    f <- mkTerm $ Abs (Scope (xs <> [k']) t')
    mkTerm =<< liftA2 App (mkTerm k) (pure [f])
  App f ts -> do
    k' <- mkTerm k
    mkTerm $ App f (ts <> [k'])
  Let t (Scope [x] b) -> do
    b' <- toCps b k
    toCps t (Abs $ Scope [x] b')
  Case t ps -> case k of
    Var k -> mkTerm . Case t =<< traverse (flip toCps (Var k)) ps
    _ -> do
      k' <- freshVar
      ps' <- traverse (flip toCps (Var k')) ps
      mkTerm =<< liftA2 Let (mkTerm k) (Scope [k'] <$> mkTerm (Case t ps'))
  Cons c ts -> mkTerm =<< liftA2 App (mkTerm k) (traverse mkTerm [Cons c ts])

fromAnf :: MonadStx m => Program Term -> m (Program Term)
fromAnf (Program defs) = Program <$> traverse aux defs
  where
    aux (Def as x (Scope xs t))
      | x == mkVar "main" = do
        a <- freshVar
        k <- Abs . Scope [a] <$> mkTerm (Var a)
        t' <- toCps t k
        pure (Def as x (Scope xs t'))
      | otherwise = do
        k <- freshVar
        t' <- toCps t (Var k)
        pure (Def as x (Scope (xs <> [k]) t'))