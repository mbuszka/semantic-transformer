module Pipeline.Inline
  ( transform
  )
where

import qualified Data.Map as Map
import Syntax

type Effs r = Members '[FreshLabel] r

transform :: Effs r => Term -> Sem r Term
transform = transform' Map.empty

transform' :: Effs r => Map Var Term -> Term -> Sem r Term
transform' env Term {..} = case termTerm of
  Var v -> case Map.lookup v env of
    Just t -> pure t
    Nothing -> term $ Var v
  Abs a s -> term . Abs a =<< transformS env s
  Case t ps -> term =<< (Case <$> transform' env t <*> transformP env ps)
  Let LetAnnot {letGenerated = True} (PVar x) t body -> do
    t' <- transform' env t
    transform' (Map.insert x t' env) body
  Let a x t body -> do
    term =<< (Let a x <$> transform' env t <*> transform' (Map.restrictKeys env (patternVarsSet x)) body)
  t -> term =<< traverse (transform' env) t

transformS :: Effs r => Map Var Term -> Scope Term -> Sem r (Scope Term)
transformS env (Scope xs t) = do
  let env' = foldr' Map.delete env (fmap fst xs)
  Scope xs <$> transform' env' t

transformP :: Effs r => Map Var Term -> Patterns Term -> Sem r (Patterns Term)
transformP env (Patterns ps) = do
  Patterns <$> traverse (traverse (transformS env)) ps