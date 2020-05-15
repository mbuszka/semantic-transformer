module Pipeline.InlineLet
  ( transform,
  )
where

import qualified Data.Map as Map
import Syntax

type Effs r = Members '[FreshLabel] r

transform :: Effs r => Term -> Sem r Term
transform = transform' Map.empty

transform' :: Effs r => Map Var Term -> Term -> Sem r Term
transform' env Term {..} = case termF of
  Var v -> case Map.lookup v env of
    Just t -> pure t
    Nothing -> mkTerm' $ Var v
  Abs a xs t -> mkTerm' . Abs a xs =<< transform' (foldr' Map.delete env xs) t
  Case t ps -> mkTerm' =<< (Case <$> transform' env t <*> transformP env ps)
  Let LetAnnot {letGenerated = True} (PVar x) t body -> do
    t' <- transform' env t
    transform' (Map.insert x t' env) body
  Let a x t body -> do
    mkTerm' =<< (Let a x <$> transform' env t <*> transform' (Map.withoutKeys env (patternVarsSet x)) body)
  t -> mkTerm' =<< traverse (transform' env) t

transformP :: Effs r => Map Var Term -> Branches Term -> Sem r (Branches Term)
transformP env (Branch p t bs) =
  let xs = patternVarsSet p
   in Branch p <$> (transform' (Map.withoutKeys env xs) t) <*> transformP env bs
transformP _ BNil = pure BNil