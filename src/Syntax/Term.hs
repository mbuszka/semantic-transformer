module Syntax.Term where

import Polysemy
import Polysemy.Output
import Syntax
import qualified Data.Set as Set
import Control.Monad (foldM)

newtype Term = Term {unTerm :: TermF Term}
  deriving (Bound, Pretty)

class Subst t where
  subst :: Member FreshVar r => Var -> TermF Term -> t -> Sem r t

instance Subst Term where
  subst x s (Term t) = Term <$> subst x s t

instance Subst (TermF Term) where
  subst x s t = case t of
    Var v -> pure if v == x then s else Var v
    Abs scope -> Abs <$> subst x s scope
    Let t' scope -> liftA2 Let (subst x s t') (subst x s scope)
    Case t' ps -> liftA2 Case (subst x s t') (subst x s ps)
    t' -> traverse (subst x s) t'

instance Subst (Scope Term) where
  subst x s (Scope xs t)
    | elem x xs = pure (Scope xs t)
    | otherwise =
      let fvs = freeVars s
          aux t x =
            if Set.member x fvs
              then do
                x' <- freshVar
                output x'
                subst x (Var x') t
              else do
                output x
                pure t
       in do
            (xs', t') <- runOutputList $ foldM aux t xs
            Scope xs' <$> subst x s t'

instance Subst (Patterns Term) where
  subst x s(Patterns ps) = Patterns <$> traverse (traverse (subst x s)) ps