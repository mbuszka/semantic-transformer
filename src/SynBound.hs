module SynBound where

import           Bound
import           Bound.Name
import           Control.Monad                  ( liftM )
import           Data.List.NonEmpty             ( NonEmpty(..) )

type Bind f a = Scope (Name String Int) f a


abstractEitherReName
  :: Monad f => (a -> n) -> (a -> Either b c) -> f a -> Scope (Name n b) f c
abstractEitherReName r f e = Scope (liftM k e) where
  k y = case f y of
    Left  z  -> B (Name (r y) z)
    Right y' -> F (return y')


data Exp a
  = Var a
  | Lam (Bind Exp a)
  | Let (Exp a) [Bind Exp a] (Bind Exp a)
  | App (Exp a) (NonEmpty (Exp a))

