module Transform.Simplify
  ( expr
  , top
  )
where

import           Syntax
import           Bind
import qualified Data.List.NonEmpty            as NE
import Data.List.NonEmpty (NonEmpty)

expr :: Expr a -> Expr a
expr (App f args) = app (expr f) (fmap expr args)
expr (Case e ps) = Case e (fmap (mapPattern expr) ps)
expr (Lambda ns (Scope e)) = Lambda ns (Scope (expr e))
expr e = e

app :: Expr a -> NonEmpty (Expr a) -> Expr a
app f@(Lambda ns body) xs
  | all isTrivial xs = expr (instantiate (xs NE.!!) body)
  | otherwise = App f xs
app f xs = App f xs

top :: TopLevel a -> TopLevel a
top (DefFun name args (Scope e)) = DefFun name args (Scope $ expr e)

isTrivial :: Expr a -> Bool
isTrivial Var{}       = True
isTrivial Const{}     = True
isTrivial Lambda{}    = True
isTrivial (CApp _ xs) = all isTrivial xs
isTrivial _           = False