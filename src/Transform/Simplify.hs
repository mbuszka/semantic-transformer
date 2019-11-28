module Transform.Simplify
  ( expr
  , top
  )
where

import           Syntax
import           Bind
import qualified Data.List.NonEmpty            as NE

expr :: Expr a -> Expr a
expr (App (Lambda _ body) args) = expr $ instantiate (args NE.!!) body
expr (App f args) = App (expr f) (fmap expr args)
expr (Case e ps) = Case e (fmap (mapPattern expr) ps)
expr (Lambda ns (Scope e)) = Lambda ns (Scope (expr e))
expr e = e

top :: TopLevel a -> TopLevel a
top (DefFun name args (Scope e)) = DefFun name args (Scope $ expr e)
