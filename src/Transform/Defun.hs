{-# LANGUAGE LambdaCase #-}

module Transform.Defun where

import qualified Analysis.ControlFlow as Cfa
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Syntax.Anf
import Syntax.Base

-- lambdas :: Program Anf -> Map SLabel (Scope Anf)
-- lambdas =

-- lambdas :: Program Expr -> Map SLabel (Scope Expr)
-- lambdas =
--   foldMap
--       (\case
--         Def _ (Scope _ _ e) -> lambdasE e
--       )
--     . definitions

-- lambdasE :: Expr -> Map SLabel (Scope Expr)
-- lambdasE (App   _ f xs) = lambdasA f <> foldMap lambdasA xs
-- lambdasE (Match _ _ ps) = foldMap (foldMap lambdasE) ps
-- lambdasE (Let   _ e s ) = lambdasE e <> foldMap lambdasE s
-- lambdasE (Err  _ _    ) = Map.empty
-- lambdasE (Atom _ a    ) = lambdasA a

-- lambdasA :: Atom -> Map SLabel (Scope Expr)
-- lambdasA (Lambda s@(Scope l _ e)) = Map.singleton l s <> lambdasE e
-- lambdasA (CApp _ xs             ) = foldMap lambdasA xs
-- lambdasA _                        = Map.empty

-- genApply :: [Scope Expr] -> M (Scope Expr)
-- genApply ss = do
--   l <- scope
--   let Scope _ cnt _ = head ss
--       vars          = [ Local l x | x <- [1 .. cnt] ]
--       gen s@(Scope l' cnt e) =
--         let vs =
--                 Set.filter
--                     (\case
--                       Local{} -> True
--                       _       -> False
--                     )
--                   $ fvsS s
--             old = [ Local l' x | x <- [0 .. cnt - 1] ]
--             e' =
--                 rename (Map.fromList $ vs `zip` [ Local l' x | x <- [0 ..] ])
--                   . rename (Map.fromList $ old `zip` vars)
--                   $ e
--         in  PatConstructor (MkTag $ "Lam" ++ show l') (Scope l' (length vs) e')
--   ps <- traverse gen ss
--   return $ Scope l (cnt + 1) (Match (Local l 0) ps)
