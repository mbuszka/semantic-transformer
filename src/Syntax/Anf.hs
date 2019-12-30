{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

module Syntax.Anf where
  -- ( Atom(..)
  -- , Expr(..)
  -- , M
  -- , rename
  -- , scope
  -- , label
  -- , fromSurface
  -- , toSurface
  -- , fvs
  -- , fvsS
  -- )

import           Control.Lens
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Foldable                  ( fold )
import           Data.Text.Prettyprint.Doc
import           Control.Monad.State
import           Syntax.Base
import qualified Syntax.Surface                as S

data Expr
  = Err ELabel String
  | App ELabel Atom (NonEmpty Atom)
  | Match ELabel Atom [Pattern Expr]
  | Let ELabel Expr (Scope Expr)
  | Atom ELabel Atom

data Atom
  = Var Var
  | Constant Constant
  | Lambda (Scope Expr)
  | CApp Tag (NonEmpty Atom)

$(makePrisms ''Expr)
$(makePrisms ''Atom)

rename :: Map Var Var -> Expr -> Expr
rename _ e@Err{}         = e
rename e (App   l f  xs) = App l (renameA e f) (fmap (renameA e) xs)
rename e (Match l v  ps) = Match l (renameA e v) $ fmap (fmap (rename e)) ps
rename e (Let   l ex b ) = Let l (rename e ex) (fmap (rename e) b)
rename e (Atom l a     ) = Atom l $ renameA e a

renameA :: Map Var Var -> Atom -> Atom
renameA e (Var v)      = Var $ e Map.! v
renameA _ c@Constant{} = c
renameA e (Lambda s )  = Lambda $ fmap (rename e) s
renameA e (CApp c xs)  = CApp c $ fmap (renameA e) xs

fvs :: Expr -> Set Var
fvs (App   _ f xs) = fvsA f <> foldMap fvsA xs
fvs (Match _ a ps) = fvsA a <> foldMap fvsP ps
fvs (Let   _ e s ) = fvs e <> fvsS s
fvs (Atom _ a    ) = fvsA a

fvsA :: Atom -> Set Var
fvsA (Var      v) = Set.singleton v
fvsA (Constant _) = Set.empty
fvsA (Lambda   s) = fvsS s
fvsA (CApp _ xs ) = foldMap fvsA xs

fvsP :: Pattern Expr -> Set Var
fvsP (PatConstructor _ s) = fvsS s
fvsP (PatConst       _ e) = fvs e
fvsP (PatWild e         ) = fvs e

fvsS :: Scope Expr -> Set Var
fvsS (Scope l cnt e) =
  fvs e Set.\\ Set.fromList [ Local l x | x <- [0 .. cnt - 1] ]


type M a = State Metadata a
type Res = M (Either Expr (ELabel, Atom))

label :: M ELabel
label = state nextLabel

scope :: M SLabel
scope = state $ nextScope Nothing

binding :: M (SLabel, Atom)
binding = do
  l <- scope
  return (l, Var $ Local l 0)

app :: ELabel -> Atom -> NonEmpty S.Expr -> M Expr
app l f xs = aux [] (NE.toList xs)
 where
  aux :: [Atom] -> [S.Expr] -> M Expr
  aux acc []       = pure $ App l f (NE.fromList $ reverse acc)
  aux acc (x : xs) = toExpr <$> atomic x (\a -> Left <$> aux (a : acc) xs)

toExpr :: Either Expr (ELabel, Atom) -> Expr
toExpr (Left  e     ) = e
toExpr (Right (l, a)) = Atom l a

atomic :: S.Expr -> (Atom -> Res) -> Res
atomic e k = expr e >>= \case
  Left e -> do
    (s, v) <- binding
    l      <- label
    body   <- k v
    return $ Left (Let l e (Scope s 1 $ toExpr body))
  Right (_, a) -> k a

capp :: ELabel -> Tag -> NonEmpty S.Expr -> Res
capp l c xs = aux [] (NE.toList xs)
 where
  aux :: [Atom] -> [S.Expr] -> Res
  aux acc []       = pure . Right $ (l, CApp c $ (NE.fromList $ reverse acc))
  aux acc (x : xs) = atomic x (\a -> aux (a : acc) xs)

expr :: S.Expr -> Res
expr (S.Var      l a               ) = pure (Right (l, Var a))
expr (S.Constant l c               ) = pure (Right (l, Constant c))
expr (S.Lambda   l (Scope sl cnt e)) = expr e >>= \case
  Left  e       -> pure (Right (l, Lambda (Scope sl cnt e)))
  Right (l', a) -> pure (Right (l, Lambda (Scope sl cnt (Atom l' a))))
expr (S.App  l f xs) = atomic f \f -> Left <$> app l f xs
expr (S.CApp l c xs) = capp l c xs
expr (S.Case l e ps) = atomic e (\v -> match l ps v)
expr (S.Err l s    ) = pure (Left (Err l s))

match :: ELabel -> [Pattern S.Expr] -> Atom -> Res
match l ps v = do
  ps' <- traverse aux ps
  pure (Left $ Match l v ps')
  where aux = traverse (\p -> toExpr <$> expr p)

fromSurface :: Program S.Expr -> Program Expr
fromSurface (Program defs md) =
  let (ds', md') =
          runState (traverse (traverse (\v -> toExpr <$> expr v)) defs) md
  in  Program ds' md'

dummy :: ELabel
dummy = ELabel 0

toSurface :: Program Expr -> Program S.Expr
toSurface (Program ds md) = Program ds' md where ds' = fmap toSurfaceE <$> ds

toSurfaceA :: Atom -> S.Expr
toSurfaceA (Var      a) = S.Var dummy a
toSurfaceA (Constant c) = S.Constant dummy c
toSurfaceA (Lambda   s) = S.Lambda dummy (fmap toSurfaceE s)
toSurfaceA (CApp c xs ) = S.CApp dummy c (toSurfaceA <$> xs)

toSurfaceE :: Expr -> S.Expr
toSurfaceE (App l f xs) = S.App l (toSurfaceA f) (toSurfaceA <$> xs)
toSurfaceE (Match _ v ps) =
  S.Case dummy (toSurfaceA v) (fmap toSurfaceE <$> ps)
toSurfaceE (Err _ s  ) = S.Err dummy s
toSurfaceE (Let _ e s) = S.Let dummy (toSurfaceE e) (fmap toSurfaceE s)
toSurfaceE (Atom _ a ) = toSurfaceA a
