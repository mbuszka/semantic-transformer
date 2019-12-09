{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module Syntax.Anf
  ( Atom(..)
  , Expr(..)
  , fromSurface
  , toSurface
  )
where

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
import           Syntax.Scope
import qualified Syntax.Surface                as S

data Expr a
  = Err Label String
  | App Label (Atom a) (NonEmpty (Atom a))
  | Match Label (Atom a) [Pattern Expr a]
  | Let Label (Expr a) (Scope Expr a)
  | Atom Label (Atom a)
  deriving (Functor, Foldable, Traversable)

data Atom a
  = Var a
  | Const Const
  | Lambda (Scope Expr a)
  | CApp Cons (NonEmpty (Atom a))
  deriving (Functor, Foldable, Traversable)

type M a = State Metadata a
type Res a = M (Either (Expr a) (Label, Atom a))

label :: M Label
label = state nextLabel

scope :: M ScopeLabel
scope = state $ nextScope Nothing

fresh :: Atom (Var a)
fresh = Var (B 0)

app :: Label -> Atom a -> NonEmpty (S.Expr a) -> M (Expr a)
app l f xs = aux f [] (NE.toList xs)
 where
  aux :: Atom a -> [Atom a] -> [S.Expr a] -> M (Expr a)
  aux f acc []       = pure $ App l f (NE.fromList $ reverse acc)
  aux f acc (x : xs) = toExpr <$> atomic
    x
    (\v -> Left <$> aux (weaken f) (v : fmap weaken acc) (fmap weaken xs))
    (\a -> Left <$> aux f (a : acc) xs)

toExpr :: (Either (Expr a) (Label, Atom a)) -> Expr a
toExpr (Left  e     ) = e
toExpr (Right (l, a)) = Atom l a

atomic
  :: S.Expr a -> (Atom (Var a) -> Res (Var a)) -> (Atom a -> Res a) -> Res a
atomic e kl kr = expr e >>= \case
  Left e -> do
    l <- label
    b <- kl fresh
    s <- scope
    return $ Left (Let l e (Scope s 1 $ toExpr b))
  Right (_, a) -> kr a

capp :: Label -> Cons -> NonEmpty (S.Expr a) -> Res a
capp l c xs = aux [] (NE.toList xs)
 where
  aux :: [Atom a] -> [S.Expr a] -> Res a
  aux acc []       = pure . Right $ (l, CApp c $ (NE.fromList $ reverse acc))
  aux acc (x : xs) = atomic
    x
    (\v -> aux (v : fmap weaken acc) (fmap weaken xs))
    (\a -> aux (a : acc) xs)

expr :: S.Expr a -> Res a
expr (S.Var    l a               ) = pure (Right (l, Var a))
expr (S.Const  l c               ) = pure (Right (l, Const c))
expr (S.Lambda l (Scope sl cnt e)) = expr e >>= \case
  Left  e       -> pure (Right (l, Lambda (Scope sl cnt e)))
  Right (l', a) -> pure (Right (l, Lambda (Scope sl cnt (Atom l' a))))
expr (S.App l f xs) = expr f >>= \case
  Left e -> do
    l' <- label
    sl <- scope
    b  <- app l fresh (weaken <$> xs)
    return $ Left (Let l' e (Scope sl 1 b))
  Right (_, a) -> Left <$> app l a xs
expr (S.CApp l c xs) = capp l c xs
expr (S.Case l e ps) =
  atomic e (\v -> match l (fmap weaken ps) v) (\v -> match l ps v)
expr (S.Err l s) = pure (Left (Err l s))

match :: Label -> [Pattern S.Expr a] -> Atom a -> Res a
match l ps v = do
  ps' <- traverse aux ps
  pure (Left $ Match l v ps')
  where aux = mtraverse (\p -> toExpr <$> expr p)

fromSurface :: Program S.Expr -> Program Expr
fromSurface (Program defs md) =
  let (ds', md') =
          runState (traverse (mtraverse (\v -> toExpr <$> expr v)) defs) md
  in  Program ds' md'

dummy :: Label
dummy = Label 0

toSurface :: Program Expr -> Program S.Expr
toSurface (Program ds md) = Program ds' md where ds' = mmap toSurfaceE <$> ds

toSurfaceA :: Atom a -> S.Expr a
toSurfaceA (Var    a ) = S.Var dummy a
toSurfaceA (Const  c ) = S.Const dummy c
toSurfaceA (Lambda s ) = S.Lambda dummy (mmap toSurfaceE s)
toSurfaceA (CApp c xs) = S.CApp dummy c (toSurfaceA <$> xs)

toSurfaceE :: Expr a -> S.Expr a
toSurfaceE (App l f xs) = S.App l (toSurfaceA f) (toSurfaceA <$> xs)
toSurfaceE (Match _ v ps) =
  S.Case dummy (toSurfaceA v) (mmap toSurfaceE <$> ps)
toSurfaceE (Err _ s  ) = S.Err dummy s
toSurfaceE (Let _ e s) = S.Let dummy (toSurfaceE e) (mmap toSurfaceE s)
toSurfaceE (Atom _ a ) = toSurfaceA a
