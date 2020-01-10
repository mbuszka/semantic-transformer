{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

module Syntax.Anf where
  -- ( Atom(..)
  -- , Anf(..)
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
import           Data.Data
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

data Anf = Anf ELabel (ExprF Anf)
  deriving (Data, Typeable)

data ExprF e
  = Err String
  | App (Atom e) (NonEmpty (Atom e))
  | Match (Atom e) [Pattern e]
  | Let e (Scope e)
  | Atom (Atom e)
  deriving (Functor, Foldable, Traversable, Data, Typeable)

data Atom e
  = Var Var
  | Constant Constant
  | Lambda (Scope e)
  | CApp Tag (NonEmpty (Atom e))
  deriving (Functor, Foldable, Traversable, Data, Typeable)

$(makePrisms ''ExprF)
$(makePrisms ''Atom)
$(makeLenses ''Anf)

instance Plated Anf
instance (Data e, Plated e) => Plated (ExprF e)
instance (Data e, Plated e) => Plated (Atom e)

unExpr :: Anf -> ExprF Anf
unExpr (Anf _ e) = e

rename :: Map Var Var -> Anf -> Anf
rename env (Anf l e) = Anf l $ case e of
  e@Err{}       -> e
  (App   f  xs) -> App (renameA env f) (fmap (renameA env) xs)
  (Match v  ps) -> Match (renameA env v) $ fmap (fmap (rename env)) ps
  (Let   ex b ) -> Let (rename env ex) (fmap (rename env) b)
  (Atom a     ) -> Atom $ renameA env a

renameA :: Map Var Var -> Atom Anf -> Atom Anf
renameA e (Var v)      = Var $ e Map.! v
renameA _ c@Constant{} = c
renameA e (Lambda s )  = Lambda $ fmap (rename e) s
renameA e (CApp c xs)  = CApp c $ fmap (renameA e) xs

fvs :: Anf -> Set Var
fvs (Anf _ e) = case e of
  (App   f xs) -> fvsA f <> foldMap fvsA xs
  (Match a ps) -> fvsA a <> foldMap fvsP ps
  (Let   e s ) -> fvs e <> fvsS s
  (Atom a    ) -> fvsA a

fvsA :: Atom Anf -> Set Var
fvsA (Var      v) = Set.singleton v
fvsA (Constant _) = Set.empty
fvsA (Lambda   s) = fvsS s
fvsA (CApp _ xs ) = foldMap fvsA xs

fvsP :: Pattern Anf -> Set Var
fvsP (PatConstructor _ s) = fvsS s
fvsP (PatConst       _ e) = fvs e
fvsP (PatWild e         ) = fvs e

fvsS :: Scope Anf -> Set Var
fvsS (Scope l cnt e) =
  fvs e Set.\\ Set.fromList [ Local l x | x <- [0 .. cnt - 1] ]


type M a = State Metadata a
type Res = M (Either Anf (ELabel, Atom Anf))

label :: M ELabel
label = state nextLabel

scope :: M SLabel
scope = state $ nextScope Nothing

binding :: M (SLabel, Atom Anf)
binding = do
  l <- scope
  return (l, Var $ Local l 0)

app :: ELabel -> Atom Anf -> NonEmpty S.Expr -> M Anf
app l f xs = aux [] (NE.toList xs)
 where
  aux :: [Atom Anf] -> [S.Expr] -> M Anf
  aux acc []       = pure . Anf l $ App f (NE.fromList $ reverse acc)
  aux acc (x : xs) = toExpr <$> atomic x (\a -> Left <$> aux (a : acc) xs)

toExpr :: Either Anf (ELabel, Atom Anf) -> Anf
toExpr (Left  e     ) = e
toExpr (Right (l, a)) = Anf l $ Atom a

atomic :: S.Expr -> (Atom Anf -> Res) -> Res
atomic e k = expr e >>= \case
  Left e -> do
    (s, v) <- binding
    l      <- label
    body   <- k v
    return . Left . Anf l $ Let e (Scope s 1 $ toExpr body)
  Right (_, a) -> k a

capp :: ELabel -> Tag -> NonEmpty S.Expr -> Res
capp l c xs = aux [] (NE.toList xs)
 where
  aux :: [Atom Anf] -> [S.Expr] -> Res
  aux acc []       = pure . Right $ (l, CApp c $ (NE.fromList $ reverse acc))
  aux acc (x : xs) = atomic x (\a -> aux (a : acc) xs)

expr :: S.Expr -> Res
expr (S.Var      l a               ) = pure (Right (l, Var a))
expr (S.Constant l c               ) = pure (Right (l, Constant c))
expr (S.Lambda   l (Scope sl cnt e)) = do
  b <- expr e
  pure . Right $ (l, Lambda (Scope sl cnt $ toExpr b))
expr (S.App  l f xs) = atomic f \f -> Left <$> app l f xs
expr (S.CApp l c xs) = capp l c xs
expr (S.Case l e ps) = atomic e (\v -> match l ps v)
expr (S.Err l s    ) = pure . Left . Anf l $ Err s

match :: ELabel -> [Pattern S.Expr] -> Atom Anf -> Res
match l ps v = do
  ps' <- traverse aux ps
  pure . Left . Anf l $ Match v ps'
  where aux = traverse (\p -> toExpr <$> expr p)

fromSurface :: Program S.Expr -> Program Anf
fromSurface (Program defs md) =
  let (ds', md') =
          runState (traverse (traverse (\v -> toExpr <$> expr v)) defs) md
  in  Program ds' md'

dummy :: ELabel
dummy = ELabel 0

toSurface :: Program Anf -> Program S.Expr
toSurface (Program ds md) = Program ds' md where ds' = fmap toSurfaceE <$> ds

toSurfaceA :: Atom Anf -> S.Expr
toSurfaceA (Var      a) = S.Var dummy a
toSurfaceA (Constant c) = S.Constant dummy c
toSurfaceA (Lambda   s) = S.Lambda dummy (fmap toSurfaceE s)
toSurfaceA (CApp c xs ) = S.CApp dummy c (toSurfaceA <$> xs)

toSurfaceE :: Anf -> S.Expr
toSurfaceE (Anf l e) = case e of
  (App   f xs) -> S.App l (toSurfaceA f) (toSurfaceA <$> xs)
  (Match v ps) -> S.Case dummy (toSurfaceA v) (fmap toSurfaceE <$> ps)
  (Err s     ) -> S.Err dummy s
  (Let e s   ) -> S.Let dummy (toSurfaceE e) (fmap toSurfaceE s)
  (Atom a    ) -> toSurfaceA a
