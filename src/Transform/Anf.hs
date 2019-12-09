{-# LANGUAGE LambdaCase #-}

module Transform.Anf where

import qualified Syntax                        as S
import qualified Transform.Cfa                 as C
import           Control.Monad.State
import           Control.Monad
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import qualified Bind                          as B
import qualified Data.List.NonEmpty            as NE
import           Data.List.NonEmpty             ( NonEmpty )

fresh :: State Int String
fresh = do
  s <- get
  modify (+ 1)
  return $ "gen-" ++ show s

toExpr :: Either (C.Expr a) (C.Atom a) -> C.Expr a
toExpr (Left  e) = e
toExpr (Right a) = C.Atom a

expr :: S.Expr String -> State Int (Either (C.Expr String) (C.Atom String))
expr (S.Var   x       ) = pure $ Right (C.Var $ x)
expr (S.Const c       ) = pure $ Right (C.Const c)
expr (S.Err   e       ) = pure $ Left (C.Err e)
expr (S.Lambda xs body) = do
  xs'   <- traverse (\x -> fresh) xs
  body' <- expr (S.unbind xs' body)
  return $ Right (C.Lambda xs' (toExpr body'))
expr (S.App  f xs) = Left <$> atomic f (\f -> app f xs)
expr (S.CApp c xs) = capp c xs
expr (S.Case e ps) = Left <$> atomic e (\v -> match v ps)

match :: C.Atom String -> [S.Pattern S.Expr String] -> State Int (C.Expr String)
match v ps = do
  ps' <- traverse aux ps
  pure $ C.Match v ps'
 where
  aux (S.PatWild e     ) = C.PatWild . toExpr <$> expr e
  aux (S.PatConst c e  ) = C.PatConst c . toExpr <$> expr e
  aux (S.PatCons c xs e) = do
    xs' <- traverse (const fresh) xs
    C.PatCons c xs' . toExpr <$> expr (S.unbind xs' e)

atomic
  :: S.Expr String
  -> (C.Atom String -> (State Int (C.Expr String)))
  -> State Int (C.Expr String)
atomic e k = expr e >>= \case
  Left e -> do
    v <- fresh
    C.Let v e <$> k (C.Var v)
  Right a -> k a

app :: C.Atom String -> NonEmpty (S.Expr String) -> State Int (C.Expr String)
app f xs = aux [] (NE.toList xs)
 where
  aux acc []       = pure $ C.App f (NE.fromList $ reverse acc)
  aux acc (x : xs) = atomic x (\v -> aux (v : acc) xs)

capp
  :: S.Cons
  -> NonEmpty (S.Expr String)
  -> State Int (Either (C.Expr String) (C.Atom String))
capp c xs = aux [] (NE.toList xs)
 where
  aux acc []       = pure . Right $ C.CApp c (NE.fromList $ reverse acc)
  aux acc (x : xs) = do
    e' <- expr x
    case e' of
      Left e -> do
        v <- fresh
        Left . C.Let v e . toExpr <$> aux (C.Var v : acc) xs
      Right a -> aux (a : acc) xs
