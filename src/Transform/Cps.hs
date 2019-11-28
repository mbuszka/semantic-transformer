{-# LANGUAGE RankNTypes #-}

module Transform.Cps
  ( env
  , repl
  , top
  )
where

import           Bind
import           Control.Monad.State
import           Data.Bifunctor
import qualified Data.List.NonEmpty            as NE
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.Map                      as Map
import           Eval
import           Syntax
import qualified Transform.Simplify            as Simplify

-- Exported Functions --

repl :: Expr a -> Expr a
repl e = Simplify.expr $ evalState (cpsTransform (lam "x" $ bound 0) e) 0

top :: TopLevel a -> TopLevel a
top t = Simplify.top $ evalState (cpsTop t) 0

env :: Env -> Env
env = fmap f
 where
  f (PrimOp p) = PrimOp
    (\xs ->
      let ys                    = NE.fromList $ NE.init xs
          Closure (k :| []) b e = NE.last xs
      in  (eval (Map.insert k (p ys) e) b id)
    )


-- Helpers --

singleton :: a -> NonEmpty a
singleton x = x :| []

append :: NonEmpty a -> a -> NonEmpty a
append (x :| xs) y = x :| xs ++ [y]

fresh :: State Int String
fresh = do
  x <- get
  modify' (+ 1)
  return $ "gen-" ++ show x

app :: Expr a -> Expr a -> Expr a
app f x = App f (singleton x)

lam :: String -> Expr (Var Int a) -> Expr a
lam n b = Lambda (singleton n) (Scope b)

bound :: b -> Expr (Var b a)
bound = Var . B

free :: a -> Expr (Var b a)
free = Var . F

mkAppK :: a -> [a] -> Expr a -> State Int (Expr a)
mkAppK f xs k = do
  name <- fresh
  let body = App (free f) (NE.reverse $ weaken k :| bound 0 : fmap free xs)
  return $ lam name body

mkCAppK :: Cons -> [a] -> Expr a -> State Int (Expr a)
mkCAppK c xs k = do
  name <- fresh
  let body = app (weaken k) $ CApp c (NE.reverse $ bound 0 :| fmap free xs)
  return $ lam name body


-- Cps Transformation --

cpsTransform :: Expr a -> Expr a -> State Int (Expr a)
cpsTransform k v@Var{}                      = pure $ App k (singleton v)
cpsTransform k c@Const{}                    = pure $ App k (singleton c)
cpsTransform k e@(Err s                   ) = pure e -- not sure
cpsTransform k (  CApp   c    xs          ) = cpsCApp c [] xs k
cpsTransform k (  Lambda args (Scope body)) = do
  name <- fresh
  let var = bound $ NE.length args
  body' <- cpsTransform var body
  return $ app k (Lambda (append args name) (Scope body'))
cpsTransform k (App f args) = do
  fname <- fresh
  let args' = fmap weaken args
  b <- cpsApp (B 0) [] args' (weaken k)
  cpsTransform (lam fname b) f
cpsTransform k (Case e ps) = do
  ps' <- traverse (cpsPattern k) ps
  k'  <- lam <$> fresh <*> pure (Case (bound 0) (fmap weaken ps'))
  cpsTransform k' e

cpsApp :: a -> [a] -> NonEmpty (Expr a) -> Expr a -> State Int (Expr a)
cpsApp fVar vars (e :| []) k = do
  k' <- mkAppK fVar vars k
  cpsTransform k' e
cpsApp fVar vars (e :| ee : es) k = do
  e' <- cpsApp (F fVar) (B 0 : weaken vars) (fmap weaken (ee :| es)) (weaken k)
  k' <- lam <$> fresh <*> pure e'
  cpsTransform k' e

cpsCApp :: Cons -> [a] -> NonEmpty (Expr a) -> Expr a -> State Int (Expr a)
cpsCApp c vars (e :| []) k = do
  k' <- mkCAppK c vars k
  cpsTransform k' e
cpsCApp c vars (e :| ee : es) k = do
  e' <- cpsCApp c (B 0 : weaken vars) (fmap weaken (ee :| es)) (weaken k)
  k' <- lam <$> fresh <*> pure e'
  cpsTransform k' e

cpsPattern :: Expr a -> Pattern Expr a -> State Int (Pattern Expr a)
cpsPattern k (PatConst c e          ) = PatConst c <$> cpsTransform k e
cpsPattern k (PatWild e             ) = PatWild <$> cpsTransform k e
cpsPattern k (PatCons c ns (Scope e)) = do
  e' <- cpsTransform (weaken k) e
  return $ PatCons c ns (Scope e')

cpsTop :: TopLevel a -> State Int (TopLevel a)
cpsTop (DefFun n args (Scope b)) = do
  kname <- fresh
  b'    <- cpsTransform (Var (B (length args))) b
  return $ DefFun n (append args kname) (Scope b')

isTrivial :: Expr a -> Bool
isTrivial Var{}       = True
isTrivial Const{}     = True
isTrivial Lambda{}    = True
isTrivial (CApp _ xs) = all isTrivial xs
isTrivial _           = False

isCps :: Expr a -> Bool
isCps (App f xs) = isTrivial f && all isTrivial xs
isCps e          = isTrivial e
