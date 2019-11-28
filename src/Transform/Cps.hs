{-# LANGUAGE RankNTypes #-}

module Transform.Cps where

import           Control.Monad.State
import           Syntax
import           Bind
import           Eval
import           Data.Bifunctor
import qualified Data.List.NonEmpty            as NE
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.Map                      as Map

singleton :: a -> NonEmpty a
singleton x = x :| []

append :: NonEmpty a -> a -> NonEmpty a
append (x :| xs) y = x :| xs ++ [y]

fresh :: State Int String
fresh = do
  x <- get
  modify' (+ 1)
  return $ "gen-" ++ show x

mkCont :: String -> Expr (Var () a) -> Expr a
mkCont x = Lambda (singleton x) . Scope . fmap (first (const 0))

cpsTransform :: Expr a -> Expr a -> State Int (Expr a)
cpsTransform k v@Var{}                      = pure $ App k (singleton v)
cpsTransform k c@Const{}                    = pure $ App k (singleton c)
cpsTransform k e@(Err s                   ) = pure e -- not sure
cpsTransform k (  Lambda args (Scope body)) = do
  name <- fresh
  let var = Var . B $ NE.length args
  body' <- cpsTransform var body
  return $ App k (singleton $ Lambda (append args name) (Scope body'))
cpsTransform k (App f args) = do
  fname <- fresh
  names <- traverse (const fresh) args
  let args' = NE.zip names $ fmap weaken args
  b <- buildArgs (B 0) [] args' (weaken k)
  cpsTransform (Lambda (singleton fname) (Scope b)) f
cpsTransform k (CApp c xs) = do
  names <- traverse (const fresh) xs
  let args' = NE.zip names $ xs
  buildArgs' c [] args' k
cpsTransform k (Case e ps) = do
  fname <- fresh
  ps'   <- traverse (cpsPattern k) ps
  let k' =
        Lambda (singleton fname) (Scope (Case (Var (B 0)) (fmap weaken ps')))
  cpsTransform k' e

buildArgs
  :: a -> [a] -> NonEmpty (String, Expr a) -> Expr a -> State Int (Expr a)
buildArgs fVar vars ((name, e) :| []) k =
  let k' = Lambda
        (singleton name)
        (Scope $ App
          (Var . F $ fVar)
          (NE.reverse (weaken k :| Var (B 0) : (fmap (Var . F)) vars))
        )
  in  cpsTransform k' e
buildArgs fVar vars ((name, e) :| ee : es) k = do
  e' <- buildArgs (F fVar)
                  (B 0 : weaken vars)
                  (fmap (second weaken) (ee :| es))
                  (weaken k)
  cpsTransform (Lambda (singleton name) $ Scope e') e

buildArgs'
  :: Cons -> [a] -> NonEmpty (String, Expr a) -> Expr a -> State Int (Expr a)
buildArgs' c vars ((name, e) :| []) k =
  let k' = Lambda
        (singleton name)
        (Scope $ App
          (weaken k)
          (singleton $ CApp c (NE.reverse (Var (B 0) :| (fmap (Var . F)) vars)))
        )
  in  cpsTransform k' e
buildArgs' c vars ((name, e) :| ee : es) k = do
  e' <- buildArgs' c
                   (B 0 : weaken vars)
                   (fmap (second weaken) (ee :| es))
                   (weaken k)
  cpsTransform (Lambda (singleton name) $ Scope e') e

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

cpsEnv :: Env -> Env
cpsEnv = fmap f
 where
  f (PrimOp p) = PrimOp
    (\xs ->
      let ys                    = NE.fromList $ NE.init xs
          Closure (k :| []) b e = NE.last xs
      in  (eval (Map.insert k (p ys) e) b id)
    )

isTrivial :: Expr a -> Bool
isTrivial Var{}       = True
isTrivial Const{}     = True
isTrivial Lambda{}    = True
isTrivial (CApp _ xs) = all isTrivial xs
isTrivial _           = False

isCps :: Expr a -> Bool
isCps (App f xs) = isTrivial f && all isTrivial xs
isCps e          = isTrivial e

mapPattern :: (forall a . f a -> f a) -> Pattern f a -> Pattern f a
mapPattern f (PatWild e             ) = PatWild (f e)
mapPattern f (PatConst c e          ) = PatConst c (f e)
mapPattern f (PatCons t ns (Scope e)) = PatCons t ns (Scope (f e))

simplify :: Expr a -> Expr a
simplify (App (Lambda _ body) args) = simplify $ instantiate (args NE.!!) body
simplify (App f args) = App (simplify f) (fmap simplify args)
simplify (Case e ps) = Case e (fmap (mapPattern simplify) ps)
simplify (Lambda ns (Scope e)) = Lambda ns (Scope (simplify e))
simplify e = e

simplifyTop :: TopLevel a -> TopLevel a
simplifyTop (DefFun name args (Scope e)) =
  DefFun name args (Scope $ simplify e)
