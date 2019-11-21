{-# LANGUAGE LambdaCase #-}

module Syntax where

import           Control.Monad.State
import           Debug.Trace
import qualified Data.List.NonEmpty            as NE
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Maybe

data Expr a
  = Lambda (NonEmpty a) (Expr a)
  | App (Expr a) (NonEmpty (Expr a))
  | Case (Expr a) [CaseExpr a] (Maybe (Expr a))
  | AppConstructor String [Expr a]
  | Var a
  | Const Const
  | Err String
  deriving Show

data CaseExpr a = CaseExpr String [a] (Expr a)
  deriving Show

data TopLevel a
  = DefFun a (NonEmpty a) (Expr a)
  deriving Show
  -- | DefData String (NonEmpty DefConstructor)

-- data DefConstructor = DefConstructor String [String]

data Const = IConst Int | SConst String
  deriving Show

type Env = Map String Value

data Value
  = Closure (NonEmpty String) (Expr String) Env
  | I Int
  | S String
  | Constructor String [Value]

instance Show Value where
  show (Closure args body _  ) = "<closure>"
  show (I x                  ) = show x
  show (S s                  ) = show s
  show (Constructor name vals) = "(" ++ name ++ " " ++ show vals ++ ")"

type Cont = Value -> Value

nonEmptyToSet :: NonEmpty String -> Set String
nonEmptyToSet = Set.fromList . NE.toList

freeVars :: Ord a => Expr a -> Set a
freeVars (Lambda bound expr) =
  Set.difference (freeVars expr) (Set.fromList $ NE.toList bound)
freeVars (Case expr branches def) =
  freeVars expr
    `Set.union` foldMap freeVarsCase branches
    `Set.union` maybe Set.empty freeVars def
freeVars (App f exprs) = freeVars f `Set.union` foldMap freeVars exprs
freeVars (Var v      ) = Set.singleton v
freeVars _             = Set.empty

freeVarsCase :: Ord a => CaseExpr a -> Set a
freeVarsCase (CaseExpr _ bound expr) =
  freeVars expr `Set.difference` Set.fromList bound

eval :: Env -> Expr String -> Cont -> Value
eval env expr k = trace ("eval " ++ show expr) $ case expr of
  Lambda bound body -> k $ Closure bound body (Map.restrictKeys env fvs)
  App    f     args -> eval env f $ \case
    Closure params body boundEnv ->
      trace "closure cont" $ if length params == length args
        then evalArgs
          env
          (NE.toList args)
          []
          (\vs ->
            let env' = Map.union (Map.fromList $ NE.toList params `zip` vs)
                                 boundEnv
            in  eval env' body k
          )
        else error "Wrong argument count"
  AppConstructor name args ->
    evalArgs env args [] $ \vs -> k $ Constructor name vs
  Case e branches def -> eval env e (\v -> evalCase env branches def v k)
  Var   v             -> k $ env Map.! v
  Const (IConst i)    -> k $ I i
  Const (SConst s)    -> k $ S s
  Err   msg           -> error msg
  where fvs = freeVars expr

evalArgs :: Env -> [Expr String] -> [Value] -> ([Value] -> Value) -> Value
evalArgs env [] vals k = k $ reverse vals
evalArgs env (e : exprs) vals k =
  eval env e (\v -> evalArgs env exprs (v : vals) k)

evalCase
  :: Env -> [CaseExpr String] -> Maybe (Expr String) -> Value -> Cont -> Value
evalCase env [] Nothing  v k = error "No branch in case"
evalCase env [] (Just e) _ k = eval env e k
evalCase env ((CaseExpr name bound e) : es) def (Constructor cname vals) k
  | name == cname = if length bound == length vals
    then
      let env' = Map.fromList (zip bound vals) `Map.union` env in eval env' e k
    else error $ "Wrong variable count: " ++ show (length bound)
evalCase env (_ : es) def v k = evalCase env es def v k

buildEnv :: [TopLevel String] -> Env
buildEnv ts = let e = aux e Map.empty ts in e
 where
  aux fin e [] = e
  aux fin e ((DefFun name args body) : es) =
    aux fin (Map.insert name (Closure args body fin) e) es


getVar :: State Int Int
getVar = do
  x <- get
  modify' (+ 1)
  return x


mkCont :: Int -> Expr (Either String Int) -> Expr (Either String Int)
mkCont x = Lambda (Right x :| [])

cpsTransform
  :: Expr (Either String Int)
  -> Expr String
  -> State Int (Expr (Either String Int))
cpsTransform k (Var   s         ) = pure $ App k (Var (Left s) :| [])
cpsTransform k (Const c         ) = pure $ App k (Const c :| [])
cpsTransform k (Err   s         ) = pure $ App k (Err s :| [])
cpsTransform k (Lambda args body) = do
  x     <- getVar
  body' <- cpsTransform (Var $ Right x) body
  return $ App k (Lambda (fmap Left args <> (Right x :| [])) body' :| [])
cpsTranform k (App f args) = do
  x    <- getVar
  vars <- traverse (const getVar) args
  let fin = App (Var $ Right x) (fmap (Var . Right) vars <> (k :| []))
  k' <- buildArgs fin (NE.toList $ NE.zip vars args)
  cpsTransform k' f
 where
  buildArgs
    :: Expr (Either String Int)
    -> [(Int, Expr String)]
    -> State Int (Expr (Either String Int))
  buildArgs fin []            = return fin
  buildArgs fin ((x, e) : es) = do
    k <- mkCont x <$> buildArgs fin es
    cpsTransform k e
