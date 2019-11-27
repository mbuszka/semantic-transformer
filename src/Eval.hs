{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module Eval where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE
import qualified Data.Set                      as Set
import           Data.Foldable
import           Bind
import           Syntax
import           Data.Text.Prettyprint.Doc      ( pretty )

type Env = Map String Value

data Value
  = Closure (NonEmpty String) (Expr String) Env
  | PrimOp (NonEmpty Value -> Value)
  | VConst Const
  | Struct Cons (NonEmpty Value)

instance Show Value where
  show (Closure args body _  ) = "<closure>"
  show (VConst c) = show c
  show (Struct (MkCons name) vals) = "(" ++ show name ++ " " ++ show vals ++ ")"

type Cont = Value -> Value

eval :: Env -> Expr String -> Cont -> Value
eval env expr k = {- trace ("eval " ++ show expr) $ -}
                  case expr of
  Lambda bound body ->
    k $ Closure bound (unbind bound body) (Map.restrictKeys env fvs)
  App f args -> eval env f $ \case
    Closure params body boundEnv -> if length params == length args
      then evalArgs
        env
        args
        []
        (\vs ->
          let env' =
                  Map.union (Map.fromList . NE.toList $ params `NE.zip` vs)boundEnv
          in  eval env' body k
        )
      else error $ "Wrong argument count\n" ++ show (pretty expr)
    PrimOp op -> evalArgs env args [] (k . op)
    VConst (Cons tag) ->
      evalArgs env args [] (k . Struct tag)
    other -> error $ show other
  Case e ps -> eval env e \v -> evalCase env v ps k
  Var   v          -> k $ env Map.! v
  Const c -> k $ VConst c
  Err   msg        -> error msg
  where fvs = Set.fromList $ toList expr

evalArgs :: Env -> NonEmpty (Expr String) -> [Value] -> (NonEmpty Value -> Value) -> Value
evalArgs env (e :| []) vals k =
  eval env e (\v -> k . NE.reverse $ v :| vals)
evalArgs env (e :| e' : exprs) vals k =
  eval env e (\v -> evalArgs env (e :| exprs) (v : vals) k)

evalCase
  :: Env -> Value -> [Pattern Expr String] -> Cont -> Value
evalCase env _ [] k = error "No branch in case"
evalCase env (VConst a) (PatConst b e:ps) k | a == b = eval env e k
evalCase env (Struct t vs) (PatCons t' ns b : es) k | t == t' =
  if length vs == length ns then
      let env' = (Map.fromList . toList) (NE.zip ns vs) `Map.union` env
          e = unbind ns b
      in eval env' e k
  else error $ "Wrong variable count: " ++ show (length ns)
evalCase env _ (PatWild e : ps) k = eval env e k

initial :: Env
initial = Map.fromList
  [ ("add", PrimOp $
    \case VConst (Int a) :| [VConst (Int b)] -> VConst . Int $ a + b)
  , ("sub", PrimOp $
    \case VConst (Int a) :| [VConst (Int b)] -> VConst . Int $ a - b)
  ]

buildEnv :: Env -> [TopLevel String] -> Env
buildEnv initial ts = let e = aux e initial ts in e
 where
  aux fin e [] = e
  aux fin e ((DefFun name args body) : es) =
    aux fin (Map.insert name (Closure args (unbind args body) fin) e) es
