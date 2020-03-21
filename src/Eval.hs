module Eval where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as Map
import Pretty
import Syntax
import Syntax.Term

data Value
  = Struct Tag [Value]
  | Closure Env (Scope Term)
  | TopLevel Var

instance Eq Value where
  Struct c vs == Struct d ws = c == d && vs == ws
  TopLevel v == TopLevel w = v == w
  _ == _ = False

data Cont
  = EvalFun Env [Term]
  | EvalArgs Env Value [Value] [Term]
  | EvalLet Env Var Term
  | EvalCase Env (Patterns Term)
  | EvalCons Env Tag [Value] [Term]

type Env = Map Var Value

type MonadEval m = (MonadReader (Map Var (Scope Term)) m, MonadError Text m)

run :: MonadError Text m => Program Term -> m Value
run (Program defs _) =
  let defs' = Map.fromList $ fmap (\case Def _ x s -> (x, s)) defs
      Scope [] t = defs' Map.! mkVar "main"
   in runReaderT (eval Map.empty t []) defs'

lookup :: MonadEval m => Env -> Var -> m Value
lookup env x = case env Map.!? x of
  Nothing -> do
    tl <- asks (Map.lookup x)
    case tl of
      Nothing -> throwError $ "No binding for: " <> pshow x
      Just _ -> pure $ TopLevel x
  Just v -> pure v

topLevel :: MonadEval m => Var -> m (Scope Term)
topLevel x = do
  defs <- ask
  case defs Map.!? x of
    Nothing -> throwError $ "Unknown top-level function: " <> pshow x
    Just s -> pure s

eval :: MonadEval m => Env -> Term -> [Cont] -> m Value
eval env term cont = case unTerm term of
  Var x -> do
    v <- Eval.lookup env x
    continue v cont
  Abs s -> do
    let clo = Closure (env `Map.restrictKeys` freeVars term) s
    continue clo cont
  App f xs -> eval env f (EvalFun env xs : cont)
  Let t (Scope [x] b) -> eval env t (EvalLet env x b : cont)
  Case t ps -> eval env t (EvalCase env ps : cont)
  Cons (Record c []) -> continue (Struct c []) cont
  Cons (Record c (t : ts)) -> eval env t (EvalCons env c [] ts : cont)
  Let _ _ -> error "Not implemented yet"
  Error -> error "Not implemented yet"

continue :: MonadEval m => Value -> [Cont] -> m Value
continue v cont = case cont of
  EvalFun _ [] : ks ->
    apply v [] ks
  EvalFun env (t : ts) : ks ->
    eval env t $ EvalArgs env v [] ts : ks
  EvalArgs _ f vs [] : ks ->
    apply f (reverse (v : vs)) ks
  EvalArgs env f vs (t : ts) : ks ->
    eval env t $ EvalArgs env f (v : vs) ts : ks
  EvalLet env x t : ks ->
    eval (Map.insert x v env) t ks
  EvalCase env (Patterns ps) : ks ->
    evalCase env v ps ks
  EvalCons _env c vs [] : ks ->
    continue (Struct c (reverse (v : vs))) ks
  EvalCons env c vs (t : ts) : ks ->
    eval env t $ EvalCons env c (v : vs) ts : ks
  [] -> pure v

evalCase ::
  MonadEval m =>
  Env ->
  Value ->
  [(Pattern (), Scope Term)] ->
  [Cont] ->
  m Value
evalCase environment value patterns ks = case patterns of
  [] -> throwError "Non-exhaustive pattern match"
  (p, Scope xs t) : ps -> case aux [p] xs [value] environment of
    Nothing -> evalCase environment value ps ks
    Just env -> eval env t ks
  where
    aux (PVar () : ps) (x : xs) (v : vs) env = aux ps xs vs (Map.insert x v env)
    aux (PWild : ps) xs (_ : vs) env = aux ps xs vs env
    aux (PCons (Record c ps') : ps) xs (Struct c' vs' : vs) env
      | c == c' = aux (ps' <> ps) xs (vs' <> vs) env
      | otherwise = Nothing
    aux [] [] [] env = Just env
    aux [] [] _ _ = Nothing
    aux _ _ _ _ = error "Corrupted pattern"

apply :: MonadEval m => Value -> [Value] -> [Cont] -> m Value
apply f vs ks = do
  (env, Scope xs t) <- case f of
    Closure env s -> pure (env, s)
    TopLevel x -> do
      s <- topLevel x
      pure (Map.empty, s)
    _ -> throwError "Expected closure or top-level function"
  if length xs == length vs
    then eval (Map.fromList (xs `zip` vs) `Map.union` env) t ks
    else throwError "Wrong argument count in application"

instance Pretty Value where
  pretty (Struct c vs) = braces (pretty c <> nested' 2 vs)
  pretty (Closure _ _) = "<closure>"
  pretty (TopLevel x) = "<function:" <+> pretty x <> ">"
