module Eval
  ( tests,
  )
where

import qualified Data.Map as Map
import Polysemy.Error
import Polysemy.Reader
import Pretty
import Syntax hiding (Value (..))
import qualified Syntax as Stx
import Util

data Value
  = Struct (Record Value)
  | Closure Env (Scope Term)
  | TopLevel Var

instance Eq Value where
  Struct a == Struct b = a == b
  TopLevel v == TopLevel w = v == w
  _ == _ = False

data Cont
  = EvalFun Env [Term]
  | EvalArgs Env Value [Value] [Term]
  | EvalLet Env Var Term
  | EvalCase Env (Patterns Term)
  | EvalCons Env Tag [Value] [Term]

type Env = Map Var Value

type Effs r = Members '[Error Err, Reader (Map Var (Scope Term))] r

inject :: Stx.Value -> Value
inject (Stx.Struct r) = Struct (fmap inject r)
inject _ = error "Not implemented yet"

zip' :: [a] -> [b] -> Maybe [(a, b)]
zip' [] [] = Just []
zip' (a : as) (b : bs) = fmap ((a, b) :) (zip' as bs)
zip' _ _ = Nothing

data TestResult
  = RuntimeError Err
  | MalformedTestCase Int Int
  | Success
  | Failure Stx.Value Value

tests :: Program Term -> [(Text, TestResult)]
tests Program {..} =
  let defs = fmap (\case Def _ s -> s) programDefinitions
      Def _ (Scope xs main) = programMain
      evalTest (TestCase name inputs output) =
        ( name,
          case fmap fst xs `zip'` fmap inject inputs of
            Nothing -> MalformedTestCase (length xs) (length inputs)
            Just init ->
              let c = eval (Map.fromList init) main []
               in case run . runError . runReader defs $ c of
                    Left err -> RuntimeError err
                    Right v ->
                      if v == inject output
                        then Success
                        else Failure output v
        )
   in fmap evalTest programTests

lookup :: Effs r => Env -> Var -> Sem r Value
lookup env x = case env Map.!? x of
  Nothing -> do
    tl <- asks (Map.lookup x)
    case tl of
      Nothing -> throw . EvalError $ "No binding for: " <> pshow x
      Just _ -> pure $ TopLevel x
  Just v -> pure v

topLevel :: Effs r => Var -> Sem r (Scope Term)
topLevel x = do
  defs <- ask
  case defs Map.!? x of
    Nothing -> throw . EvalError $ "Unknown top-level function: " <> pshow x
    Just s -> pure s

eval :: Effs r => Env -> Term -> [Cont] -> Sem r Value
eval env term cont = case unTerm term of
  Var x -> do
    v <- Eval.lookup env x
    continue v cont
  Abs s -> do
    let clo = Closure (env `Map.restrictKeys` freeVars term) s
    continue clo cont
  App f xs -> eval env f (EvalFun env xs : cont)
  Let t (Scope [(x, _)] b) -> eval env t (EvalLet env x b : cont)
  Case t ps -> eval env t (EvalCase env ps : cont)
  Cons (Record c []) -> continue (Struct (Record c [])) cont
  Cons (Record c (t : ts)) -> eval env t (EvalCons env c [] ts : cont)
  Let _ _ -> error "Not implemented yet"
  Panic -> error "Not implemented yet"

continue :: Effs r => Value -> [Cont] -> Sem r Value
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
    continue (Struct (Record c (reverse (v : vs)))) ks
  EvalCons env c vs (t : ts) : ks ->
    eval env t $ EvalCons env c (v : vs) ts : ks
  [] -> pure v

evalCase ::
  Effs r =>
  Env ->
  Value ->
  [(Pattern (), Scope Term)] ->
  [Cont] ->
  Sem r Value
evalCase environment value patterns ks = case patterns of
  [] -> throw . EvalError $ "Non-exhaustive pattern match"
  (p, Scope xs t) : ps -> case aux [p] (fmap fst xs) [value] environment of
    Nothing -> evalCase environment value ps ks
    Just env -> eval env t ks
  where
    aux (PVar () : ps) (x : xs) (v : vs) env = aux ps xs vs (Map.insert x v env)
    aux (PWild : ps) xs (_ : vs) env = aux ps xs vs env
    aux (PCons (Record c ps') : ps) xs (Struct (Record c' vs') : vs) env
      | c == c' = aux (ps' <> ps) xs (vs' <> vs) env
      | otherwise = Nothing
    aux [] [] [] env = Just env
    aux [] [] _ _ = Nothing
    aux _ _ _ _ = error "Corrupted pattern"

apply :: Effs r => Value -> [Value] -> [Cont] -> Sem r Value
apply f vs ks = do
  (env, Scope xs t) <- case f of
    Closure env s -> pure (env, s)
    TopLevel x -> do
      s <- topLevel x
      pure (Map.empty, s)
    _ -> throw . EvalError $ "Expected closure or top-level function"
  new <-
    note
      (EvalError "Wrong argument count in application")
      (fmap fst xs `zip'` vs)
  eval (Map.fromList new `Map.union` env) t ks

instance Pretty Value where
  pretty (Struct r) = pretty r
  pretty (Closure _ _) = "<closure>"
  pretty (TopLevel x) = "<function:" <+> pretty x <> ">"

instance Pretty TestResult where
  pretty (Failure expected got) = "Failure:" <> nested 2 (rows values)
    where
      values =
        [ "Expected:" <> nested 2 (pretty expected),
          "Got" <> nested 2 (pretty got)
        ]
  pretty Success = "Success"
  pretty (RuntimeError err) = "Runtime error:" <> nested 2 (pretty err)
  pretty (MalformedTestCase expected got) =
    "Malformed test case: expected"
      <> pretty expected <+> "arguments, got" <+> pretty got
