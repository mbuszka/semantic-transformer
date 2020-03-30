{-# LANGUAGE PatternSynonyms #-}

module Eval
  ( tests,
  )
where

import qualified Data.Map as Map
import Polysemy.Error
import Polysemy.Reader
import Pretty
import Syntax hiding (Value (..), ValueF (..))
import qualified Syntax as Stx
import Util

data Value
  = Closure Env (Scope Term)
  | TopLevel Var
  | Value (Stx.ValueF Value)

pattern Number :: Int -> Value
pattern Number n = Value (Stx.Number n)

pattern String :: Text -> Value
pattern String s = Value (Stx.String s)

pattern Boolean :: Bool -> Value
pattern Boolean b = Value (Stx.Boolean b)

pattern Struct :: Tag -> [Value] -> Value
pattern Struct c s = Value (Stx.Record c s)

instance Eq Value where
  Value l == Value r = l == r
  TopLevel v == TopLevel w = v == w
  _ == _ = False

data Cont = CLet Var Env Term

type Env = Map Var Value

type Effs r = Members '[Embed IO, Error Err, Reader (Map Var (Scope Term))] r

inject :: Stx.Value -> Value
inject = Value . fmap inject . Stx.unValue

zip' :: [a] -> [b] -> Maybe [(a, b)]
zip' [] [] = Just []
zip' (a : as) (b : bs) = fmap ((a, b) :) (zip' as bs)
zip' _ _ = Nothing

data TestResult
  = RuntimeError Err
  | MalformedTestCase Int Int
  | Success
  | Failure Stx.Value Value

tests :: Member (Embed IO) r => Program Term -> Sem r [(Text, TestResult)]
tests Program {..} =
  let defs = fmap (\case Def _ s -> s) programDefinitions
      Def _ (Scope xs main) = programMain
      evalTest (TestCase name inputs output) =
        (name,)
          <$> case fmap fst xs `zip'` fmap inject inputs of
            Nothing -> pure $ MalformedTestCase (length xs) (length inputs)
            Just init -> do
              c <- runError . runReader defs $ eval (Map.fromList init) main []
              case c of
                Left err -> pure $ RuntimeError err
                Right v ->
                  if v == inject output
                    then pure Success
                    else pure $ Failure output v
   in traverse evalTest programTests

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

aval :: Effs r => Env -> Term -> Sem r Value
aval env term = case unTerm term of
  Var x -> lookup env x
  Abs s -> pure $ Closure (env `Map.restrictKeys` freeVars term) s
  Cons v -> Value <$> traverse (aval env) v
  Prim op vs -> prim op =<< traverse (aval env) vs
  _ -> throw (EvalError "Term not in A-normal form")

eval :: Effs r => Env -> Term -> [Cont] -> Sem r Value
eval env term cont = case unTerm term of
  App f xs -> do
    f' <- aval env f
    xs' <- traverse (aval env) xs
    apply f' xs' cont
  Let t (Scope [(x, _)] b) -> eval env t (CLet x env b : cont)
  Let _ _ -> throw (EvalError "Let only binds a single variable")
  Case t (Patterns ps) -> do
    v <- aval env t
    evalCase env v ps cont
  Panic -> throw (EvalError "PANIC")
  _ -> aval env term >>= flip continue cont

continue :: Effs r => Value -> [Cont] -> Sem r Value
continue v cont = case cont of
  CLet x env t : ks -> eval (Map.insert x v env) t ks
  [] -> pure v

prim :: Effs r => PrimOp -> [Value] -> Sem r Value
prim op vs = case (op, vs) of
  (Add, [Number l, Number r]) -> pure $ Number (l + r)
  (Add, [String l, String r]) -> pure $ String (l <> r)
  (Sub, [Number l, Number r]) -> pure $ Number (l - r)
  (Mul, [Number l, Number r]) -> pure $ Number (l * r)
  (Div, [Number l, Number r]) -> pure $ Number (l `div` r)
  (And, [Boolean l, Boolean r]) -> pure $ Boolean (l && r)
  (Or, [Boolean l, Boolean r]) -> pure $ Boolean (l || r)
  (Eq, [l, r]) -> pure $ Boolean (l == r)
  (Neg, [Number n]) -> pure $ Number (- n)
  (Not, [Boolean b]) -> pure $ Boolean (not b)
  x -> throw (EvalError $ "Bad prim-op: " <> pshow x)

evalCase ::
  forall r.
  Effs r =>
  Env ->
  Value ->
  [(Pattern (), Scope Term)] ->
  [Cont] ->
  Sem r Value
evalCase environment value patterns ks = case patterns of
  [] -> throw . EvalError $ "Non-exhaustive pattern match"
  (p, Scope xs t) : ps -> aux [p] (fmap fst xs) [value] environment >>= \case
    Nothing -> evalCase environment value ps ks
    Just env -> eval env t ks
  where
    aux :: [Pattern ()] -> [Var] -> [Value] -> Env -> Sem r (Maybe Env)
    aux (PVar () : ps) (x : xs) (v : vs) env = aux ps xs vs (Map.insert x v env)
    aux (PWild : ps) xs (_ : vs) env = aux ps xs vs env
    aux (PCons (Stx.Record c ps') : ps) xs (Struct c' vs' : vs) env
      | c == c' = aux (ps' <> ps) xs (vs' <> vs) env
      | otherwise = pure Nothing
    aux (PCons (Stx.Boolean b) : ps) xs (Boolean b' : vs) env
      | b == b' = aux ps xs vs env
      | otherwise = pure Nothing
    aux (PCons (Stx.Number b) : ps) xs (Number b' : vs) env
      | b == b' = aux ps xs vs env
      | otherwise = pure Nothing
    aux (PCons (Stx.String b) : ps) xs (String b' : vs) env
      | b == b' = aux ps xs vs env
      | otherwise = pure Nothing
    aux [] [] [] env = pure $ Just env
    aux [] [] _ _ = pure Nothing
    aux ps xs vs _ = do
      pprint ps
      pprint xs
      pprint vs
      error "Corrupted pattern"

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
  pretty (Value v) = pretty v
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
