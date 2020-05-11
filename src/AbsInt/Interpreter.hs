module AbsInt.Interpreter (run) where

import Control.Applicative ((<|>), empty)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Polysemy.Error
import Polysemy.NonDet
import Polysemy.State
import Syntax hiding (ValueF (..), term)
import qualified Syntax as Stx
import Util
import AbsInt.Types

type Effs r = (Common r, VStore r, KStore r, Member NonDet r)

oneOf :: (Foldable t, Member NonDet r) => t a -> Sem r a
oneOf = foldr (<|>) empty . fmap pure . toList

derefK :: Effs r => ContPtr -> Sem r Cont
derefK (ContPtr p) = gets (Map.lookup p . unStore) >>= \case
  Just vs -> oneOf vs
  Nothing -> throw $ InternalError $ "Cont not found: " <> pshow p

derefV :: Effs r => ValuePtr -> Sem r Value
derefV (ValuePtr p) = gets (Map.lookup p . unStore) >>= \case
  Just vs -> oneOf vs
  Nothing -> throw $ InternalError $ "Value not found: " <> pshow p

insertK :: Effs r => Label -> Cont -> Sem r ContPtr
insertK lbl k = insert lbl k >> pure (ContPtr lbl)

insertV :: Effs r => Label -> Value -> Sem r ValuePtr
insertV lbl v = insert lbl v >> pure (ValuePtr lbl)

lookup :: Effs r => Env -> Var -> Label -> Sem r ValuePtr
lookup env var dst = case env Map.!? var of
  Just v -> copy v dst
  Nothing -> gets (Map.member var . absIntGlobals) >>= \case
    True -> insertV dst (Global var)
    False -> case Map.lookup var Stx.primOps of
      Nothing -> error $ "Unknown variable: " <> pshow var <> " at " <> pshow dst
      Just op -> insertV dst (PrimOp op)

lookupWith :: (Member (Error Err) r, Ord k) => k -> Map k v -> Err -> Sem r v
lookupWith k map err = case Map.lookup k map of
  Nothing -> throw err
  Just v -> pure v

aval :: Effs r => Env -> Label -> Sem r ValuePtr
aval env lbl = term lbl >>= \case
  Var v -> lookup env v lbl
  Abs _ _ -> insertV lbl $ Closure env lbl
  Cons (Stx.Boolean _) -> insertV lbl Boolean
  Cons (Stx.Number _) -> insertV lbl Integer
  Cons (Stx.String _) -> insertV lbl String
  Cons (Stx.Record c vs) -> insertV lbl . Record c =<< traverse (aval env) vs
  -- App f vs -> do
  --   f' <- aval env f >>= derefV
  --   vs' <- traverse (derefV <=< aval env) vs
  --   o <- lookupWith op Stx.primOps $ InternalError $ "Not an operator: " <> pshow op
  --   insertV lbl =<< prim o vs'
  _ -> error "term not in A-normal form"

eval :: Effs r => Env -> Label -> ContPtr -> Sem r Config
eval env lbl k = term lbl >>= \case
  App f ts -> do
    f' <- aval env f
    ts' <- traverse (aval env) ts
    apply lbl f' ts' k
  Let _ x t body -> do
    k' <- insertK lbl (CLet env x body k)
    pure $ Eval env t k'
  Case t ps -> do
    v <- aval env t
    evalCase env v ps k
  Panic -> empty
  _ -> do
    v <- aval env lbl
    pure $ Continue v k

prim :: Effs r => PrimOp -> [Value] -> Sem r Value
prim op vs = case (op, vs) of
  (Add, [Integer, Integer]) -> pure Integer
  (Sub, [Integer, Integer]) -> pure Integer
  (Mul, [Integer, Integer]) -> pure Integer
  (Div, [Integer, Integer]) -> pure Integer
  (Neg, [Integer]) -> pure Integer
  (And, [Boolean, Boolean]) -> pure Boolean
  (Or, [Boolean, Boolean]) -> pure Boolean
  (Not, [Boolean]) -> pure Boolean
  (Eq, [_, _]) -> pure Boolean
  _ -> empty

continue :: Effs r => ValuePtr -> ContPtr -> Sem r Config
continue val k = derefK k >>= \case
  CLet env pattern body k' -> do
    env' <- match env pattern val
    pure $ Eval env' body k'
  Halt -> empty

evalCase :: Effs r => Env -> ValuePtr -> Patterns Label -> ContPtr -> Sem r Config
evalCase env vPtr (Patterns ps) k = do
  (p, Scope xs l) <- oneOf ps
  env' <- match env (insertNames p (fmap fst xs)) vPtr
  pure $ Eval env' l k

match :: Effs r => Env -> Pattern Var -> ValuePtr -> Sem r Env
match env p vPtr = do
  case p of
    PVar x -> pure $ Map.insert x vPtr env
    PWild -> pure env
    PType tp x -> do
      v <- derefV vPtr
      if Map.lookup tp builtinTypes == Just v
        then pure $ Map.insert x vPtr env
        else empty
    PCons c -> do
      v <- derefV vPtr
      case (c, v) of
        (Stx.Number _, Integer) -> pure env
        (Stx.String _, String) -> pure env
        (Stx.Boolean _, Boolean) -> pure env
        (Stx.Record l ps, Record r vs) | l == r -> matchAll env ps vs
        _ -> empty

matchAll :: Effs r => Env -> [Pattern Var] -> [ValuePtr] -> Sem r Env
matchAll env [] [] = pure env
matchAll env (p : ps) (v : vs) = do
  env' <- match env p v
  matchAll env' ps vs
matchAll _ _ _ = empty

apply :: Effs r => Label -> ValuePtr -> [ValuePtr] -> ContPtr -> Sem r Config
apply l f as k = derefV f >>= \case
  Closure env lbl -> term lbl >>= \case
    Abs _ s ->
      pure $ Eval (Map.fromList (scopeVars s `zip` as) <> env) (scopeBody s) k
    t -> throw $ InternalError $ "Expected a lambda"
  Global x -> do
    Scope xs body <- gets ((Map.! x) . absIntGlobals)
    pure $ Eval (Map.fromList (fmap fst xs `zip` as)) body k
  PrimOp op -> do
    vs <- traverse derefV as
    ptr <- insertV l =<< prim op vs
    pure $ Continue ptr k
  _ -> empty

run :: Common r => (Store Value, Store Cont, Set Config) -> Sem r (Store Value)
run c@(vStore, kStore, configs) = do
  (vStore', (kStore', cs)) <-
    runState vStore . runState kStore . runNonDet $ do
      oneOf configs >>= \case
        Eval env e k -> eval env e k
        Continue v k -> continue v k
  let c' = (vStore', kStore', configs <> Set.fromList cs)
  if c == c' then pure vStore else run c'
  