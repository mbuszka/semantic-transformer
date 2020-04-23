module Defun.Cfa
  ( Res (..),
    analyse,
  )
where

import Control.Applicative ((<|>), empty)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Defun.Labeled
import Polysemy.Error
import Polysemy.NonDet
import Polysemy.Reader
import Polysemy.State
import qualified Syntax as Stx
import Syntax.Term
import Util
import Util.Pretty hiding (body)

data Config
  = Eval Env Label ContPtr
  | Continue ValuePtr ContPtr
  deriving (Eq, Ord)

data Res
  = RLambda Label
  | RGlobal Var
  | RPrim Var
  deriving (Eq, Ord)

type Terms = Reader (Map Label Labeled)

type TopLevels = Reader (Map Var (Scope Label))

type Effs r =
  Members
    '[Terms, TopLevels, State (Store Cont), State (Store Value), NonDet, Error Err]
    r

term :: Effs r => Label -> Sem r (TermF Label)
term lbl = fmap (Map.lookup lbl) ask >>= \case
  Nothing -> error "Term not found"
  Just t -> pure t

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

insertK :: Member (State (Store Cont)) r => Label -> Cont -> Sem r ContPtr
insertK lbl k = insert lbl k >> pure (ContPtr lbl)

insertV :: Member (State (Store Value)) r => Label -> Value -> Sem r ValuePtr
insertV lbl v = insert lbl v >> pure (ValuePtr lbl)

lookup :: Effs r => Env -> Var -> Label -> Sem r ValuePtr
lookup env var dst = case env Map.!? var of
  Just v -> copy v dst
  Nothing -> asks (Map.member var) >>= \case
    True -> insertV dst (TopLevel var)
    False -> case Map.lookup var Stx.primOps of
      Nothing -> error $ "Unknown variable: " <> pshow var <> " at " <> pshow dst
      Just _ -> insertV dst (Prim var)

aval :: Effs r => Env -> Label -> Sem r ValuePtr
aval env lbl = term lbl >>= \case
  Var v -> lookup env v lbl
  Abs (Scope xs t) -> insertV lbl $ Closure lbl env (fmap fst xs) t
  Cons (Stx.Boolean _) -> insertV lbl Boolean
  Cons (Stx.Number _) -> insertV lbl Number
  Cons (Stx.String _) -> insertV lbl String
  Cons (Stx.Record c vs) -> insertV lbl . Record c =<< traverse (aval env) vs
  App f vs -> do
    op <- term f >>= \case
      Var op -> pure op
      _ -> error "term not in A-normal form"
    vs' <- traverse (derefV <=< aval env) vs
    insertV lbl =<< prim (Stx.primOps Map.! op) vs'
  _ -> error "term not in A-normal form"

eval :: Effs r => Env -> Label -> ContPtr -> Sem r Config
eval env lbl k = term lbl >>= \case
  App f ts -> do
    f' <- aval env f
    ts' <- traverse (aval env) ts
    apply f' ts' k
  Let t (Scope [(x, _)] body) -> do
    k' <- insertK lbl (CLet x env body k)
    pure $ Eval env t k'
  Let _ _ -> error "Bad syntax"
  Case t ps -> do
    v <- aval env t
    evalCase env v ps k
  Panic -> empty
  _ -> do
    v <- aval env lbl
    pure $ Continue v k

prim :: Effs r => PrimOp -> [Value] -> Sem r Value
prim op vs = case (op, vs) of
  (Add, [Number, Number]) -> pure Number
  (Sub, [Number, Number]) -> pure Number
  (Mul, [Number, Number]) -> pure Number
  (Div, [Number, Number]) -> pure Number
  (Neg, [Number]) -> pure Number
  (And, [Boolean, Boolean]) -> pure Boolean
  (Or, [Boolean, Boolean]) -> pure Boolean
  (Not, [Boolean]) -> pure Boolean
  (Eq, [_, _]) -> pure Boolean
  _ -> empty

continue :: Effs r => ValuePtr -> ContPtr -> Sem r Config
continue val k = derefK k >>= \case
  CLet var env body k' -> pure $ Eval (Map.insert var val env) body k'
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
        (Stx.Number _, Number) -> pure env
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

apply :: Effs r => ValuePtr -> [ValuePtr] -> ContPtr -> Sem r Config
apply f as k = derefV f >>= \case
  Closure _ env xs body ->
    pure $ Eval (Map.fromList (xs `zip` as) `Map.union` env) body k
  TopLevel x -> do
    Scope xs body <- asks (Map.! x)
    pure $ Eval (Map.fromList (fmap fst xs `zip` as)) body k
  _ -> empty

step ::
  Members '[Terms, TopLevels, Error Err] r =>
  (Store Value, Store Cont, Set Config) ->
  Sem r (Store Value, Store Cont, Set Config)
step (vStore, kStore, confs) = do
  (vStore', (kStore', cs)) <-
    runState vStore . runState kStore . runNonDet $ do
      oneOf confs >>= \case
        Eval env e k -> eval env e k
        Continue v k -> continue v k
  pure (vStore', kStore', confs `Set.union` Set.fromList cs)

format :: Map Label (Set Value) -> Map Label (Set Res)
format = Map.fromList . fmap aux . Map.toList
  where
    aux (lbl, vs) = (lbl, Set.fromList . (fmt =<<) . toList $ vs)
    fmt (TopLevel x) = [RGlobal x]
    fmt (Closure l _ _ _) = [RLambda l]
    fmt (Prim p) = [RPrim p]
    fmt _ = []

type EffsA r = Members '[Embed IO, Error Err] r

analyse :: EffsA r => Program Term -> Sem r (Abstract, Map Label (Set Res))
analyse program = do
  abstract@(Abstract {..}) <- fromSource program
  let Program {..} = abstractProgram
  let functions = 
        Map.fromList $ programDefinitions <&> \DefFun {..} -> (funName, funScope)
  let go s n = do
        s' <- step s
        if s == s'
          then pure s
          else go s' (n + 1)
      DefFun {funScope=Scope _ main} = programMain
      conf = Set.singleton (Eval abstractInitEnv main (ContPtr main))
  (Store vStore, _, _) <-
    runReader functions
      . runReader abstractTerms
      $ go (abstractDataStore, abstractContStore, conf) (0 :: Int)
  -- pprint' $ pmap (fmap toList vStore)
  pure (abstract, format vStore)

instance Pretty Config where
  pretty (Eval _ t k) = parens ("eval" <+> pretty t <+> pretty k)
  pretty (Continue v k) = parens ("continue" <+> pretty v <+> pretty k)
