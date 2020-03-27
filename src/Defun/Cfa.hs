module Defun.Cfa
  ( analyse,
  )
where

import Control.Applicative ((<|>), empty)
import Control.Monad (guard)
import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Defun.Labeled
import Polysemy.Error
import Polysemy.NonDet
import Polysemy.Reader
import Polysemy.State
import Pretty hiding (body)
import Util
import qualified Syntax as Stx

data Config
  = Eval Env Label ContPtr
  | Continue ValuePtr ContPtr
  deriving (Eq, Ord)

type Terms = Reader (Map Label Labeled)

type TopLevels = Reader (Map Var (Scope Label))

type Effs r =
  Members
    '[Terms, TopLevels, State (Store Cont), State (Store Value), NonDet]
    r

term :: Effs r => Label -> Sem r (TermF Label)
term lbl = fmap (Map.lookup lbl) ask >>= \case
  Nothing -> error "Term not found"
  Just t -> pure t

oneOf :: (Foldable t, Member NonDet r) => t a -> Sem r a
oneOf = foldr (<|>) empty . fmap pure . toList

derefK :: Effs r => ContPtr -> Sem r Cont
derefK (ContPtr p) = do
  Store xs <- get
  oneOf (xs Map.! p)

derefV :: Effs r => ValuePtr -> Sem r Value
derefV (ValuePtr p) = do
  Store xs <- get
  oneOf (xs Map.! p)

insertK :: Member (State (Store Cont)) r => Label -> Cont -> Sem r ContPtr
insertK lbl k = insert lbl k >> pure (ContPtr lbl)

insertV :: Member (State (Store Value)) r => Label -> Value -> Sem r ValuePtr
insertV lbl v = insert lbl v >> pure (ValuePtr lbl)

copy :: Member (State (Store Value)) r => ValuePtr -> Label -> Sem r ValuePtr
copy (ValuePtr src) dst = do
  (Store values) <- get @(Store Value)
  put . Store . Map.insertWith (<>) dst (values Map.! src) $ values
  pure $ ValuePtr dst

lookup :: Effs r => Env -> Var -> Label -> Sem r ValuePtr
lookup env var dst = case env Map.!? var of
  Just v -> copy v dst
  Nothing -> asks (Map.member var) >>= \case
    True -> insertV dst (TopLevel var)
    False -> error $ "Unknown variable: " <> pshow var <> " at " <> pshow dst

aval :: Effs r => Env -> Label -> Sem r ValuePtr
aval env lbl = term lbl >>= \case
  Var v -> lookup env v lbl
  Abs (Scope xs t) -> insertV lbl $ Closure lbl env (fmap fst xs) t
  Cons (Stx.Boolean _) -> insertV lbl Boolean
  Cons (Stx.Number _) -> insertV lbl Number
  Cons (Stx.String _) -> insertV lbl String
  Cons (Stx.Record c vs) -> insertV lbl . Record c =<< traverse (aval env) vs
  Prim op vs -> insertV lbl =<< prim op =<< traverse (derefV <=< aval env) vs
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
    applyCases env v ps k
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

applyCases :: Effs r => Env -> ValuePtr -> Patterns Label -> ContPtr -> Sem r Config
applyCases environment vPtr (Patterns patterns) k =
  let aux env (PVar x : ps) (v : vs) = aux (Map.insert x v env) ps vs
      aux env (PWild : ps) (_ : vs) = aux env ps vs
      aux env (PCons c : ps) (v : vs) = do
        v' <- derefV v
        case (c, v') of
          (Stx.Record t ps', Record t' vs') -> do
            guard (t' == t)
            guard (length ps' == length vs')
            aux env (ps' <> ps) (vs' <> vs)
          (Stx.Number _, Number) -> aux env ps vs
          (Stx.String _, String) -> aux env ps vs
          (Stx.Boolean _, Boolean) -> aux env ps vs
          _ -> empty
      aux env [] [] = pure env
      aux _ _ _ = empty
   in do
        (p, Scope xs l) <- oneOf patterns
        env <- aux environment [insertNames p (fmap fst xs)] [vPtr]
        pure $ Eval env l k

apply :: Effs r => ValuePtr -> [ValuePtr] -> ContPtr -> Sem r Config
apply f as k = derefV f >>= \case
  Closure _ env xs body ->
    pure $ Eval (Map.fromList (xs `zip` as) `Map.union` env) body k
  TopLevel x -> do
    Scope xs body <- asks (Map.! x)
    pure $ Eval (Map.fromList (fmap fst xs `zip` as)) body k
  _ -> empty

step ::
  Members '[Terms, TopLevels] r =>
  (Store Value, Store Cont, Set Config) ->
  Sem r (Store Value, Store Cont, Set Config)
step (vStore, kStore, confs) = do
  (vStore', (kStore', cs)) <-
    runState vStore . runState kStore . runNonDet $ do
      oneOf confs >>= \case
        Eval env e k -> eval env e k
        Continue v k -> continue v k
  pure (vStore', kStore', confs  `Set.union` Set.fromList cs)

format :: Map Label (Set Value) -> Map Label [Tag]
format = Map.fromList . fmap aux . Map.toList
  where
    aux (lbl, vs) = (lbl, nub . (fmt =<<) . toList $ vs)
    fmt (TopLevel x) = [TopTag x]
    fmt (Closure (Label l) _ _ _) = [GenTag l]
    fmt _ = []

analyse ::
  Members '[Embed IO, Error Err] r => Program Term -> Sem r (Abstract, Map Label [Tag])
analyse program = do
  abstract@(Abstract {..}) <- fromSource program
  let Program {..} = abstractProgram
  let go s n = do
        s' <- step s
        -- let (Store vs, Store ks, cs) = s
        -- let (Store vs', Store ks', cs') = s'
        -- when (n `mod` 1000 == 0) do
        --   pprint' $ "Step: " <> pretty n
        --   pprint $ Store (Map.difference vs' vs)
        --   pprint $ Store (Map.difference ks' ks)
        --   pprint $ toList (Set.difference cs' cs)
        if s == s'
          then pure s
          else go s' (n + 1)
      Def _ (Scope _ main) = programMain
      conf = Set.singleton (Eval abstractInitEnv main (ContPtr main))
  (Store vStore, _, _) <-
    runReader (fmap defScope (programDefinitions))
      . runReader abstractTerms
      $ go (abstractDataStore, abstractContStore, conf) (0 :: Int)
  -- pprint' $ pmap (fmap toList vStore)
  pure (abstract, format vStore)

instance Pretty Config where
  pretty (Eval _ t k) = parens ("eval" <+> pretty t <+> pretty k)
  pretty (Continue v k) = parens ("continue" <+> pretty v <+> pretty k)