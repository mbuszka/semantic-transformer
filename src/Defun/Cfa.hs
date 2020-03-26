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

copy :: Effs r => ValuePtr -> CacheDst -> Sem r ()
copy (ValuePtr src) (CacheDst dst) = do
  Store vals <- get @(Store Value)
  let vals' = Map.insertWith (<>) dst (vals Map.! src) vals
  put (Store vals')

insertK :: Member (State (Store Cont)) r => Label -> Cont -> Sem r ContPtr
insertK lbl k = insert lbl k >> pure (ContPtr lbl)

insertV :: Member (State (Store Value)) r => Label -> Value -> Sem r ValuePtr
insertV lbl v = insert lbl v >> pure (ValuePtr lbl)

eval :: Effs r => Env -> Label -> ContPtr -> Sem r Config
eval env lbl k = term lbl >>= \case
  App f es -> do
    k' <- insertK f (EvalApp env [] es (CacheDst f) k)
    pure $ Eval env f k'
  Abs (Scope xs body) -> do
    ptr <- insertV lbl (Closure lbl env (fmap fst xs) body)
    pure $ Continue ptr k
  Var x -> do
    ptr <- case env Map.!? x of
      Just v -> do
        copy v (CacheDst lbl)
        pure $ ValuePtr lbl
      Nothing -> asks (Map.member x) >>= \b ->
        if b
          then insertV lbl (TopLevel x)
          else error $ "Unknown variable: " <> pshow x <> " at " <> pshow lbl
    pure $ Continue ptr k
  Cons (Record c []) -> do
    ptr <- insertV lbl (Struct (Record c []))
    pure $ Continue ptr k
  Cons (Record c (e : es)) -> do
    k' <- insertK e (EvalCons lbl c env [] es (CacheDst e) k)
    pure $ Eval env e k'
  Case e ps -> do
    k' <- insertK e (EvalCase env ps (CacheDst e) k)
    pure $ Eval env e k'
  Panic -> empty
  Let {} -> error "Not implemented yet"

continue :: Effs r => ValuePtr -> ContPtr -> Sem r Config
continue vPtr kPtr = do
  derefK kPtr >>= \case
    EvalApp env vs es dst k -> do
      when (null vs) (copy vPtr dst)
      case es of
        e : es' -> do
          k' <- insertK e $ EvalApp env (vPtr : vs) es' (CacheDst e) k
          pure $ Eval env e k'
        [] -> do
          let f : as = reverse (vPtr : vs)
          apply f as k
    Halt -> empty
    EvalCons lbl c env vs es _dst k -> do
      -- copy vPtr dst
      case es of
        e : es' -> do
          k' <- insertK e $ EvalCons lbl c env (vPtr : vs) es' (CacheDst e) k
          pure $ Eval env e k'
        [] -> do
          cPtr <- insertV lbl $ Struct (Record c (reverse $ vPtr : vs))
          pure $ Continue cPtr k
    EvalCase env ps _dst k -> do
      -- copy vPtr dst
      applyCases env vPtr ps k

applyCases :: Effs r => Env -> ValuePtr -> Patterns Label -> ContPtr -> Sem r Config
applyCases environment vPtr (Patterns patterns) k =
  let aux env (PVar x : ps) (v : vs) = aux (Map.insert x v env) ps vs
      aux env (PWild : ps) (_ : vs) = aux env ps vs
      aux env (PCons (Record c ps') : ps) (v : vs) = derefV v >>= \case
        Struct (Record c' vs') -> do
          guard (c' == c)
          guard (length ps' == length vs')
          aux env (ps' <> ps) (vs' <> vs)
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
  pure (vStore', kStore', Set.fromList cs)

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
        if s == s'
          then pure s
          else go s' (n + 1)
      Def _ (Scope _ main) = programMain
      conf = Set.singleton (Eval abstractInitEnv main (ContPtr main))
  (Store vStore, _, _) <-
    runReader (fmap defScope (programDefinitions))
      . runReader abstractTerms
      $ go (abstractDataStore, abstractContStore, conf) (0 :: Int)
  pprint' $ pmap (fmap toList vStore)
  pure (abstract, format vStore)
