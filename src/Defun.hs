{-# LANGUAGE TupleSections #-}
module Defun where

import Control.Lens
import Control.Monad.Except
import qualified Data.Map as Map
import qualified Data.Set as Set
import Syntax

data Value
  = Struct Text [Addr]
  | Closure Env (Scope Label)
  | TopLevel Var
  deriving (Eq, Ord)

newtype Addr = Addr Label deriving (Eq, Ord)

type Env = Map Var Addr

data Cont
  = EvalFun [Label] Label
  | EvalArgs Value [Value] [Term] Label
  | EvalLet Var Label Label
  | EvalCase (Patterns Label) Label
  | EvalCons Text [Value] [Label] Label

newtype Store k v = Store {_store :: Map k (Set v)}

$(makeLenses ''Store)

instance (Ord k, Ord v) => Semigroup (Store k v) where
  Store l <> Store r = Store $ Map.unionWith (<>) l r

instance (Ord k, Ord v) => Monoid (Store k v) where
  mempty = Store Map.empty

type KStore = Store Label Cont

type VStore = Store Var Value

data Static
  = Static
      { _topLevel :: Map Var (Scope Label),
        _term :: Map Label (TermF Label)
      }

$(makeLenses ''Static)

type MonadEval m = (MonadState Static m, MonadError Text m)

denormalise :: MonadEval m => Term -> m Label
denormalise t = do
  t' <- traverse denormalise (unTerm t)
  term %= Map.insert (label t) t'
  pure $ label t

initialise :: MonadEval m => Program Term -> m Label
initialise (Program []) = do
  m <- use $ topLevel . at (mkVar "main")
  case m of
    Nothing -> throwError "main not found"
    Just (Scope [] l) -> pure l
    _ -> throwError "main should accept no arguments"
initialise (Program (Def _ x s : ds)) = do
  s' <- traverse denormalise s
  topLevel %= Map.insert x s'
  initialise (Program ds)

lookupVar :: MonadEval m => VStore -> Var -> m [Value]
lookupVar (Store m) k = case Map.lookup k m of
  Nothing -> do
    tl <- use $ topLevel . at k
    case tl of
      Nothing -> throwError $ "No binding for: " <> pshow k
      Just _ -> pure [TopLevel k]
  Just vs -> pure $ toList vs

lookupCont :: MonadEval m => KStore -> Label -> m [Cont]
lookupCont (Store m) k = case Map.lookup k m of
  Nothing -> throwError $ "No continuation for address " <> pshow k
  Just ks -> pure $ toList ks

data TermNotFound = TermNotFound deriving Show
instance Exception TermNotFound

lookupTerm :: MonadEval m => Label -> m (TermF Label)
lookupTerm l = do
  ml <- use $ term . at l
  case ml of
    Nothing -> bug TermNotFound
    Just t -> pure t

insert :: (Ord k) => k -> v -> Store k v -> Store k v
insert k v (Store m) = Store $ Map.insert k v m

data Conf 
  = Eval Label Label
  | Continue Value Label

type MState = (VStore, KStore, Conf)

eval :: MonadEval m => VStore -> KStore -> Label -> Label -> m [MState]
eval vStore kStore l k = do
  t <- lookupTerm l
  case t of
    Var v -> map (\v -> (vStore, kStore, Continue v k)) <$> lookupVar vStore v
    Abs s -> pure [(vStore, kStore, Continue (Closure s) k)]
    App f xs ->
      let k' = EvalFun xs k
       in pure [(vStore, insert l k' kStore, Eval f l)]
    Let t (Scope [x] t') ->
      let k' = EvalLet x t' k
       in pure [(vStore, insert l k' kStore, Eval t l)]
    Case t ps ->
      let k' = EvalCase ps k
       in pure [(vStore, insert l k' kStore, Eval t l)]
    Cons c (t:ts) ->
      let k' = EvalCons c [] ts k
       in pure [(vStore, insert l k' kStore, Eval t l)]
    Cons c [] -> pure [(vStore, kStore, Continue (Struct c []) k)]

continue :: MonadEval m => VStore -> KStore -> Value -> Label -> m [MState]
continue vStore kStore v k = do
  ks <- lookupCont kStore k
  ks >>= \case
    EvalFun [] k -> apply v [] k