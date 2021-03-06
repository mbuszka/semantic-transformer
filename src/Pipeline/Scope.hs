{-# LANGUAGE UndecidableInstances #-}

module Pipeline.Scope
  ( Result (..),
    analyse,
    analyseProgram,
    isLocal,
  )
where

import Common
import qualified Data.Map as Map
import qualified Data.Set as Set
import Polysemy.Error
import Optics
import Polysemy.State
import Syntax
import Import

type Env = Map Var RefersTo

data Result = Result
  { bound :: Map Label (Set Var),
    free :: Map Label Fvs,
    -- Maps the label INSIDE the binder to uses of the variable
    uses :: Map (Label, Var) (Set Label)
  }

$(makeFieldLabelsWith noPrefixFieldLabels ''Result)

type Effs r = Members [Error Err, State Result] r

isLocal :: RefersTo -> Bool
isLocal (RefLocal _) = True
isLocal _ = False

analyse :: Effs r => Env -> Term -> Sem r Fvs
analyse env Term {..} = do
  modify (over #bound $ Map.insert termLabel (Map.keysSet env))
  fvs <- case termF of
    Var v -> do
      case Map.lookup v env of
        Nothing -> throw (Err termLoc (Just termLabel) $ "Unknown variable: " <> pshow v)
        Just (RefLocal l) -> modify (over #uses (Map.insertWith (<>) (l, v) (Set.singleton termLabel)))
        _ -> pure ()
      pure $ Map.restrictKeys env (Set.singleton v)
    Abs _ xs t -> scoped env xs t
    App t ts -> do
      fvss <- traverse (analyse env) (t : ts)
      pure $ fold fvss
    Let _ p t b -> do
      fvs <- analyse env t
      fvs' <- scoped env (patternVars p) b
      pure $ fvs <> fvs'
    Case e bs -> do
      fvs <- analyse env e
      fvs' <- analyseBranches env bs
      pure $ fvs <> fvs'
    Cons c -> fold <$> traverse (analyse env) c
    Error {} -> pure $ Map.empty
  modify (over #free $ Map.insert termLabel fvs)
  pure fvs

scoped :: Effs r => Env -> [Var] -> Term -> Sem r Fvs
scoped env xs t = do
  let env' = Map.fromList (fmap (,RefLocal (termLabel t)) xs) <> env
  Map.withoutKeys <$> analyse env' t <*> pure (Set.fromList xs)

analyseBranches :: Effs r => Env -> Branches Term -> Sem r Fvs
analyseBranches env (Branch p t bs) = do
  fvs <- scoped env (patternVars p) t
  fvs' <- analyseBranches env bs
  pure $ fvs <> fvs'
analyseBranches _ BNil = pure Map.empty

analyseProgram :: Member (Error Err) r => Program Term -> Sem r Result
analyseProgram Program {..} = do
  let globals =
        Map.fromList $
          programDefinitions
            <&> \DefFun {..} -> (funName, RefGlobal)
      env = globals <> (RefPrimOp <$ primOps)
  execState (Result Map.empty Map.empty Map.empty) do
    for_ (programMain : programDefinitions) \case
      DefFun {..} -> scoped env (fmap snd funVars) funBody $> ()
