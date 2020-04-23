{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}

module Pipeline.Defun
  ( transform,
  )
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Defun.Cfa
import Defun.Labeled hiding (fromSource)
import Optics
import Polysemy.Error
import Polysemy.State
import qualified Syntax as Stx
import Syntax.Term
import Util

data Defun
  = Defun
      { defunTerms :: Map Label Labeled,
        defunApplys :: Map (Set Res) (Var, [Var]),
        defunFvs :: Map Label Fvs,
        defunGlobals :: Map Var Tp,
        defunPrimOps :: Map Var Tp,
        defunAnalysis :: Map Label (Set Res),
        defunLambdas :: Map Label (Tp, [Var], Scope Term)
      }

$(makeFieldLabels ''Defun)

initState :: Map Label Labeled -> Map Label Fvs -> Map Label (Set Res) -> Defun
initState terms fvs analysis =
  Defun
    { defunTerms = terms,
      defunApplys = Map.empty,
      defunGlobals = Map.empty,
      defunPrimOps = Map.empty,
      defunAnalysis = analysis,
      defunLambdas = Map.empty,
      defunFvs = fvs
    }

transform ::
  Members [FreshVar, Embed IO, Error Err] r => Program Term -> Sem r (Program Term)
transform program = do
  (Abstract {..}, analysis) <- analyse program
  let s = initState abstractTerms abstractFvs analysis
  evalState s do
    Program {..} <- traverse transform' abstractProgram
    structs <- genStructs
    applys <- genApplys
    pure $
      Program
        { programDefinitions = programDefinitions <> applys,
          programStructs = programStructs <> structs,
          ..
        }

genStructs ::
  Members [State Defun, Error Err] r => Sem r [DefStruct]
genStructs = do
  lambdas <- gets (toListOf $ #lambdas % folded)
  globals <- gets (toListOf $ #globals % folded)
  primOps <- gets (toListOf $ #primOps % folded)
  let ls = lambdas <&> \case (name, fields, _) -> DefStruct name (fmap FieldName fields)
      xs = globals <> primOps <&> \tag -> DefStruct tag []
  pure $ ls <> xs

genApplys ::
  Members [FreshVar, State Defun, Error Err] r => Sem r [DefFun Term]
genApplys = do
  applys <- gets (view $ #applys)
  for (Map.toList applys) \case
    (cases, (name, f : vs)) -> do
      ps <- traverse (genBody vs) (toList cases)
      let b = Term $ Case (Term $ Var f) (Patterns ps)
      pure $ DefFun name Set.empty (scope (f : vs) b)
    _ -> error "Uh oh"

genBody ::
  Members [FreshVar, State Defun, Error Err] r => [Var] -> Res -> Sem r (Pattern (), Scope Term)
genBody vs (RGlobal v) = do
  tag <- getGlobal v
  let vs' = fmap (Term . Var) vs
  pure $ (PCons $ Stx.Record tag [], scope [] $ Term (App (Term $ Var v) vs'))
genBody vs (RPrim v) = do
  tag <- getPrim v
  let vs' = fmap (Term . Var) vs
  pure $ (PCons $ Stx.Record tag [], scope [] $ Term (App (Term $ Var v) vs'))
genBody vs (RLambda l) = do
  (tag, fvs, Scope xs b) <- getLambda l
  let p = PCons $ Stx.Record tag (fvs $> PVar ())
      b' = scope fvs (rename (Map.fromList (fmap fst xs `zip` vs)) b)
  pure (p, b')

getLambda ::
  Members [State Defun, Error Err] r => Label -> Sem r (Tp, [Var], Scope Term)
getLambda label = gets (preview (#lambdas % ix label)) >>= \case
  Nothing -> error "No lambda"
  Just x -> pure x

getFvs :: Member (State Defun) r => Label -> Sem r Fvs
getFvs label = gets (preview (#fvs % ix label)) >>= \case
  Nothing -> error "No free variable information"
  Just fvs -> pure fvs

getPrim :: Members [FreshVar, State Defun] r => Var -> Sem r Tp
getPrim p = do
  gets (preview (#primOps % ix p)) >>= \case
    Just tag -> pure tag
    Nothing -> do
      tag <- freshTag ("Prim-" <> pshow p)
      modify' (over #primOps $ Map.insert p tag)
      pure tag

getGlobal :: Members [FreshVar, State Defun] r => Var -> Sem r Tp
getGlobal v = do
  gets (preview (#globals % ix v)) >>= \case
    Just tag -> pure tag
    Nothing -> do
      tag <- freshTag . pshow . varToTp $ v
      modify' (over #globals $ Map.insert v tag)
      pure tag

transform' :: Members [FreshVar, State Defun] r => Label -> Sem r Term
transform' label = getTerm label >>= \case
  App f xs -> do
    fvs <- getFvs f
    xs' <- traverse transform' xs
    getTerm f >>= \case
      Var v
        | fvs Map.! v /= Stx.Local ->
          pure $ Term $ App (Term $ Var v) xs'
      _ -> do
        f' <- transform' f
        apply <- getApply f (length xs)
        pure . Term $ App (Term . Var $ apply) (f' : xs')
  t -> traverse transform' t >>= transformL label

transformL :: Members [FreshVar, State Defun] r => Label -> TermF Term -> Sem r Term
transformL label term = case term of
  Abs s -> do
    fvs <- getFvs label <&> Map.filter (==Local) <&> Map.keysSet <&> toList
    tag <- freshTag "Lambda"
    modify' $ over #lambdas (Map.insert label (tag, fvs, s))
    pure $ Term (Cons (Stx.Record tag (fmap (Term . Var) fvs)))
  Var v -> do
    fvs <- getFvs label
    case fvs Map.! v of
      Stx.Global _ -> do
        tag <- getGlobal v
        pure . Term . Cons $ Stx.Record tag []
      Stx.PrimOp -> do
        tag <- getPrim v
        pure . Term . Cons $ Stx.Record tag []
      Stx.Local -> pure . Term . Var $ v
  t -> pure $ Term t

getTerm :: Member (State Defun) r => Label -> Sem r Labeled
getTerm lbl = do
  mby <- gets (preview (#terms % ix lbl))
  case mby of
    Nothing -> error ("No binding for label " <> pshow lbl)
    Just t -> pure t

getFuns :: Member (State Defun) r => Label -> Sem r (Set Res)
getFuns lbl = do
  mby <- gets (preview (#analysis % ix lbl))
  case mby of
    Nothing -> error ("No analysis for label " <> pshow lbl)
    Just t -> pure t

getApply :: Members [FreshVar, State Defun] r => Label -> Int -> Sem r Var
getApply lbl n = do
  functions <- getFuns lbl
  mby <- gets (preview (#applys % ix functions))
  case mby of
    Nothing -> do
      v <- freshVar "apply"
      vs <- sequence . take (n + 1) $ freshVar "fn" : freshVars
      modify (over #applys $ Map.insert functions (v, vs))
      pure v
    Just (v, _) ->
      pure v
  where
    freshVars = freshVar "val" : freshVars
