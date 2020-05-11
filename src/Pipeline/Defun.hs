{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}

module Pipeline.Defun
  ( transform,
  )
where

import qualified Data.Map as Map
import Optics
import Polysemy.Error
import Polysemy.State
import qualified Pipeline.Scope as Scope
import Syntax as Stx
import AbsInt
import Util
-- import Util.Pretty

data Defun
  = Defun
      { defunTerms :: Map Label Term,
        defunApplys :: Map (Set Function) (Var, [Var]),
        defunGlobals :: Map Var Tp,
        defunPrimOps :: Map Var Tp,
        defunAnalysis :: Result,
        defunLambdas :: Map Label (Tp, [Var], Scope Term),
        defunFvs :: Map Label Fvs
      }

$(makeFieldLabels ''Defun)

type Effs r = Members [FreshVar, FreshLabel, State Defun, Error Err, Embed IO] r

initState :: Map Label Term -> Result -> Defun
initState terms analysis =
  Defun
    { defunTerms = terms,
      defunApplys = Map.empty,
      defunGlobals = Map.empty,
      defunPrimOps = Map.empty,
      defunAnalysis = analysis,
      defunLambdas = Map.empty,
      defunFvs = Map.empty
    }

transform ::
  Members [FreshVar, FreshLabel, Error Err, Embed IO] r => Program Term -> Sem r (Program Term)
transform program = do
  analysis <- AbsInt.run program
  let terms = programTerms program
  let s = initState terms analysis
  evalState s do
    let unwrap t = pure (termTerm t, Nothing)
        wrap old fvs _ = do
          modify (over #fvs $ Map.insert (termLabel old) fvs)
          pure ()
    _ <- Scope.checkProgram unwrap wrap program
    Program {..} <- traverse transform' program
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

genApplys :: Effs r => Sem r [DefFun Term]
genApplys = do
  applys <- gets (view $ #applys)
  for (Map.toList applys) \case
    (cases, (name, f : vs)) -> do
      ps <- traverse (genBody vs) (toList cases)
      b <- term =<< Case <$> (term $ Var f) <*> pure (Patterns ps)
      pure $ DefFun name FunAnnot {funAtomic=False} (scope (f : vs) b)
    _ -> error "Uh oh"

genBody :: Effs r => [Var] -> Function -> Sem r (Pattern (), Scope Term)
genBody vs (Global v) = do
  tag <- getGlobal v
  vs' <- traverse (term . Var) vs
  let pat = PCons $ Stx.Record tag []
  body <- term =<< App <$> (term $ Var v) <*> pure vs'
  pure (pat, scope [] body)
genBody vs (PrimOp op) = do
  let v = Stx.primOpNames Map.! op
  tag <- getPrim v
  vs' <- traverse (term . Var) vs
  let pat = PCons $ Stx.Record tag []
  body <- term =<< App <$> (term $ Var v) <*> pure vs'
  pure (pat, scope [] body)
genBody vs (Lambda l) = do
  -- pprint' $ "generating body for" <+> pretty l
  (tag, fvs, Scope xs b) <- getLambda l
  -- pprint' $ "bound vars: " <+> pretty (fmap fst xs)
  -- pprint' $ "free vars:" <+> pretty fvs
  let p = PCons $ Stx.Record tag (fvs $> PVar ())
      b' = scope fvs (rename (Map.fromList (fmap fst xs `zip` vs)) b)
  pure (p, b')

getLambda ::
  Members [State Defun, Error Err] r => Label -> Sem r (Tp, [Var], Scope Term)
getLambda label = gets (preview (#lambdas % ix label)) >>= \case
  Nothing -> error "No lambda"
  Just x -> pure x

getFvs :: Member (State Defun) r => Label -> Sem r Fvs
getFvs lbl = gets (preview (#fvs % ix lbl)) >>= \case
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

transform' :: Effs r => Term -> Sem r Term
transform' tm = case termTerm tm of
  App f xs -> do
    fvs <- getFvs (termLabel f)
    xs' <- traverse transform' xs
    case termTerm f of
      Var v
        | fvs Map.! v /= RefLocal ->
          term =<< App <$> (term $ Var v) <*> pure xs'
      _ -> do
        f' <- transform' f
        apply <- getApply f (length xs)
        term =<< App <$> (term . Var $ apply) <*> pure (f' : xs')
  t -> traverse transform' t >>= transformL (termLabel tm)

transformL :: Effs r => Label -> TermF Term -> Sem r Term
transformL label tm = case tm of
  Abs _ s -> do
    fvs <- getFvs label <&> Map.filter (==RefLocal) <&> Map.keysSet <&> toList
    tag <- freshTag "Lambda"
    modify' $ over #lambdas (Map.insert label (tag, fvs, s))
    term =<< Cons <$> (Stx.Record tag <$> traverse (term . Var) fvs)
  Var v -> do
    fvs <- getFvs label
    case fvs Map.! v of
      RefGlobal -> do
        tag <- getGlobal v
        term . Cons $ Stx.Record tag []
      RefPrimOp -> do
        tag <- getPrim v
        term . Cons $ Stx.Record tag []
      RefLocal -> term . Var $ v
  t -> term t

-- getTerm :: Member (State Defun) r => Label -> Sem r Labeled
-- getTerm lbl = do
--   mby <- gets (preview (#terms % ix lbl))
--   case mby of
--     Nothing -> error ("No binding for label " <> pshow lbl)
--     Just t -> pure t

getFuns :: Member (State Defun) r => Label -> Sem r (Set Function)
getFuns lbl = do
  mby <- gets (preview (#analysis % ix lbl))
  case mby of
    Nothing -> error ("No analysis for label " <> pshow lbl)
    Just t -> pure t

getApply :: Members [FreshVar, State Defun] r => Term -> Int -> Sem r Var
getApply Term {..} n = do
  functions <- getFuns termLabel
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
