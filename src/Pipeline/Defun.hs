{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}

module Pipeline.Defun
  ( transform,
  )
where

import AbsInt
import qualified Data.Map as Map
import Optics
import Polysemy.Error
import qualified Pipeline.Scope as Scope
import Polysemy.State
import Syntax as Stx
import Common

data Defun = Defun
  { defunTerms :: Map Label Term,
    defunApplys :: Map (Set Function) (Var, [Var]),
    defunGlobals :: Map Var Tp,
    defunPrimOps :: Map Var Tp,
    defunAnalysis :: Result,
    defunLambdas :: Map Label (Tp, [Var], [Var], Term),
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

noDefun :: Effs r => Function -> Sem r Bool
noDefun (Lambda l) = gets (defunTerms) <&> Map.lookup l >>= \case
  Just Term {termF = Abs FunAnnot {..} _ _} -> pure $ not funDoDefun
  _ -> throwLabeled l "Defun: Term not found"
noDefun _ = pure True

doDefun :: Effs r => Function -> Sem r Bool
doDefun (Lambda l) = gets (defunTerms) <&> Map.lookup l >>= \case
  Just Term {termF = Abs FunAnnot {..} _ _} -> pure $ funDoDefun
  _ -> throwLabeled l "Defun: Term not found"
doDefun _ = pure True

transform ::
  Members [FreshVar, FreshLabel, Error Err, Embed IO] r => Program Term -> Sem r (Program Term)
transform program = do
  analysis <- AbsInt.run program
  let terms = programTerms program
  let s = initState terms analysis
  evalState s do
    res <- Scope.analyseProgram program
    modify (set #fvs $ Scope.free res)
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
  prims <- gets (toListOf $ #primOps % folded)
  let ls = lambdas <&> \case (name, fields, _, _) -> DefStruct name (fmap FieldName fields)
      xs = globals <> prims <&> \tag -> DefStruct tag []
  pure $ ls <> xs

genApplys :: Effs r => Sem r [DefFun Term]
genApplys = do
  applys <- gets (view $ #applys)
  for (Map.toList applys) \case
    (cases, (name, f : vs)) -> do
      bs <- genBody vs (toList cases)
      b <- mkTerm' =<< Case <$> (mkTerm' $ Var f) <*> pure bs
      pure $ DefFun name defaultFunAnnot (fmap (Nothing,) (f : vs)) b
    _ -> error "Uh oh"

genBody :: Effs r => [Var] -> [Function] -> Sem r (Branches Term)
genBody vs (Global v:fs) = do
  tag <- getGlobal v
  vs' <- traverse (mkTerm' . Var) vs
  let pat = PCons $ Stx.Record tag []
  body <- mkTerm' =<< App <$> (mkTerm' $ Var v) <*> pure vs'
  Branch pat body <$> genBody vs fs
genBody vs (PrimOp op:fs) = do
  let v = Stx.primOpNames Map.! op
  tag <- getPrim v
  vs' <- traverse (mkTerm' . Var) vs
  let pat = PCons $ Stx.Record tag []
  body <- mkTerm' =<< App <$> (mkTerm' $ Var v) <*> pure vs'
  Branch pat body <$> genBody vs fs
genBody vs (Lambda l:fs) = do
  (tag, fvs, xs, b) <- getLambda l
  let pat = PCons $ Stx.Record tag (fmap PVar fvs)
      body = rename (Map.fromList (xs `zip` vs)) b
  Branch pat body <$> genBody vs fs
genBody _ [] = pure BNil

getLambda :: Effs r => Label -> Sem r (Tp, [Var], [Var], Term)
getLambda label = gets (preview (#lambdas % ix label)) >>= \case
  Nothing -> error "Defun: Not a lambda"
  Just x -> pure x

getFvs :: Effs r => Label -> Sem r Fvs
getFvs lbl = gets (preview (#fvs % ix lbl)) >>= \case
  Nothing -> error "Defun: No free variable information"
  Just fvs -> pure fvs

getPrim :: Effs r => Var -> Sem r Tp
getPrim p = do
  gets (preview (#primOps % ix p)) >>= \case
    Just tag -> pure tag
    Nothing -> do
      tag <- freshTag ("Prim-" <> pshow p)
      modify' (over #primOps $ Map.insert p tag)
      pure tag

getGlobal :: Effs r => Var -> Sem r Tp
getGlobal v = do
  gets (preview (#globals % ix v)) >>= \case
    Just tag -> pure tag
    Nothing -> do
      tag <- freshTag . pshow . varToTp $ v
      modify' (over #globals $ Map.insert v tag)
      pure tag

transform' :: Effs r => Term -> Sem r Term
transform' tm = case termF tm of
  App f xs -> do
    fvs <- getFvs (termLabel f)
    xs' <- traverse transform' xs
    case termF f of
      Var v
        | fvs Map.! v /= RefLocal ->
          mkTerm' =<< App <$> (mkTerm' $ Var v) <*> pure xs'
      _ -> do
        f' <- transform' f
        fs <- getFuns (termLabel f)
        nd <- all noDefun fs
        dd <- all doDefun fs
        if  | nd -> mkTerm' $ App f' xs'
            | dd -> do
              apply <- getApply f (length xs)
              mkTerm' =<< App <$> (mkTerm' . Var $ apply) <*> pure (f' : xs')
            | otherwise ->
              throwLabeled (termLabel f) "Defun: Inconsitent defunctionalization requirements of lambdas"
  t -> traverse transform' t >>= transformL (termLabel tm)

transformL :: Effs r => Label -> TermF Term -> Sem r Term
transformL label tm = case tm of
  Abs as xs t -> do
    nd <- noDefun (Lambda label)
    if nd
      then pure $ Term {termLabel = label, termF = tm, termLoc = Nothing}
      else do
        fvs <- getFvs label <&> Map.filter (== RefLocal) <&> Map.keysSet <&> toList
        tag <- case funDefunName as of
          Nothing -> freshTag "Fun"
          Just tag -> pure tag
        modify' $ over #lambdas (Map.insert label (tag, fvs, xs, t))
        mkTerm' =<< Cons <$> (Stx.Record tag <$> traverse (mkTerm' . Var) fvs)
  Var v -> do
    fvs <- getFvs label
    case fvs Map.! v of
      RefGlobal -> do
        tag <- getGlobal v
        mkTerm' . Cons $ Stx.Record tag []
      RefPrimOp -> do
        tag <- getPrim v
        mkTerm' . Cons $ Stx.Record tag []
      RefLocal -> mkTerm' . Var $ v
  t -> mkTerm' t

getFuns :: Effs r => Label -> Sem r (Set Function)
getFuns lbl = do
  mby <- gets (preview (#analysis % ix lbl))
  case mby of
    Nothing -> throwLabeled lbl "Defun: No analysis"
    Just t -> pure t

getApply :: Effs r => Term -> Int -> Sem r Var
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
