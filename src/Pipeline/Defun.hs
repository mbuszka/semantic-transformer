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
        defunGlobals :: Map Var Tag,
        defunPrimOps :: Map Var Tag,
        defunAnalysis :: Map Label (Set Res),
        defunLambdas :: Map Label (Tag, [Var], Scope Term)
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
  let ls = lambdas <&> \case (name, fields, _) -> DefStruct name fields
      xs = globals <> primOps <&> \tag -> DefStruct tag []
  pure $ ls <> xs

genApplys ::
  Members [FreshVar, State Defun, Error Err] r => Sem r (Map Var (Def Term))
genApplys = do
  applys <- gets (view $ #applys)
  defs <- for (Map.toList applys) \case
    (cases, (name, f : vs)) -> do
      ps <- traverse (genBody vs) (toList cases)
      let b = Term $ Case (Term $ Var f) (Patterns ps)
      pure (name, Def Set.empty (scope (f : vs) b))
    _ -> error "Uh oh"
  pure $ Map.fromList defs

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
  let p = PCons $ Stx.Record tag (fvs $> PVar () Nothing)
      b' = scope fvs (rename (Map.fromList (fmap fst xs `zip` vs)) b)
  pure (p, b')

getLambda ::
  Members [State Defun, Error Err] r => Label -> Sem r (Tag, [Var], Scope Term)
getLambda label = gets (preview (#lambdas % ix label)) >>= \case
  Nothing -> error "No lambda"
  Just x -> pure x

-- fromTerm program = do
--   (pgm@Abstract {..}, analysis) <- analyse program
--   let Program {..} = abstractProgram
--   -- pprint' . pmap
--   --   . fmap (defScope . fmap (toDbg abstractTerms))
--   --   $ programDefinitions
--   (lambdas, (applys, (pgm, main))) <-
--     runOutputList
--       . runState Map.empty
--       . runReader pgm
--       . runReader analysis
--       . runReader (Map.keysSet $ programDefinitions)
--       $ do
--         ds <- traverse (traverse runDefun) $ programDefinitions
--         m' <- traverse runDefun $ programMain
--         pure (ds, m')
--   let lambdas' = Map.fromList lambdas
--   let genBody vs t = error ""
--       --   pure
--       --     ( PCons (Stx.Record t []),
--       --       Scope [] (Term . App (Term . Var $ v) $ fmap (Term . Var) vs)
--       --     )
--       -- genBody vs tag = do
--       --   let (fvs, (Scope xs b)) = lambdas' Map.! tag
--       --   let b' = foldl' sub b (fmap fst xs `zip` vs)
--       --   pure (PCons (Stx.Record tag (fmap (const (PVar () Nothing)) fvs)), scope fvs b')
--   let genApply (tags, (var, f : vs)) = do
--         ps <- traverse (genBody vs) . toList $ tags
--         let b = Term $ Case (Term . Var $ f) (Patterns ps)
--         pure $ (var, Def Set.empty (scope (f : vs) b))
--       genApply _ = throw (InternalError "Malformed analysis result")
--   newDefs <- traverse genApply . Map.toList $ applys
--   let defs = pgm `Map.union` Map.fromList newDefs
--   pure $ Program {programDefinitions = defs, programMain = main, ..}
--   where
--     sub t (x, y) = rename (Map.singleton x y) t

getFvs :: Member (State Defun) r => Label -> Sem r Fvs
getFvs label = gets (preview (#fvs % ix label)) >>= \case
  Nothing -> error "No free variable information"
  Just fvs -> pure fvs

getPrim :: Members [FreshVar, State Defun] r => Var -> Sem r Tag
getPrim p = do
  gets (preview (#primOps % ix p)) >>= \case
    Just tag -> pure tag
    Nothing -> do
      tag <- freshTag ("prim-" <> pshow p)
      modify' (over #primOps $ Map.insert p tag)
      pure tag

getGlobal :: Members [FreshVar, State Defun] r => Var -> Sem r Tag
getGlobal v = do
  gets (preview (#globals % ix v)) >>= \case
    Just tag -> pure tag
    Nothing -> do
      tag <- freshTag (pshow v)
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
    tag <- freshTag "lambda"
    modify' $ over #lambdas (Map.insert label (tag, fvs, s))
    pure $ Term (Cons (Stx.Record tag (fmap (Term . Var) fvs)))
  Var v -> do
    fvs <- getFvs label
    case fvs Map.! v of
      Stx.Global -> do
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
