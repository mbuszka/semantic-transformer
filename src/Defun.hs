module Defun
  ( fromSource,
  )
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Defun.Cfa
import Defun.Labeled hiding (fromSource)
import Polysemy.Error
import Polysemy.Output
import Polysemy.Reader
import Polysemy.State
import Util
import qualified Syntax as Stx

type Effs r =
  Members
    '[ Reader (Map Label (TermF Label)),
       Reader (Map Label [Tag]),
       Reader (Set Var),
       Output (Tag, ([Var], Scope Term)),
       State (Map (Set Tag) (Var, [Var])),
       Embed IO,
       FreshVar
     ]
    r

fromSource ::
  Members '[FreshVar, Embed IO, Error Err] r => Program Term -> Sem r (Program Term)
fromSource program = do
  (Abstract {..}, analysis) <- analyse program
  let Program {..} = abstractProgram
  -- pprint' . pmap
  --   . fmap (defScope . fmap (toDbg abstractTerms))
  --   $ programDefinitions
  (lambdas, (applys, (pgm, main))) <-
    runOutputList
      . runState Map.empty
      . runReader abstractTerms
      . runReader analysis
      . runReader (Map.keysSet $ programDefinitions)
      $ do
        ds <- traverse (traverse runDefun) $ programDefinitions
        m' <- traverse runDefun $ programMain
        pure (ds, m')
  let lambdas' = Map.fromList lambdas
  let genBody vs t@(TopTag v) =
        pure
          ( PCons (Stx.Record t []),
            Scope [] (Term . App (Term . Var $ v) $ fmap (Term . Var) vs)
          )
      genBody vs tag = do
        let (fvs, (Scope xs b)) = lambdas' Map.! tag
        let b' = foldl' sub b (fmap fst xs `zip` vs)
        pure (PCons (Stx.Record tag (fmap (const (PVar ())) fvs)), scope fvs b')
  let genApply (tags, (var, f : vs)) = do
        ps <- traverse (genBody vs) . toList $ tags
        let b = Term $ Case (Term . Var $ f) (Patterns ps)
        pure $ (var, Def Set.empty (scope (f : vs) b))
      genApply _ = throw (InternalError "Malformed analysis result")
  newDefs <- traverse genApply . Map.toList $ applys
  let defs = pgm `Map.union` Map.fromList newDefs
  pure $ Program {programDefinitions = defs, programMain = main, ..}
  where
    sub t (x, y) = rename (Map.singleton x y) t

runDefun :: Effs r => Label -> Sem r Term
runDefun label@(Label x) = do
  term <- getTerm label
  term' <- traverse runDefun term
  case term' of
    Abs s -> do
      topVars <- ask
      let fvs = toList $ freeVars term' Set.\\ topVars
          tag = GenTag x
      output (tag, (fvs, s))
      pure . Term . Cons . Stx.Record tag . fmap (Term . Var) $ fvs
    Var {} -> do
      functions <- getFuns label
      case functions of
        [TopTag v] -> pure . Term . Cons $ Stx.Record (TopTag v) []
        _ -> pure . Term $ term'
    App (Term (Cons (Stx.Record (TopTag v) []))) xs ->
      pure . Term $ App (Term . Var $ v) xs
    App f xs -> do
      let App f' _ = term
      apply <- getApply f' (length xs)
      pure . Term $ App (Term . Var $ apply) (f : xs)
    _ -> pure . Term $ term'

getTerm :: Member (Reader (Map Label Labeled)) r => Label -> Sem r Labeled
getTerm lbl = do
  mby <- asks (Map.lookup lbl)
  case mby of
    Nothing -> error ("No binding for label " <> pshow lbl)
    Just t -> pure t

getFuns :: Effs r => Label -> Sem r [Tag]
getFuns lbl = do
  mby <- asks (Map.lookup lbl)
  case mby of
    Nothing -> error ("No analysis for label " <> pshow lbl)
    Just t -> pure t

getApply :: Effs r => Label -> Int -> Sem r Var
getApply lbl n = do
  functions <- Set.fromList <$> getFuns lbl
  mby <- gets (Map.lookup functions)
  case mby of
    Nothing -> do
      v <- freshVar
      vs <- sequence . take (n + 1) $ freshVars
      modify (Map.insert functions (v, vs))
      pure v
    Just (v, _) ->
      pure v
  where
    freshVars = freshVar : freshVars
