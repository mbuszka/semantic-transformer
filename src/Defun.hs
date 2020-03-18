module Defun where

import Cfa ()
import Control.Monad (foldM)
import qualified Data.Map as Map
import qualified Data.Set as Set
import MyPrelude
import Polysemy
import Polysemy.Output
import Polysemy.Reader
import Polysemy.State
import Syntax
import Syntax.Labeled (Label (..), Labeled (..))
import Syntax.Term

fromLabeled ::
  Members '[FreshVar, Embed IO] r =>
  Labeled ->
  Map Label [Tag] ->
  Sem r (Program Term)
fromLabeled (Labeled definitions terms decl) analysis = do
  (lambdas, (applys, pgm)) <-
    runOutputList
      . runState Map.empty
      . runReader terms
      . runReader analysis
      . runReader (Map.keysSet definitions)
      . traverse (traverse runDefun)
      . map aux
      . Map.toList
      $ definitions
  embed $ pprint applys
  embed $ pprint lambdas
  let lambdas' = Map.fromList lambdas
  let genBody vs t@(TopTag v) =
        pure
          ( PCons (Record t []),
            Scope [] (Term . App (Term . Var $ v) $ map (Term . Var) vs)
          )
      genBody vs tag = do
        let (fvs, (Scope xs b)) = lambdas' Map.! tag
        b' <- foldM sub b (xs `zip` vs)
        pure (PCons (Record tag (map (const (PVar ())) fvs)), Scope fvs b')
  let genApply (tags, (var, f : vs)) = do
        ps <- traverse (genBody vs) . toList $ tags
        let b = Term $ Case (Term . Var $ f) (Patterns ps)
        pure $ Def Set.empty var (Scope (f:vs) b)
  newDefs <- traverse genApply . Map.toList $ applys
  pure $ Program (pgm <> newDefs) decl
  where
    aux (name, scope) = Def mempty name scope
    sub t (x, y) = subst x (Var y) t

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
      pure . Term . Cons . Record tag . map (Term . Var) $ fvs
    Var {} -> do
      functions <- getFuns label
      case functions of
        [TopTag v] -> pure . Term . Cons $ Record (TopTag v) []
        _ -> pure . Term $ term'
    App (Term (Cons (Record (TopTag v) []))) xs ->
      pure . Term $ App (Term . Var $ v) xs
    App f xs -> do
      let App f' _ = term
      apply <- getApply f' (length xs)
      pure . Term $ App (Term . Var $ apply) (f : xs)
    _ -> pure . Term $ term'

getTerm :: Member (Reader (Map Label (TermF Label))) r => Label -> Sem r (TermF Label)
getTerm lbl = do
  mby <- asks (Map.lookup lbl)
  case mby of
    Nothing -> error ("No binding for label " <> (show . pretty $ lbl))
    Just t -> pure t

getFuns :: Effs r => Label -> Sem r [Tag]
getFuns lbl = do
  mby <- asks (Map.lookup lbl)
  case mby of
    Nothing -> do
      terms <- ask @(Map Label (TermF Label))
      pprint terms
      error ("No analysis for label " <> (show . pretty $ lbl))
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
