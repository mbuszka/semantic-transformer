{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}

module Pipeline.Defun
  ( transform,
  )
where

import AbsInt hiding (lookup)
import Common
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Ord (Down (..))
import qualified Data.Set as Set
import Optics
import qualified Pipeline.Scope as Scope
import Polysemy.Error
import Polysemy.State
import Syntax as Stx
import Util.Pretty
import Import

data Defun = Defun
  { defunTerms :: Map Label Term,
    defunApplys :: Map (Set Function) Var,
    defunGlobals :: Map Var Tp,
    defunPrimOps :: Map Var Tp,
    defunAnalysis :: Result,
    defunLambdas :: Map Label (Tp, [Var], [Var], Term),
    defunFvs :: Map Label Fvs,
    defunDefinitions :: Map Var (DefFun Term)
  }

$(makeFieldLabels ''Defun)

type Effs r = Members [FreshVar, FreshLabel, State Defun, Error Err, Embed IO] r

lookup :: (Ord k, Effs r) => k -> Text -> Map k a -> Sem r a
lookup k err m = case Map.lookup k m of
  Just v -> pure v
  Nothing -> throwMsg err

initState :: Program Term -> Result -> Map Label Fvs -> Defun
initState pgm analysis fvs =
  Defun
    { defunTerms = programTerms pgm,
      defunApplys = Map.empty,
      defunGlobals = Map.empty,
      defunPrimOps = Map.empty,
      defunAnalysis = analysis,
      defunLambdas = Map.empty,
      defunFvs = fvs,
      defunDefinitions = programFuns pgm
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
  resCfa <- AbsInt.run program
  resScope <- Scope.analyseProgram program
  -- embed $ pprint' $ prettyMap' $ fmap prettyMap (Scope.free resScope)
  -- embed $ pprint' $ prettyMap $ fmap toList resCfa
  let s = initState program resCfa (Scope.free resScope)
  evalState s do
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
    (cases, name) -> do
      let cs = Set.toList cases
      vs <- genNames cs
      bs <- genBody vs cs
      f <- freshVar "fn"
      b <- mkTerm' =<< Case <$> (mkTerm' $ Var f) <*> pure bs
      pure $ DefFun name defaultFunAnnot (fmap (Nothing,) (f : vs)) b

genBody :: Effs r => [Var] -> [Function] -> Sem r (Branches Term)
genBody vs (Global v : fs) = do
  tag <- getGlobal v
  vs' <- traverse (mkTerm' . Var) vs
  let pat = PCons $ Stx.Record tag []
  body <- mkTerm' =<< App <$> (mkTerm' $ Var v) <*> pure vs'
  Branch pat body <$> genBody vs fs
genBody vs (PrimOp op : fs) = do
  let v = Stx.primOpNames Map.! op
  tag <- getPrim v
  vs' <- traverse (mkTerm' . Var) vs
  let pat = PCons $ Stx.Record tag []
  body <- mkTerm' =<< App <$> (mkTerm' $ Var v) <*> pure vs'
  Branch pat body <$> genBody vs fs
genBody vs (Lambda l : fs) = do
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
    -- embed $ pprint' $ "transforming application:" <+> pretty (termLabel f)
    fvs <- getFvs (termLabel f)
    xs' <- traverse transform' xs
    case termF f of
      Var v
        | not $ Scope.isLocal $ fvs Map.! v -> do
          -- embed . pprint' $ "applying not local variable:" <+> pretty v
          mkTerm' =<< App <$> (mkTerm' $ Var v) <*> pure xs'
      _ -> do
        -- embed . pprint' $ "applying something else"
        f' <- transform' f
        fs <- getFuns (termLabel f)
        -- embed . pprint' $ "Used funs:" <+> aligned (toList fs)
        nd <- all noDefun fs
        dd <- all doDefun fs
        -- embed . pprint' $ "No defun:" <+> pretty nd <+> "Do defun:" <+> pretty dd
        if  | dd -> do
              apply <- getApply fs
              mkTerm' =<< App <$> (mkTerm' . Var $ apply) <*> pure (f' : xs')
            | nd -> mkTerm' $ App f' xs'
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
        fvs <- getFvs label <&> Map.filter Scope.isLocal <&> Map.keysSet <&> toList
        tag <- case funDefunName as of
          Nothing -> freshTag "Fun"
          Just tag -> pure tag
        modify' $ over #lambdas (Map.insert label (tag, fvs, xs, t))
        mkTerm' =<< Cons <$> (Stx.Record tag <$> traverse (mkTerm' . Var) fvs)
  Var v -> do
    fvs <- getFvs label
    case fvs Map.! v of
      RefGlobal -> do
        globals <- gets (defunDefinitions)
        let DefFun {funAnnot = FunAnnot {funDoDefun}} = globals Map.! v
        if funDoDefun
          then do
            tag <- getGlobal v
            mkTerm' . Cons $ Stx.Record tag []
          else pure $ Term {termLabel = label, termF = tm, termLoc = Nothing}
      RefPrimOp -> do
        tag <- getPrim v
        mkTerm' . Cons $ Stx.Record tag []
      RefLocal _ -> mkTerm' . Var $ v
  t -> mkTerm' t

getFuns :: Effs r => Label -> Sem r (Set Function)
getFuns lbl = do
  mby <- gets (preview (#analysis % ix lbl))
  case mby of
    Nothing -> throwLabeled lbl "Defun: No analysis"
    Just t -> pure t

getApply :: Effs r => Set Function -> Sem r Var
getApply functions = do
  mby <- gets (preview (#applys % ix functions))
  case mby of
    Nothing -> do
      a <-
        traverse getApplyName (Set.toList functions)
          <&> (headOf $ folded % folded)
      v <- maybe (freshVar "apply") pure a
      modify (over #applys $ Map.insert functions v)
      pure v
    Just v -> pure v

getApplyName :: Effs r => Function -> Sem r (Maybe Var)
getApplyName f = case f of
  Lambda l -> gets (preview $ #terms % ix l) >>= \case
    Just (Term {termF = Abs as _ _}) -> pure (funDefunApply as)
    _ -> throwLabeled l "Defun: Not a lambda"
  Global v ->
    gets defunDefinitions
      >>= lookup v ("No such var " <> pshow v)
      <&> funDefunApply . funAnnot
  _ -> pure Nothing

genNames :: Effs r => [Function] -> Sem r [Var]
genNames fs = do
  let aux (Lambda l) = getLambda l <&> \case (_, fvs, xs, _) -> [(xs, fvs)]
      aux (Global x) = do
        defs <- gets defunDefinitions
        let xs = snd <$> funVars (defs Map.! x)
        pure [(xs, [])]
      aux _ = pure []
  (usedVars, fvss) <- fmap (List.unzip . join) $ traverse aux fs
  let fvs = foldMap Set.fromList fvss
      vars = List.transpose usedVars
      order vs = fmap fst $ List.sortOn (Down . snd) $ Map.toList $ group vs
      firstVar [] = freshVar "val"
      firstVar (v : vs) = if Set.notMember v fvs then pure v else firstVar vs
  traverse (firstVar . order) vars

group :: Ord a => [a] -> Map a Int
group xs' = aux Map.empty xs'
  where
    aux m [] = m
    aux m (x : xs) = aux (Map.insertWith (+) x 1 m) xs
