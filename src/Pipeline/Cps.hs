{-# LANGUAGE UndecidableInstances #-}

module Pipeline.Cps
  ( fromAnf,
  )
where

import AbsInt
import Common
import qualified Data.Map as Map
import Optics
import Polysemy.Error
import Polysemy.State
import Syntax

data Cps = Cps
  { cpsAnalysis :: AbsInt.Result,
    cpsGlobals :: Map Var FunAnnot,
    cpsTerms :: Map Label Term
  }

$(makeFieldLabels ''Cps)

data CT
  = Trivial Term
  | SLet Label LetAnnot Pattern CT CT
  | SApp Label Term [Term]
  | SCase Label Term (Branches CT)
  | SPanic Label Text

type Effs r = Members '[Error Err, State Cps, FreshVar, FreshLabel] r

type Effs' r = Members '[Error Err, FreshVar, FreshLabel, Embed IO] r

isAtomicFunction :: Effs r => AbsInt.Function -> Sem r Bool
isAtomicFunction (AbsInt.Global v) = gets (preview $ #globals % ix v) >>= \case
  Nothing -> throwMsg $ "Cps: Unknown top-level function: " <> pshow v
  Just as -> pure $ not $ funDoCps as
isAtomicFunction (AbsInt.PrimOp _) = pure True
isAtomicFunction (AbsInt.Lambda l) = gets (preview $ #terms % ix l) >>= \case
  Just (Term {termF = Abs as _ _}) -> pure $ not $ funDoCps as
  _ -> throwLabeled l $ "Cps: Not a lambda"

isAtomic :: Effs r => Term -> Sem r Bool
isAtomic t =
  gets cpsAnalysis >>= AbsInt.lookup (termLabel t) >>= all isAtomicFunction

allSerious :: Effs r => Term -> Sem r Bool
allSerious t =
  gets cpsAnalysis >>= AbsInt.lookup (termLabel t) >>= all (\f -> not <$> isAtomicFunction f)

orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing m = m
orElse m _ = m

transformAtomic :: Effs r => Maybe Text -> Term -> Sem r Term
transformAtomic txt tm = case termF tm of
  App f ts -> do
    b <- isAtomic f
    ts' <- traverse (transformAtomic txt) ts
    if b
      then pure tm {termF = App f ts'}
      else do
        b' <- allSerious f
        if b'
          then do
            v <- freshVar "x"
            let as = defaultFunAnnot {funDefunName = Just (MkTp "Halt"), funDefunApply = Just (MkVar "continue")}
            k <- mkTerm' =<< Abs as [v] <$> mkTerm' (Var v)
            pure tm {termF = App f (ts <> [k])}
          else throwLabeled (termLabel tm) $ "Cps: Application of both cps and non-cps functions"
  Abs a xs t
    | not $ funDoCps a ->
      mkTerm' . Abs a xs =<< transformAtomic txt t
  Abs a xs t -> do
    k' <- freshVar "cont"
    t' <- transformNormal (Var k') txt =<< classify t
    pure tm {termF = Abs a (xs <> [k']) t'}
  Case t bs -> do
    t' <- transformAtomic txt t
    bs' <- transformBranches transformAtomic txt bs
    pure tm {termF = Case t' bs'}
  t -> mkTerm' =<< traverse (transformAtomic txt) t

transformBranches :: Effs r => (Maybe Text -> t -> Sem r Term) -> Maybe Text -> Branches t -> Sem r (Branches Term)
transformBranches f txt (Branch p t bs) = do
  let txt' = patternTp p `orElse` txt
  Branch p <$> f txt' t <*> transformBranches f txt bs
transformBranches _ _ BNil = pure $ BNil

isTrivial :: CT -> Sem r Bool
isTrivial Trivial {} = pure True
isTrivial _ = pure False

trivial :: Term -> Sem r CT
trivial = pure . Trivial

classify :: Effs r => Term -> Sem r CT
classify old@Term {..} =
  case termF of
    Var {} -> trivial old
    Abs {} -> trivial old
    App f ts -> isAtomic f >>= \case
      True -> trivial old
      False -> pure $ SApp termLabel f ts
    Let as x t b -> ((,) <$> classify t <*> classify b) >>= \case
      (t', b') -> pure $ SLet termLabel as x t' b'
    Case t ps -> do
      ps' <- traverse classify ps
      all isTrivial ps' >>= \case
        True -> trivial old
        False -> pure $ SCase termLabel t ps'
    Cons _ -> trivial old
    Error err -> pure $ SPanic termLabel err

patternTp :: Pattern -> Maybe Text
patternTp (PCons (Record (MkTp t) _)) = Just t
patternTp _ = Nothing

transformNormal :: Effs r => TermF Term -> Maybe Text -> CT -> Sem r Term
transformNormal k txt tm = case tm of
  Trivial t -> do
    t' <- transformAtomic txt t
    v <- freshVar "val"
    body <- mkTerm' =<< App <$> mkTerm' k <*> traverse mkTerm' [Var v]
    mkTerm' $ Let LetAnnot {letGenerated = True} (PVar v) t' body
  SApp termLabel f ts -> do
    k' <- mkTerm' k
    pure Term {termF = App f (ts <> [k']), ..}
  SLet termLabel a x (Trivial t) b -> do
    t' <- transformAtomic txt t
    b' <- transformNormal k txt b
    pure Term {termF = Let a x t' b', ..}
  SLet _ _ (PVar x) t b -> do
    name <- freshTag $ fromMaybe "Cont" txt
    apply <- freshVar "continue"
    b' <- transformNormal k txt b
    let k' = Abs defaultFunAnnot {funDefunName = Just name, funDefunApply = Just apply} [x] b'
    transformNormal k' txt t
  SLet _ a p t b -> do
    v <- freshVar "val"
    name <- freshTag $ fromMaybe "Cont" txt
    apply <- freshVar "continue"
    b' <- transformNormal k txt b
    b'' <- mkTerm' =<< Let a p <$> mkTerm' (Var v) <*> pure b'
    let k' = Abs defaultFunAnnot {funDefunName = Just name, funDefunApply = Just apply} [v] b''
    transformNormal k' txt t
  SCase termLabel t bs ->
    case k of
      Var {} -> do
        bs' <- transformBranches (transformNormal k) txt bs
        pure Term {termF = Case t bs', ..}
      _ -> do
        k' <- freshVar "cont"
        bs' <- transformBranches (transformNormal (Var k')) txt bs
        let t' = Term {termF = Case t' bs', ..}
        mkTerm' =<< Let LetAnnot {letGenerated = True} (PVar k') <$> mkTerm' k <*> pure t'
  SPanic termLabel err -> pure Term {termF = Error err, ..}
  where
    termLoc = Nothing

fromAnf :: Effs' r => Program Term -> Sem r (Program Term)
fromAnf p@Program {..} = do
  let cpsGlobals = Map.fromList $ programDefinitions <&> \case DefFun {..} -> (funName, funAnnot)
  let cpsTerms = programTerms p
  cpsAnalysis <- AbsInt.run p
  evalState Cps {..} do
    defs <- traverse aux (programDefinitions)
    main <- traverse (transformAtomic Nothing) programMain
    pure $ Program {programDefinitions = defs, programMain = main, ..}
  where
    aux :: Effs r => DefFun Term -> Sem r (DefFun Term)
    aux d@DefFun {..} =
      if not $ funDoCps funAnnot
        then traverse (transformAtomic Nothing) d
        else do
          k <- freshVar "cont"
          t'' <- classify funBody
          t' <- transformNormal (Var k) Nothing t''
          pure DefFun {funVars = funVars <> [(Nothing, k)], funBody = t', ..}
