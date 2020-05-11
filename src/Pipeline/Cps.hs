{-# LANGUAGE UndecidableInstances #-}

module Pipeline.Cps
  ( fromAnf,
  )
where

import AbsInt
import qualified Data.Map as Map
import Optics
import Polysemy.Error
import Polysemy.State
import Syntax
import Util

data Cps
  = Cps
      { cpsAnalysis :: AbsInt.Result,
        cpsGlobals :: Map Var FunAnnot,
        cpsTerms :: Map Label Term
      }

$(makeFieldLabels ''Cps)

data CT
  = Trivial Term
  | SLet Label LetAnnot (Pattern Var) CT CT
  | SApp Label Term [Term]
  | SCase Label Term (Patterns CT)
  | SPanic Label

type Effs r = Members '[Error Err, State Cps, FreshVar, FreshLabel] r

type Effs' r = Members '[Error Err, FreshVar, FreshLabel, Embed IO] r

isAtomicFunction :: Effs r => AbsInt.Function -> Sem r Bool
isAtomicFunction (AbsInt.Global v) = gets (preview $ #globals % ix v) >>= \case
  Nothing -> throw $ InternalError $ "Unknown top-level function: " <> pshow v
  Just as -> pure $ funAtomic as
isAtomicFunction (AbsInt.PrimOp _) = pure True
isAtomicFunction (AbsInt.Lambda l) = gets (preview $ #terms % ix l) >>= \case
  Just (Term {termTerm=Abs as _}) -> pure $ funAtomic as
  _ -> throw $ InternalError $ "Not a label for lambda" <> pshow l

isAtomic :: Effs r => Term -> Sem r Bool
isAtomic t =
  gets cpsAnalysis >>= AbsInt.lookup (termLabel t) >>= all isAtomicFunction

allSerious :: Effs r => Term -> Sem r Bool
allSerious t =
  gets cpsAnalysis >>= AbsInt.lookup (termLabel t) >>= all (\f -> not <$> isAtomicFunction f)

transformAtomic :: Effs r => Term -> Sem r Term
transformAtomic tm = case termTerm tm of
  App f ts -> do
    b <- isAtomic f
    ts' <- traverse transformAtomic ts
    if b
      then pure tm {termTerm = App f ts'}
      else do
        b' <- allSerious f
        if b'
          then do
            v <- freshVar "x"
            k <- term =<< Abs FunAnnot {funAtomic = False} . scope [v] <$> term (Var v)
            pure tm {termTerm = App f (ts <> [k])}
          else throw $ InternalError $ "Application of both cps and non-cps functions"
  Abs a s | funAtomic a ->
    term . Abs a =<< traverse transformAtomic s
  Abs a (Scope xs t) -> do
    k' <- freshVar "cont"
    t' <- join $ transformNormal <$> classify t <*> pure (Var k')
    pure tm {termTerm = Abs a $ Scope (xs <> [(k', Nothing)]) t'}
  t' -> do
    t'' <- traverse transformAtomic t'
    pure tm {termTerm = t''}

isTrivial :: CT -> Sem r Bool
isTrivial Trivial {} = pure True
isTrivial _ = pure False

trivial :: Term -> Sem r CT
trivial = pure . Trivial

classify :: Effs r => Term -> Sem r CT
classify old@Term {..} =
  case termTerm of
    Var {} -> trivial old
    Abs {} -> trivial old
    App f ts -> isAtomic f >>= \case
      True -> trivial old
      False -> pure $ SApp termLabel f ts
    Let as x t b -> ((,) <$> classify t <*> classify b) >>= \case
      -- (Trivial {}, Trivial {}) -> trivial old
      (t', b') -> pure $ SLet termLabel as x t' b'
    Case t ps -> do
      ps' <- traverse classify ps
      all isTrivial ps' >>= \case
        True -> trivial old
        False -> pure $ SCase termLabel t ps'
    Cons _ -> trivial old
    Panic -> pure $ SPanic termLabel

transformNormal :: Effs r => CT -> TermF Term -> Sem r Term
transformNormal tm k = case tm of
  Trivial t -> do
    t' <- transformAtomic t
    v <- freshVar "val"
    body <- term =<< App <$> term k <*> traverse term [Var v]
    term $ Let LetAnnot {letGenerated = True} (PVar v) t' body
  SApp termLabel f ts -> do
    k' <- term k
    pure Term {termTerm = App f (ts <> [k']), ..}
  SLet termLabel a x (Trivial t) b -> do
    t' <- transformAtomic t
    b' <- transformNormal b k
    pure Term {termTerm = Let a x t' b', ..}
  SLet _ _ (PVar x) t b -> do
    b' <- transformNormal b k
    let k' = Abs FunAnnot {funAtomic = False} $ scope [x] b'
    transformNormal t k'
  SLet _ _ p t b -> do
    v <- freshVar "val"
    b' <- transformNormal b k
    let (pat, xs) = extractNames p
    b'' <- term =<< Case <$> (term $ Var v) <*> pure (Patterns [(pat, scope xs b')])
    let k' = Abs FunAnnot {funAtomic = False} $ scope [v] b''
    transformNormal t k'
  SCase termLabel t ps ->
    case k of
      Var {} -> do
        ps' <- traverse (flip transformNormal k) ps
        pure Term {termTerm = Case t ps', ..}
      _ -> do
        k' <- freshVar "cont"
        ps' <- traverse (flip transformNormal (Var k')) ps
        let t' = Term {termTerm = Case t' ps', ..}
        term =<< Let LetAnnot {letGenerated = True} (PVar k') <$> term k <*> pure t'
  SPanic termLabel -> pure Term {termTerm = Panic, ..}

fromAnf :: Effs' r => Program Term -> Sem r (Program Term)
fromAnf p@Program {..} = do
  let cpsGlobals = Map.fromList $ programDefinitions <&> \case DefFun {..} -> (funName, funAnnot)
  let cpsTerms = programTerms p
  cpsAnalysis <- AbsInt.run p
  evalState Cps {..} do
    defs <- traverse aux (programDefinitions)
    main <- traverse transformAtomic programMain
    pure $ Program {programDefinitions = defs, programMain = main, ..}
  where
    aux :: Effs r => DefFun Term -> Sem r (DefFun Term)
    aux d@DefFun {funScope = Scope xs t, ..} =
      if funAtomic funAnnot
        then traverse transformAtomic d
        else do
          k <- freshVar "cont"
          t'' <- classify t
          t' <- transformNormal t'' (Var k)
          pure DefFun {funScope = Scope (xs <> [(k, Nothing)]) t', ..}
