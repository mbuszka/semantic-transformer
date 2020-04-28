module Pipeline.Scope
  ( check,
    checkProgram,
    fromSource,
  )
where

import qualified Data.Map as Map
import Polysemy.Error
import Polysemy.Writer
import Syntax
import Util
import qualified Syntax.Source as Source

type Env = Map Var RefersTo

type Eff r = Member (Error Err) r

check ::
  forall r t f.
  Eff r =>
  (t -> Sem r (TermF t, Maybe Loc)) ->
  (t -> Fvs -> TermF f -> Sem r f) ->
  Env ->
  DefFun t ->
  Sem r (DefFun f)
check unwrap wrap env (DefFun {..}) = do
  (_, s') <- runWriter (goS env funScope)
  pure $ DefFun {funScope = s', ..}
  where
    wrap' :: t -> Sem (Writer Fvs : r) (TermF f) -> Sem (Writer Fvs : r) f
    wrap' old new = do
      (fvs, new') <- listen new
      raise $ wrap old fvs new'
    go :: Env -> t -> Sem (Writer Fvs : r) f
    go env term = do
      (t, loc) <- raise $ unwrap term
      case t of
        Var v -> do
          when (Map.notMember v env) $ throw (ScopeError loc $ "Unknown variable: " <> pshow v)
          wrap' term do
            tell $ Map.singleton v (env Map.! v)
            pure (Var v)
        Abs a s -> do
          wrap' term $ Abs a <$> goS env s
        App f xs -> do
          wrap' term do
            App <$> go env f <*> traverse (go env) xs
        Let a x e s -> do
          wrap' term do
            s' <- goS env (scope [x] s)
            Let a x <$> go env e <*> pure (scopeBody s')
        Case e ps -> do
          wrap' term do
            Case <$> go env e <*> goP env ps
        _ -> do
          wrap' term $ traverse (go env) t
    goS :: Env -> Scope t -> Sem (Writer Fvs : r) (Scope f)
    goS env s = do
      let defined = Map.fromList $ fmap (,RefLocal) (scopeVars s)
      body <- censor (`Map.difference` defined) $ go (defined <> env) (scopeBody s)
      pure $ s $> body
    goP :: Env -> Patterns t -> Sem (Writer Fvs : r) (Patterns f)
    goP _ (Patterns []) = pure (Patterns [])
    goP env (Patterns ((p, s) : ps)) = do
      s' <- goS env s
      (Patterns ps') <- goP env (Patterns ps)
      pure (Patterns ((p, s') : ps'))

checkProgram ::
  Member (Error Err) r =>
  (t -> Sem r (TermF t, Maybe Loc)) ->
  (t -> Fvs -> TermF f -> Sem r f) ->
  Program t ->
  Sem r (Program f)
checkProgram unwrap wrap Program {..} = do
  let globals = 
        Map.fromList 
        $ programDefinitions
        <&> \DefFun {..} -> (funName, RefGlobal)
      env = globals <> (RefPrimOp <$ primOps)
  defs <- traverse (check unwrap wrap env) programDefinitions
  main <- check unwrap wrap env programMain
  pure $ Program {programDefinitions = defs, programMain = main, ..}

fromSource ::
  forall r. Members '[Error Err, FreshLabel] r => Program Source.SrcTerm -> Sem r (Program Term)
fromSource pgm = do
  let unwrap = Source.unwrap
      wrap _ _ t = Term t <$> freshLabel @r
  checkProgram unwrap wrap pgm