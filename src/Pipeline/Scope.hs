module Pipeline.Scope
  ( check,
    checkProgram,
  )
where

import qualified Data.Map as Map
import Polysemy.Error
import Polysemy.Writer
import Syntax
import Util

type Env = Map Var Target

type Eff r = Member (Error Err) r

check ::
  forall r t f.
  Eff r =>
  (t -> Sem r (TermF t, Maybe Loc)) ->
  (t -> Fvs -> TermF f -> Sem r f) ->
  Env ->
  Def t ->
  Sem r (Def f)
check unwrap wrap env (Def {..}) = do
  (_, s') <- runWriter (goS env defScope)
  pure $ Def {defScope = s', ..}
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
        Abs s -> do
          wrap' term $ Abs <$> goS env s
        App f xs -> do
          wrap' term do
            App <$> go env f <*> traverse (go env) xs
        Let e s -> do
          wrap' term do
            Let <$> go env e <*> goS env s
        Case e ps -> do
          wrap' term do
            Case <$> go env e <*> goP env ps
        _ -> do
          wrap' term $ traverse (go env) t
    goS :: Env -> Scope t -> Sem (Writer Fvs : r) (Scope f)
    goS env s = do
      let defined = Map.fromList $ fmap (,Local) (scopeVars s)
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
checkProgram unwrap wrap (Program {..}) = do
  let env = (Global <$ programDefinitions) <> (PrimOp <$ primOps)
  defs <- traverse (check unwrap wrap env) programDefinitions
  main <- check unwrap wrap env programMain
  pure $ Program {programDefinitions = defs, programMain = main, ..}
