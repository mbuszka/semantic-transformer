module ScopeCheck
  ( fromSource,
  )
where

import qualified Data.Map as Map
import Polysemy.Error
import Syntax
import Util

data Target
  = Global
  | Local
  | PrimOp PrimOp
  deriving (Eq, Ord)

type Env = Map Var Target

fromSource :: Member (Error Err) r => Program Term -> Sem r (Program Term)
fromSource (Program {..}) = do
  let prim =
        Map.fromList . fmap (\case (k, op) -> (SrcVar k, PrimOp op))
          . Map.toList
          $ primOps
  let env = Map.union (Global <$ programDefinitions) prim
  defs <- traverse (checkD env) programDefinitions
  main <- checkD env programMain
  pure $ Program { programDefinitions = defs, programMain = main, ..}

check :: Member (Error Err) r => Env -> Term -> Sem r Term
check env (Term loc tm) = Term loc <$> case tm of
  Var v -> do
    when (Map.notMember v env) $
      throw (ScopeError loc $ "Unknown variable: " <> pshow v)
    pure $ Var v
  Abs s -> Abs <$> checkS env s
  App f xs -> do
    f' <- check env f
    xs' <- traverse (check env) xs
    case unTerm f' of
      Var v -> case env Map.! v of
        PrimOp op -> pure $ Prim op xs'
        _ -> pure $ App f' xs'
      _ -> pure $ App f' xs'
  Let t s -> Let <$> check env t <*> checkS env s
  Case t ps -> Case <$> check env t <*> checkP env ps
  _ -> traverse (check env) tm

checkS :: Member (Error Err) r => Env -> Scope Term -> Sem r (Scope Term)
checkS env (Scope xs t) =
  let env' = (Map.fromList . fmap ((,Local) . fst) $ xs) `Map.union` env
   in Scope xs <$> check env' t

checkP ::
  Member (Error Err) r => Env -> Patterns Term -> Sem r (Patterns Term)
checkP env (Patterns ps) = Patterns <$> traverse (traverse (checkS env)) ps

checkD :: Member (Error Err) r => Env -> Def Term -> Sem r (Def Term)
checkD env (Def as s) = Def as <$> checkS env s