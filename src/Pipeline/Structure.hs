module Pipeline.Structure
  ( validate
  )
where

import qualified Data.Map as Map
import Polysemy.Error
import Syntax
import Syntax.Source
import Util

validate ::
  forall r. Member (Error Err) r => [TopLevel] -> Sem r (Program SrcTerm)
validate topLevels = do
  (defs, types, main) <- aux Map.empty Map.empty Nothing topLevels
  pure $
    Program
      { programDefinitions = defs,
        programDatatypes = types,
        programMain = main,
        programStructs = []
      }
  where
    aux defs types main ts = case (main, ts) of
      (Nothing, []) -> throw @Err @r (ModuleError "No main in file")
      (Just m, []) -> pure (defs, types, m)
      (Nothing, TDef _ (MkVar "main", def) : ts') ->
        aux defs types (Just def) ts'
      (Just _, TDef l (MkVar "main", _) : _) ->
        throw (ScopeError (Just l) "Redefinition of main")
      (_, TDef l (v, def) : ts') -> case Map.lookup v defs of
        Nothing -> aux (Map.insert v def defs) types main ts'
        Just _ -> throw (ScopeError (Just l) "Redefinition of a top-level function")
      (_, TDecl l (n, d) : ts') -> case Map.lookup n types of
        Nothing -> aux defs (Map.insert n d types) main ts'
        Just _ -> throw (ScopeError (Just l) "Redefinition of a data type")
