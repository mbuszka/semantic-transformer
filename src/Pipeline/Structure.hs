module Pipeline.Structure
  ( validate,
  )
where

import qualified Data.Set as Set
import Polysemy.Error
import Syntax
import Syntax.Source
import Util

data Acc k v = Acc (Set k) [v]

empty :: Acc k v
empty = Acc Set.empty []

data Builder t
  = B
      { definitions :: Acc Var (DefFun SrcTerm),
        datatypes :: Acc Tp DefData,
        main :: Maybe (DefFun t),
        structs :: Acc Tp DefStruct
      }

validate ::
  forall r. Member (Error Err) r => [TopLevel] -> Sem r (Program SrcTerm)
validate topLevels = do
  B {..} <- aux topLevels $ B empty empty Nothing empty
  case main of
    Nothing -> throw $ ModuleError "No main in file"
    Just main ->
      pure $
        Program
          { programDefinitions = values definitions,
            programDatatypes = values datatypes,
            programMain = main,
            programStructs = values structs
          }
  where
    aux :: [TopLevel] -> Builder SrcTerm -> Sem r (Builder SrcTerm)
    aux [] b = pure b
    aux (TFun _ f@DefFun {funName = MkVar "main"} : ts) B {main = Nothing, ..} =
      aux ts B {main = Just f, ..}
    aux (TFun l DefFun {funName = MkVar "main"} : _) B {main = Just _} =
      throw $ ScopeError (Just l) "Redefinition of main"
    aux (TFun l f : ts) B {..} = do
      definitions' <- insert funName f definitions $ ScopeError (Just l) "Redefinition of a function"
      aux ts B {definitions = definitions', ..}
    aux (TData l d : ts) B {..} = do
      datatypes' <- insert dataName d datatypes $ ScopeError (Just l) "Redefinition of a data type"
      aux ts B {datatypes = datatypes', ..}
    aux (TStruct l s : ts) B {..} = do
      structs' <- insert structName s structs $ ScopeError (Just l) "Redefinition of a struct"
      aux ts B {structs = structs', ..}

insert ::
  (Ord k, Member (Error Err) r) => (v -> k) -> v -> Acc k v -> Err -> Sem r (Acc k v)
insert prj v (Acc known vs) err = case Set.member (prj v) known of
  True -> throw err
  False -> pure $ Acc (Set.insert (prj v) known) (v : vs)

values :: Acc k v -> [v]
values (Acc _ vs) = vs