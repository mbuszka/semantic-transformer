module Pipeline.Structure
  ( validate,
  )
where

import qualified Data.Set as Set
import Polysemy.Error
import Syntax
import Syntax.Source
import Common

data Acc k v = Acc (Set k) [v]

type Effs r = Members [FreshLabel, Error Err] r

empty :: Acc k v
empty = Acc Set.empty []

data Builder t
  = B
      { definitions :: Acc Var (DefFun SrcTerm),
        datatypes :: Acc Tp DefData,
        main :: Maybe (DefFun t),
        structs :: Acc Tp DefStruct
      }


validate :: forall r. Effs r => [TopLevel] -> Sem r (Program Term)
validate topLevels = do
  B {..} <- aux topLevels $ B empty empty Nothing empty
  case main of
    Nothing -> throwMsg "No main in file"
    Just main -> do
      let pgm =
            Program
              { programDefinitions = values definitions,
                programDatatypes = values datatypes,
                programMain = main,
                programStructs = values structs
              }
          f SrcTerm {..} = do
            termLabel <- freshLabel @r
            termF <- traverse f srcTerm
            pure Term {termLoc = Just srcLoc, ..}
      traverse f pgm
  where
    aux :: [TopLevel] -> Builder SrcTerm -> Sem r (Builder SrcTerm)
    aux [] b = pure b
    aux (TFun _ f@DefFun {funName = MkVar "main"} : ts) B {main = Nothing, ..} =
      aux ts B {main = Just f, ..}
    aux (TFun l DefFun {funName = MkVar "main"} : _) B {main = Just _} =
      throw $ Err (Just l) Nothing "Redefinition of main"
    aux (TFun l f : ts) B {..} = do
      definitions' <- insert funName f definitions $ Err (Just l) Nothing "Redefinition of a function"
      aux ts B {definitions = definitions', ..}
    aux (TData l d : ts) B {..} = do
      datatypes' <- insert dataName d datatypes $ Err (Just l) Nothing "Redefinition of a data type"
      aux ts B {datatypes = datatypes', ..}
    aux (TStruct l s : ts) B {..} = do
      structs' <- insert structName s structs $ Err (Just l) Nothing "Redefinition of a struct"
      aux ts B {structs = structs', ..}

insert ::
  (Ord k, Member (Error Err) r) => (v -> k) -> v -> Acc k v -> Err -> Sem r (Acc k v)
insert prj v (Acc known vs) err = case Set.member (prj v) known of
  True -> throw err
  False -> pure $ Acc (Set.insert (prj v) known) (v : vs)

values :: Acc k v -> [v]
values (Acc _ vs) = vs