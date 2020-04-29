module AbsInt.Runner (runEffs) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Optics
import Syntax hiding (Target (..), ValueF (..))
import Polysemy.State
import Polysemy.Error
import Util
import AbsInt.Types
import qualified AbsInt.Interpreter as Interpreter

type TEnv = Map Tp Label

alloc :: Common r => [Tp] -> Sem r TEnv
alloc [] = pure Map.empty
alloc (t : ts) = do
  ts' <- alloc ts
  l <- freshLabel
  if Map.member t ts'
    then throw $ ScopeError Nothing ("Multiple definitions of type: " <> pshow t)
    else pure $ Map.insert t l ts'

getTp :: Common r => TEnv -> Tp -> Sem r ValuePtr
getTp tps t = case Map.lookup t tps of
  Just l -> pure $ ValuePtr l
  Nothing -> throw $ ScopeError Nothing ("Reference to undefined type: " <> pshow t)

buildStore :: Common r => [DefData] -> [DefStruct] -> Sem r (Store Value, TEnv)
buildStore datas structs = do
  let aux DefData {..} = 
        dataName : (dataTypes >>= (either (const []) (\s -> [structName s])))
  tps <-
    alloc $ MkTp "Any" : Map.keys builtinTypes <> (datas >>= aux) <> fmap structName structs
  any <- getTp tps (MkTp "Any")
  let runData :: (Common r, VStore r) => DefData -> Sem r ()
      runData DefData {..} = do
        ValuePtr lbl <- getTp tps dataName
        for_ dataTypes \case
          Left t -> getTp tps t >>= flip copy lbl
          Right s -> runStruct s >>= flip copy lbl
      runStruct :: (Common r, VStore r) => DefStruct -> Sem r ValuePtr
      runStruct DefStruct {..} = do
        ValuePtr lbl <- getTp tps structName
        fields <- for structFields \case
          FieldName _ -> pure any
          FieldType t -> getTp tps t
          FieldBoth t _ -> getTp tps t
        insert lbl $ Record structName fields
        pure (ValuePtr lbl)
      step :: (Common r, VStore r) => Sem r ()
      step = do
        for_ (Map.toList builtinTypes) \case
          (tp, val) -> do
            (ValuePtr l) <- getTp tps tp
            insert l val
        for_ datas runData
        for_ structs runStruct
        for_ (toList tps) \l -> do
          copy (ValuePtr l) (unValuePtr any)

      go :: Common r => Store Value -> Sem r (Store Value)
      go store = do
        store' <- execState store step
        if store == store' then pure store else go store'
  (,tps) <$> go (Store Map.empty)

addBinders :: Common r => TEnv -> Env -> [(Var, Maybe Tp)] -> Sem r Env
addBinders tEnv env binders = case binders of
  [] -> pure env
  (x, Just t) : bs -> do
    v <- getTp tEnv t
    addBinders tEnv (Map.insert x v env) bs
  (x, Nothing) : bs -> do
    v <- getTp tEnv (MkTp "Any")
    addBinders tEnv (Map.insert x v env) bs

buildInitialConf ::
  Common r => TEnv -> DefFun Label -> Store Value -> Sem r (Store Value, Store Cont, Set Config)
buildInitialConf tEnv DefFun {..} vStore = do
  let Scope xs main = funScope
  env <- addBinders tEnv Map.empty xs
  let kStore = Store $ Map.singleton main $ Set.singleton Halt
  pure (vStore, kStore, Set.singleton $ Eval env main $ ContPtr main)

denormalize :: Common r => Term -> Sem r Label
denormalize Term {..} = do
  t <- traverse denormalize termTerm
  modify' (over' #terms $ Map.insert termLabel t)
  pure termLabel

runEffs :: Common r => Program Term -> Sem r (Store Value)
runEffs pgm = do
  p@Program {..} <- traverse denormalize pgm
  modify' (set #globals $ programScopes p)
  (vStore, tEnv) <- buildStore programDatatypes programStructs
  conf <- buildInitialConf tEnv programMain vStore
  Interpreter.run conf