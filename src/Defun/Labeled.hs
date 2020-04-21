module Defun.Labeled
  ( module Syntax,
    Abstract (..),
    Cont (..),
    ContPtr (..),
    Dbg (..),
    Env,
    Labeled,
    Store (..),
    Value (..),
    ValuePtr (..),
    builtinTypes,
    copy,
    insert,
    fromSource,
    toDbg,
  )
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Pipeline.Scope (checkProgram)
import Polysemy.Error
import Polysemy.State
import Syntax hiding (ValueF (..))
import Syntax.Term
import Util
import Util.Pretty

data Abstract
  = Abstract
      { abstractProgram :: Program Label,
        abstractTerms :: Map Label Labeled,
        abstractFvs :: Map Label Fvs,
        abstractInitEnv :: Env,
        abstractDataStore :: Store Value,
        abstractContStore :: Store Cont
      }

data Value
  = Closure Label Env [Var] Label
  | Prim Var
  | TopLevel Var
  | Record Tp [ValuePtr]
  | Number
  | String
  | Boolean
  deriving (Eq, Ord)

data Cont
  = CLet Var Env Label ContPtr
  | Halt
  deriving (Eq, Ord)

newtype ValuePtr = ValuePtr {unValuePtr :: Label} deriving (Eq, Ord, Pretty)

newtype ContPtr = ContPtr Label deriving (Eq, Ord, Pretty)

newtype Store v = Store {unStore :: Map Label (Set v)}
  deriving (Eq, Ord)

type Env = Map Var ValuePtr

type Labeled = TermF Label

data Dbg = Dbg Label (TermF Dbg)

insert' :: Ord v => Label -> v -> Store v -> Store v
insert' lbl v (Store vals) =
  Store (Map.insertWith (<>) lbl (Set.singleton v) vals)

insert :: (Member (State (Store v)) r, Ord v) => Label -> v -> Sem r ()
insert lbl v = modify' (insert' lbl v)

copy :: Member (State (Store Value)) r => ValuePtr -> Label -> Sem r ValuePtr
copy (ValuePtr src) dst = do
  (Store values) <- get @(Store Value)
  put . Store . Map.insertWith (<>) dst (values Map.! src) $ values
  pure $ ValuePtr dst

nextLabel :: Member (State Label) r => Sem r Label
nextLabel = do
  lbl@(Label x) <- get
  put (Label (x + 1))
  return lbl

fromSource :: Members '[Error Err] r => Program Term -> Sem r Abstract
fromSource = evalState (Label 0) . fromSource'

type TEnv = Map Tp Label

builtinTypes :: Map Tp Value
builtinTypes =
  Map.fromList
    [ (MkTp "Integer", Number),
      (MkTp "String", String),
      (MkTp "Boolean", Boolean)
    ]

alloc :: Effs r => [Tp] -> Sem r TEnv
alloc [] = pure Map.empty
alloc (t : ts) = do
  ts' <- alloc ts
  l <- nextLabel
  if Map.member t ts'
    then throw $ ScopeError Nothing ("Multiple definitions of type: " <> pshow t)
    else pure $ Map.insert t l ts'

getTp :: Effs r => TEnv -> Tp -> Sem r ValuePtr
getTp tps t = case Map.lookup t tps of
  Just l -> pure $ ValuePtr l
  Nothing -> throw $ ScopeError Nothing ("Reference to undefined type: " <> pshow t)

type Effs r = Members '[Error Err, State Label] r

type Effs' r = (Effs r, Member (State (Store Value)) r)

buildStore :: Effs r => [DefData] -> [DefStruct] -> Sem r (Store Value, TEnv)
buildStore datas structs = do
  let aux DefData {..} = 
        dataName : (dataTypes >>= (either (const []) (\s -> [structName s])))
  tps <-
    alloc $ MkTp "Any" : Map.keys builtinTypes <> (datas >>= aux) <> fmap structName structs
  any <- getTp tps (MkTp "Any")
  let runData :: Effs' r => DefData -> Sem r ()
      runData DefData {..} = do
        ValuePtr lbl <- getTp tps dataName
        for_ dataTypes \case
          Left t -> getTp tps t >>= flip copy lbl
          Right s -> runStruct s >>= flip copy lbl
      runStruct :: Effs' r => DefStruct -> Sem r ValuePtr
      runStruct DefStruct {..} = do
        ValuePtr lbl <- getTp tps structName
        fields <- for structFields \case
          FieldName _ -> pure any
          FieldType t -> getTp tps t
          FieldBoth t _ -> getTp tps t
        insert lbl $ Record structName fields
        pure (ValuePtr lbl)
      step :: Effs' r => Sem r ()
      step = do
        for_ (Map.toList builtinTypes) \case
          (tp, val) -> do
            (ValuePtr l) <- getTp tps tp
            insert l val
        for_ datas runData
        for_ structs runStruct
        for_ (toList tps) \l -> do
          copy (ValuePtr l) (unValuePtr any)

      go :: Effs r => Store Value -> Sem r (Store Value)
      go store = do
        store' <- execState store step
        if store == store' then pure store else go store'
  (,tps) <$> go (Store Map.empty)

fromSource' :: Effs r => Program Term -> Sem r Abstract
fromSource' pgm = do
  let unwrap t = pure (unTerm t, Nothing)
      wrap _ fvs new = do
        lbl <- nextLabel
        modify (Map.insert lbl new)
        modify (Map.insert lbl fvs)
        pure lbl
  (abstractTerms, (abstractFvs, abstractProgram)) <-
    runState (Map.empty @Label @(TermF Label))
      . runState (Map.empty @Label @Fvs)
      $ checkProgram unwrap wrap pgm
  let Program {..} = abstractProgram
  (abstractDataStore, env) <- buildStore programDatatypes programStructs
  let DefFun {funScope = Scope xs main} = programMain
  let abstractContStore = Store (Map.singleton main (Set.singleton Halt))
  let aux (_, Nothing) = throw (ModuleError "Missing type annotation in main")
      aux (x, Just t) = pure (x, ValuePtr (env Map.! t))
  abstractInitEnv <- fmap Map.fromList (traverse aux xs)
  pure $ Abstract {..}

toDbg :: Map Label Labeled -> Label -> Dbg
toDbg terms lbl = Dbg lbl (fmap (toDbg terms) (terms Map.! lbl))

instance Pretty Value where
  pretty (Closure lbl _ _ _) = "closure" <+> pretty lbl
  pretty (TopLevel x) = parens $ "top-level" <+> pretty x
  pretty (Prim op) = parens $ "prim-op" <+> pretty op
  pretty (Record c ts) = braces (pretty c <> (nested' 2 ts))
  pretty String = "string"
  pretty Number = "number"
  pretty Boolean = "boolean"

instance Pretty Cont where
  pretty (CLet v _ l k) = parens ("let" <+> pretty v <+> pretty l <+> pretty k)
  pretty Halt = "Halt"

instance Pretty Dbg where
  pretty (Dbg lbl t) = pretty lbl <> "#" <> pretty t

instance Pretty v => Pretty (Store v) where
  pretty (Store vs) = prettyMap . fmap toList $ vs
