{-# LANGUAGE UndecidableInstances #-}

module AbsInt.Types where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Optics
import Polysemy.Error
import Polysemy.State
import Syntax hiding (Target (..), ValueF (..))
import Util

type Labeled = TermF Label

type Env = Map Var ValuePtr

newtype ValuePtr = ValuePtr {unValuePtr :: Label} deriving (Eq, Ord)

newtype ContPtr = ContPtr {unContPtr :: Label} deriving (Eq, Ord)

data AbsInt
  = AbsInt
      { absIntTerms :: Map Label Labeled,
        absIntGlobals :: Map Var (Scope Label)
      }

$(makeFieldLabels ''AbsInt)

data Value
  = Global Var
  | Closure Env Label
  | PrimOp PrimOp
  | Record Tp [ValuePtr]
  | Integer
  | String
  | Boolean
  deriving (Eq, Ord)

data Cont
  = CLet Env Var Label ContPtr
  | Halt
  deriving (Eq, Ord)

data Config
  = Eval Env Label ContPtr
  | Continue ValuePtr ContPtr
  deriving (Eq, Ord)

type VStore r = Member (State (Store Value)) r

type KStore r = Member (State (Store Cont)) r

type Common r = Members '[Error Err, State AbsInt, FreshLabel] r

newtype Store v = Store {unStore :: Map Label (Set v)}
  deriving (Eq, Ord)

instance Ord v => Semigroup (Store v) where
  Store l <> Store r = Store $ Map.unionWith (<>) l r

instance Ord v => Monoid (Store v) where
  mempty = Store mempty

insert' :: Ord v => Label -> v -> Store v -> Store v
insert' lbl v (Store vals) =
  Store (Map.insertWith (<>) lbl (Set.singleton v) vals)

insert :: (Member (State (Store v)) r, Ord v) => Label -> v -> Sem r ()
insert lbl v = modify' (insert' lbl v)

copy :: (Common r, VStore r) => ValuePtr -> Label -> Sem r ValuePtr
copy (ValuePtr src) dst = do
  (Store values) <- get @(Store Value)
  vs <- case Map.lookup src values of
    Nothing -> throw $ InternalError $ "No value for label: " <> pshow src
    Just vs -> pure vs
  put . Store . Map.insertWith (<>) dst vs $ values
  pure $ ValuePtr dst

term :: Common r => Label -> Sem r (TermF Label)
term lbl = gets (preview $ #terms % ix lbl) >>= \case
  Nothing -> error "Term not found"
  Just t -> pure t

builtinTypes :: Map Tp Value
builtinTypes =
  Map.fromList
    [ (MkTp "Integer", Integer),
      (MkTp "String", String),
      (MkTp "Boolean", Boolean)
    ]