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
    insert,
    fromSource,
    toDbg,
  )
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Polysemy.Error
import Polysemy.State
import Util.Pretty
import Pipeline.Scope (checkProgram)
import Syntax hiding (ValueF (..))
import Syntax.Term
import Util

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
  | Record Tag [ValuePtr]
  | Number
  | String
  | Boolean
  deriving (Eq, Ord)

data Cont
  = CLet Var Env Label ContPtr
  | Halt
  deriving (Eq, Ord)

newtype ValuePtr = ValuePtr Label deriving (Eq, Ord, Pretty)

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

nextLabel :: Member (State Label) r => Sem r Label
nextLabel = do
  lbl@(Label x) <- get
  put (Label (x + 1))
  return lbl

fromSource :: Members '[Error Err] r => Program Term -> Sem r Abstract
fromSource = evalState (Label 0) . fromSource'

type TEnv = Map Tp Label

absType :: TEnv -> Tp -> Value
absType env tp = case tp of
  TInt -> Number
  TStr -> String
  TBool -> Boolean
  TRecord c ts -> Record c . fmap (ValuePtr . (env Map.!)) $ ts
  TData _ -> error "Cannot include another data type"

fromSource' ::
  Members '[Error Err, State Label] r => Program Term -> Sem r Abstract
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
  pgmDtps <- traverse (const nextLabel) programDatatypes
  (abstractDataStore, env) <- runState (Store Map.empty) do
    baseValues <- for [(TStr, String), (TInt, Number), (TBool, Boolean)] \case
      (name, value) -> do
        lbl <- nextLabel
        insert lbl value
        pure (name, lbl)
    let env = Map.union (Map.fromList baseValues) pgmDtps
    for_ (Map.toList programDatatypes) \case
      (name, types) -> do
        let vals = fmap (absType env) types
            lbl = env Map.! name
        traverse_ (insert lbl) vals
    pure env
  let Def _ (Scope xs main) = programMain
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
  pretty (Store vs) = pmap . fmap toList $ vs
