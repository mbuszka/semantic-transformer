module Defun.Labeled
  ( module Syntax,
    Abstract (..),
    CacheDst (..),
    Cont (..),
    ContPtr (..),
    Dbg (..),
    Env,
    Label (..),
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
import Pretty
import Syntax hiding (Value (..))
import Util

data Abstract
  = Abstract
      { abstractProgram :: Program Label,
        abstractTerms :: Map Label Labeled,
        abstractInitEnv :: Env,
        abstractDataStore :: Store Value,
        abstractContStore :: Store Cont
      }

data Value
  = Closure Label Env [Var] Label
  | TopLevel Var
  | Struct (Record ValuePtr)
  | String
  | Number
  deriving (Eq, Ord)

data Cont
  = EvalApp Env [ValuePtr] [Label] CacheDst ContPtr
  | EvalCons Label Tag Env [ValuePtr] [Label] CacheDst ContPtr
  | EvalCase Env (Patterns Label) CacheDst ContPtr
  | Halt
  deriving (Eq, Ord)

newtype ValuePtr = ValuePtr Label deriving (Eq, Ord, Pretty)

newtype ContPtr = ContPtr Label deriving (Eq, Ord, Pretty)

newtype CacheDst = CacheDst Label deriving (Eq, Ord, Pretty)

newtype Store v = Store {unStore :: Map Label (Set v)}
  deriving (Eq, Ord)

newtype Label = Label {unLabel :: Int} deriving (Eq, Ord, Pretty)

type Env = Map Var ValuePtr

type Labeled = TermF Label

type Effs r = Members '[State Label, State (Map Label Labeled)] r

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

label :: Effs r => Term -> Sem r Label
label (Term t) = do
  lbl <- nextLabel
  t' <- traverse label t
  modify (Map.insert lbl t')
  pure lbl

fromSource :: Members '[Error Err] r => Program Term -> Sem r Abstract
fromSource = evalState (Label 0) . fromSource'

fromSource' ::
  Members '[Error Err, State Label] r => Program Term -> Sem r Abstract
fromSource' pgm = do
  (abstractTerms, abstractProgram) <- runState Map.empty . traverse label $ pgm
  let Program {..} = abstractProgram
  pgmDtps <- traverse (const nextLabel) programDatatypes
  (abstractDataStore, dtps) <- runState (Store Map.empty) do
    baseDtps <- for [(TStr, String), (TInt, Number)] \case
      (name, value) -> do
        lbl <- nextLabel
        insert lbl value
        pure (name, lbl)
    let dtps = Map.union (Map.fromList baseDtps) pgmDtps
    for_ (Map.toList programDatatypes) \case
      (name, records) -> do
        let rs' = fmap (Struct . fmap (ValuePtr . (dtps Map.!))) records
            lbl = dtps Map.! name
        traverse_ (insert lbl) rs'
    pure dtps
  let Def _ (Scope xs main) = programMain
  let abstractContStore = Store (Map.singleton main (Set.singleton Halt))
  let aux (_, Nothing) = throw (ModuleError "Missing type annotation in main")
      aux (x, Just t) = pure (x, ValuePtr (dtps Map.! t))
  abstractInitEnv <- fmap Map.fromList (traverse aux xs)
  pure $ Abstract {..}

toDbg :: Map Label Labeled -> Label -> Dbg
toDbg terms lbl = Dbg lbl (fmap (toDbg terms) (terms Map.! lbl))

instance Pretty Value where
  pretty (Closure lbl _ _ _) = "closure" <+> pretty lbl
  pretty (TopLevel x) = pretty x
  pretty (Struct r) = pretty r
  pretty String = "string"
  pretty Number = "number"

instance Pretty Cont where
  pretty (EvalApp _ _ _ _ k) = parens ("EvalApp" <> pretty k)
  pretty Halt = "Halt"
  pretty (EvalCons _ _ _ _ _ _ k) = parens ("EvalCons" <+> pretty k)
  pretty (EvalCase _ _ _ k) = parens ("EvalCase" <+> pretty k)

instance Pretty Dbg where
  pretty (Dbg lbl t) = pretty lbl <> "#" <> pretty t
