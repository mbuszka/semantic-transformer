module Defun.Labeled
  ( module Syntax,
    AbsPgm (..),
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
import Polysemy
import Polysemy.State
import Pretty
import Syntax hiding (Value (..))

data AbsPgm
  = AbsPgm
      { apgmDefinitions :: Map Var (Def Label),
        apgmTerms :: Map Label Labeled,
        apgmDatatypes :: Map Tp [Record Tp],
        apgmTests :: [TestCase],
        apgmMain :: Def Label,
        apgmInitEnv :: Env,
        apgmDataStore :: Store Value,
        apgmContStore :: Store Cont
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

fromSource :: Program Term -> AbsPgm
fromSource = run . evalState (Label 0) . fromSource'

fromSource' :: Member (State Label) r => Program Term -> Sem r AbsPgm
fromSource' pgm = do
  (apgmTerms, Program {..}) <- runState Map.empty . traverse label $ pgm
  pgmDtps <- traverse (const nextLabel) pgmDatatypes
  (apgmDataStore, dtps) <- runState (Store Map.empty) do
    baseDtps <- for [(TStr, String), (TInt, Number)] \case
      (name, value) -> do
        lbl <- nextLabel
        insert lbl value
        pure (name, lbl)
    let dtps = Map.union (Map.fromList baseDtps) pgmDtps
    for_ (Map.toList pgmDatatypes) \case
      (name, records) -> do
        let rs' = fmap (Struct . fmap (ValuePtr . (dtps Map.!))) records
            lbl = dtps Map.! name
        traverse_ (insert lbl) rs'
    pure dtps
  let Def _ (Scope xs main) = pgmMain
  let apgmContStore = Store (Map.singleton main (Set.singleton Halt))
  let apgmInitEnv =
        Map.fromList (xs <&> \case (x, Just t) -> (x, ValuePtr (dtps Map.! t)))
  let apgmDefinitions = pgmDefinitions
  let apgmDatatypes = pgmDatatypes
  let apgmTests = pgmTests
  let apgmMain = pgmMain
  pure $ AbsPgm {..}

toDbg :: Map Label Labeled -> Label -> Dbg
toDbg terms lbl = Dbg lbl (fmap (toDbg terms) (terms Map.! lbl))

instance Pretty Value where
  pretty (Closure lbl _ _ _) = "closure" <+> pretty lbl
  pretty (TopLevel x) = pretty x
  pretty (Struct r) = pretty r
  pretty String = "string"
  pretty Number = "number"

instance Pretty Cont where
  pretty (EvalApp _ vs es _ k) = parens ("EvalApp" <> pretty k)
  pretty Halt = "Halt"
  pretty (EvalCons _ _ _ _ _ _ k) = parens ("EvalCons" <+> pretty k)
  pretty (EvalCase _ _ _ k) = parens ("EvalCase" <+> pretty k)

instance Pretty Dbg where
  pretty (Dbg lbl t) = pretty lbl <> "#" <> pretty t