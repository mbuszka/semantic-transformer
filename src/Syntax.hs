module Syntax
  ( Annot (..),
    Bound (..),
    Branches (..),
    DefData (..),
    DefFun (..),
    DefStruct (..),
    FreshLabel,
    FreshVar,
    FunAnnot (..),
    Fvs,
    Label (..),
    LetAnnot (..),
    Pattern (..),
    PrimOp (..),
    Program (..),
    RefersTo (..),
    StructField (..),
    TermF (..),
    Term (..),
    Tp (..),
    ValueF (..),
    Var (..),
    defaultFunAnnot,
    freshLabel,
    freshTag,
    freshVar,
    mkTerm,
    mkTerm',
    primOpNames,
    primOps,
    patternVars,
    patternVarsSet,
    programFuns,
    programTerms,
    runFreshLabel,
    runFreshVar,
    tpToVar,
    varToTp,
  )
where

import Common
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

data Program t = Program
  { programDefinitions :: [DefFun t],
    programDatatypes :: [DefData],
    programStructs :: [DefStruct],
    programMain :: DefFun t
  }
  deriving (Functor, Foldable, Traversable)

data DefFun t = DefFun
  { funName :: Var,
    funAnnot :: FunAnnot,
    funVars :: [(Maybe Tp, Var)],
    funBody :: t
  }
  deriving (Functor, Foldable, Traversable)

data DefData = DefData {dataName :: Tp, dataTypes :: [Either Tp DefStruct]}

data DefStruct = DefStruct {structName :: Tp, structFields :: [StructField]}

data StructField = FieldName Var | FieldType Tp | FieldBoth Tp Var

data Annot
  = Atomic
  | NoDefun
  | DefunName Tp
  deriving (Eq, Ord)

data FunAnnot = FunAnnot
  { funDoCps :: Bool,
    funDoDefun :: Bool,
    funDefunName :: Maybe Tp
  }

data LetAnnot = LetAnnot
  { letGenerated :: Bool
  }

data PrimOp = Add | Sub | Mul | Div | Neg | And | Or | Not | Eq
  deriving (Eq, Ord)

type Fvs = Map Var RefersTo

data RefersTo = RefGlobal | RefPrimOp | RefLocal deriving (Eq, Ord)

data TermF t
  = Var Var
  | Abs FunAnnot [Var] t
  | App t [t]
  | Let LetAnnot Pattern t t
  | Case t (Branches t)
  | Cons (ValueF t)
  | Error Text
  deriving (Functor, Foldable, Traversable)

data Term = Term {termLabel :: Label, termLoc :: Maybe Loc, termF :: TermF Term}

data Pattern
  = PVar Var
  | PType Tp Var
  | PWild
  | PCons (ValueF Pattern)
  deriving (Eq, Ord)

data Branches t = Branch Pattern t (Branches t) | BNil
  deriving (Functor, Foldable, Traversable, Eq, Ord)

newtype Var = MkVar Text
  deriving (Eq, Ord, Show, Pretty)

newtype Tp = MkTp Text
  deriving (Eq, Ord, Show, Pretty)

data Scope t = Scope [(Var, Maybe Tp)] t
  deriving (Eq, Ord, Functor, Foldable, Traversable)

data ValueF t
  = Number Int
  | String Text
  | Boolean Bool
  | Record Tp [t]
  deriving (Eq, Ord, Functor, Foldable, Traversable)

-- Handling of bound variables
class Bound t where
  rename :: Map Var Var -> t -> t

instance Bound t => Bound (Scope t) where
  rename vars (Scope vs t) =
    let bound = Set.fromList (fmap fst vs)
        vars' = Map.withoutKeys vars bound
     in if Set.fromList (toList vars') `Set.disjoint` bound
          then Scope vs (rename vars' t)
          else error "Free variable would become bound by renaming"

instance Bound t => Bound (TermF t) where
  rename vars term = case term of
    Var v -> Var $ Map.findWithDefault v v vars
    Abs ann xs t ->
      Abs ann xs $ rename (Map.withoutKeys vars (Set.fromList xs)) t
    Let ann x t s ->
      let vars' = Map.withoutKeys vars (patternVarsSet x)
       in Let ann x (rename vars t) (rename vars' s)
    Case t ps -> Case (rename vars t) (rename vars ps)
    _ -> fmap (rename vars) term

instance Bound v => Bound (ValueF v) where
  rename vars = fmap (rename vars)

instance Bound t => Bound (Branches t) where
  rename vars (Branch p t bs) =
    let vars' = Map.withoutKeys vars (patternVarsSet p)
     in Branch p (rename vars' t) (rename vars bs)
  rename _ BNil = BNil

instance Bound Term where
  rename vars Term {..} = Term {termF = rename vars termF, ..}

patternVarsSet :: Pattern -> Set Var
patternVarsSet = Set.fromList . patternVars

patternVars :: Pattern -> [Var]
patternVars (PVar v) = [v]
patternVars (PType _ v) = [v]
patternVars PWild = []
patternVars (PCons c) = foldMap patternVars c

defaultFunAnnot :: FunAnnot
defaultFunAnnot = FunAnnot {funDoCps = True, funDoDefun = True, funDefunName = Nothing}

varToTp :: Var -> Tp
varToTp (MkVar v) = MkTp (Text.toTitle v)

tpToVar :: Tp -> Var
tpToVar (MkTp t) = MkVar (Text.toLower t)

programFuns :: Program t -> Map Var ([Var], t)
programFuns Program {..} =
  programDefinitions
    <&> (\DefFun {..} -> (funName, (fmap snd funVars, funBody)))
    & Map.fromList

programTerms :: Program Term -> Map Label Term
programTerms = foldMap aux
  where
    aux term@Term {..} = Map.insert termLabel term $ foldMap aux termF

primOps :: Map Var PrimOp
primOps =
  Map.fromList
    [ (MkVar "+", Add),
      (MkVar "-", Sub),
      (MkVar "*", Mul),
      (MkVar "/", Div),
      (MkVar "neg", Neg),
      (MkVar "and", And),
      (MkVar "not", Not),
      (MkVar "or", Or),
      (MkVar "eq?", Eq)
    ]

primOpNames :: Map PrimOp Var
primOpNames = Map.fromList $ fmap swap $ Map.toList primOps

instance Pretty PrimOp where
  pretty op =
    let ops = Map.fromList . fmap swap . Map.toList $ primOps
     in pretty $ ops Map.! op

-- Fresh variable generation

data FreshVar m a where
  Fresh :: Text -> FreshVar m Text

$(makeSem ''FreshVar)

freshVar :: Member FreshVar r => Text -> Sem r Var
freshVar t = fresh t <&> MkVar

freshTag :: Member FreshVar r => Text -> Sem r Tp
freshTag t = fresh t <&> MkTp

runFreshVar :: Member (Embed IO) r => Sem (FreshVar ': r) a -> Sem r a
runFreshVar sem = do
  ref <- embed $ newIORef (Map.empty :: Map Text Int)
  interpret
    ( \case
        Fresh t -> do
          names <- embed $ readIORef ref
          let (n, m) = case Map.lookup t names of
                Nothing -> (1, Map.insert t 1 names)
                Just x -> (x + 1, Map.insert t (x + 1) names)
          embed $ writeIORef ref m
          return (t <> pshow n)
    )
    sem

data FreshLabel m a where
  FreshLabel :: FreshLabel m Label

runFreshLabel :: Member (Embed IO) r => Sem (FreshLabel ': r) a -> Sem r a
runFreshLabel sem = do
  ref <- embed $ newIORef (0 :: Int)
  interpret
    ( \case
        FreshLabel -> do
          n <- embed $ readIORef ref
          embed $ writeIORef ref (n + 1)
          return (Label n)
    )
    sem

$(makeSem ''FreshLabel)

mkTerm :: Member FreshLabel r => Maybe Loc -> TermF Term -> Sem r Term
mkTerm termLoc termF = do
  termLabel <- freshLabel
  pure Term {..}

mkTerm' :: Member FreshLabel r => TermF Term -> Sem r Term
mkTerm' = mkTerm Nothing
