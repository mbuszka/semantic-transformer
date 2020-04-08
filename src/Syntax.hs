module Syntax
  ( Annot (..),
    Bound (..),
    Def (..),
    FreshVar,
    Pattern (..),
    Patterns (..),
    PrimOp (..),
    Pretty (..),
    Program (..),
    Scope (..),
    Tag (..),
    Term (..),
    TermF (..),
    TestCase (..),
    Tp (..),
    Value (..),
    ValueF (..),
    Var (..),
    extractNames,
    freshVar,
    mkTerm,
    mkVar,
    insertNames,
    primOps,
    runFreshVar,
    scope,
  )
where

import Control.Monad.State.Strict
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Set as Set
import Pretty
import Util

data Program t
  = Program
      { programDefinitions :: Map Var (Def t),
        programDatatypes :: Map Tp [Tp],
        programTests :: [TestCase],
        programMain :: Def t
      }
  deriving (Functor, Foldable, Traversable)

data Def t = Def {defAnnotations :: Set Annot, defScope :: Scope t}
  deriving (Functor, Foldable, Traversable)

data Annot
  = NoCps
  deriving (Eq, Ord)

data TestCase = TestCase Text [Value] Value

data PrimOp = Add | Sub | Mul | Div | Neg | And | Or | Not | Eq
  deriving (Eq, Ord)

data TermF t
  = Var Var
  | Abs (Scope t)
  | App t [t]
  | Prim PrimOp [t]
  | Let t (Scope t)
  | Case t (Patterns t)
  | Cons (ValueF t)
  | Panic
  deriving (Functor, Foldable, Traversable)

data Pattern v
  = PVar v (Maybe Tp)
  | PWild
  | PCons (ValueF (Pattern v))
  deriving (Functor, Foldable, Eq, Ord)

newtype Patterns t = Patterns [(Pattern (), Scope t)]
  deriving (Functor, Foldable, Traversable, Eq, Ord)

data Var = SrcVar Text | GenVar Text Int
  deriving (Eq, Ord, Show)

data Tag = SrcTag Text | GenTag Int | TopTag Var
  deriving (Eq, Ord, Show)

data Tp
  = TData Text
  | TInt
  | TStr
  | TBool
  | TRecord Tag [Tp]
  deriving (Eq, Ord)

data Scope t = Scope [(Var, Maybe Tp)] t
  deriving (Eq, Ord, Functor, Foldable, Traversable)

data ValueF t
  = Number Int
  | String Text
  | Boolean Bool
  | Record Tag [t]
  deriving (Eq, Ord, Functor, Foldable, Traversable)

newtype Value = Value {unValue :: ValueF Value}
  deriving (Eq, Ord, Bound, Pretty)

-- Various syntax representations
data Term
  = Term
      { termLoc :: Maybe Loc,
        unTerm :: TermF Term
      }

instance Pretty Term where
  pretty = pretty . unTerm

instance Bound Term where
  freeVars = freeVars . unTerm

  rename vs (Term l t) = Term l (rename vs t)

-- Handling of bound variables
class Bound t where
  freeVars :: t -> Set Var

  rename :: Map Var Var -> t -> t

instance Bound t => Bound (Scope t) where
  freeVars (Scope vs t) = freeVars t Set.\\ (Set.fromList . fmap fst $ vs)

  rename vars (Scope vs t) =
    let bound = Set.fromList (fmap fst vs)
        vars' = Map.withoutKeys vars bound
     in if Set.fromList (toList vars') `Set.disjoint` bound
          then Scope vs (rename vars' t)
          else error "Free variable would become bound by renaming"

instance Bound t => Bound (TermF t) where
  freeVars term = case term of
    Var v -> Set.singleton v
    Abs s -> freeVars s
    Let t s -> freeVars t `Set.union` freeVars s
    Case t ps -> freeVars t `Set.union` freeVars ps
    _ -> foldMap freeVars term

  rename vars term = case term of
    Var v -> Var $ Map.findWithDefault v v vars
    Abs s -> Abs $ rename vars s
    Let t s -> Let (rename vars t) (rename vars s)
    Case t ps -> Case (rename vars t) (rename vars ps)
    _ -> fmap (rename vars) term

instance Bound v => Bound (ValueF v) where
  freeVars (Record _ ts) = foldMap freeVars ts
  freeVars _ = Set.empty

  rename vars (Record c ts) = Record c (fmap (rename vars) ts)
  rename _ v = v

instance Bound t => Bound (Patterns t) where
  freeVars (Patterns ps) = foldMap (freeVars . snd) ps

  rename vars (Patterns ps) = Patterns (fmap (fmap (rename vars)) ps)

-- Pattern helpers
extractNames :: Pattern Var -> (Pattern (), [Var])
extractNames p = (p $> (), toList p)

insertNames :: Pattern a -> [Var] -> Pattern Var
insertNames pattern vars = case go pattern vars of
  (p, []) -> p
  _ -> error "Too many variables to insert into pattern"
  where
    go (PVar _ tp) (v : vs) = (PVar v tp, vs)
    go (PVar _ _) [] = error "Not enough variables to insert into pattern"
    go PWild vs = (PWild, vs)
    go (PCons r) vs = runState (PCons <$> traverse (state . go) r) vs

mkVar :: Text -> Var
mkVar = SrcVar

-- Smart constructors
scope :: [Var] -> t -> Scope t
scope vs = Scope (fmap (,Nothing) vs)

mkTerm :: TermF Term -> Term
mkTerm = Term Nothing

primOps :: Map Text PrimOp
primOps =
  Map.fromList
    [ ("add", Add),
      ("sub", Sub),
      ("mul", Mul),
      ("div", Div),
      ("neg", Neg),
      ("and", And),
      ("not", Not),
      ("or", Or),
      ("eq", Eq)
    ]

-- Pretty printing
instance Pretty Var where
  pretty (SrcVar v) = pretty v
  pretty (GenVar t n) = pretty t <> "-" <> pretty n

instance Pretty (Pattern Var) where
  pretty (PVar v tp) = variable (v, tp)
  pretty PWild = "_"
  pretty (PCons r) = pretty r

instance Pretty t => Pretty (TermF t) where
  pretty = prettyTerm

instance Pretty Annot where
  pretty NoCps = "@no-cps"

instance Pretty t => Pretty (Program t) where
  pretty Program {..} =
    types <> hardline <> main <> hardline <> defs <> hardline <> tests
    where
      defs = rows . fmap def . Map.toList $ programDefinitions
      def (name, Def _ (Scope vs t)) =
        hardline
          <> parens ("def" <+> pretty name <> body (variables vs) (pretty t))
      types = rows . fmap typ . Map.toList $ programDatatypes
      typ (name, records) =
        parens
          ( "def-data"
              <+> pretty name <> nested 2 (rows . fmap pretty $ records)
          )
          <> hardline
      main = parens ("def main" <> body (variables mvs) (pretty mb))
      Def _ (Scope mvs mb) = programMain
      tests = rows . fmap pretty $ programTests

instance Pretty v => Pretty (ValueF v) where
  pretty (Number n) = pretty n
  pretty (String s) = escape s
  pretty (Record c ts) = braces (pretty c <> (nested' 2 ts))
  pretty (Boolean True) = "#t"
  pretty (Boolean False) = "#f"

instance Pretty TestCase where
  pretty (TestCase desc inputs output) =
    let vs = aligned' [parens (aligned inputs), pretty output]
     in parens ("def-test" <+> escape desc <> nested 2 vs)

instance Pretty t => Pretty (Patterns t) where
  pretty (Patterns ps) = case ps of
    [] -> mempty
    [p] -> pat p
    _ -> rows . fmap pat $ ps
    where
      pat (p, Scope vs t) =
        parens (pretty (insertNames p . fmap fst $ vs) <> nested 1 (pretty t))

instance Pretty t => Pretty (Scope t) where
  pretty (Scope xs t) = variables xs <+> pretty t

instance Pretty Tag where
  pretty (SrcTag c) = pretty c
  pretty (GenTag lbl) = "lam-" <> pretty lbl
  pretty (TopTag v) = pretty v

instance Pretty Tp where
  pretty (TRecord c ts) = braces (pretty c <> (nested' 2 ts))
  pretty TInt = "integer"
  pretty TStr = "string"
  pretty TBool = "boolean"
  pretty (TData t) = pretty t

instance Pretty PrimOp where
  pretty op =
    let ops = Map.fromList . fmap swap . Map.toList $ primOps
     in pretty $ ops Map.! op

prettyTerm :: Pretty t => TermF t -> Doc ann
prettyTerm term = case term of
  Var v -> pretty v
  Cons v -> pretty v
  Abs (Scope vs t) ->
    parens ("fun" <> body (variables vs) (pretty t))
  App f ts ->
    parens (pretty f <> nested' 2 ts)
  Prim op ts ->
    "#" <> parens (pretty op <> nested' 2 ts)
  Let t (Scope [v] b) ->
    parens ("let" <+> pretty v <> body (pretty t) (pretty b))
  Let _ _ ->
    error "Let should bind only a single variable"
  Case t ps ->
    parens ("case" <> body (pretty t) (pretty ps))
  Panic -> "panic"

variable :: (Var, Maybe Tp) -> Doc ann
variable (v, Nothing) = pretty v
variable (v, Just tp) = brackets . aligned' $ [pretty v, pretty tp]

variables :: [(Var, Maybe Tp)] -> Doc ann
variables = parens . aligned' . fmap variable

-- Fresh variable generation
data FreshVar m a where
  FreshVar :: Text -> FreshVar m Var

$(makeSem ''FreshVar)

runFreshVar :: Member (Embed IO) r => Sem (FreshVar ': r) a -> Sem r a
runFreshVar sem = do
  ref <- embed $ newIORef (Map.empty :: Map Text Int)
  interpret
    ( \case
        FreshVar t -> do
          names <- embed $ readIORef ref
          let (n, m) = case Map.lookup t names of
                Nothing -> (1, Map.insert t 1 names)
                Just x -> (x + 1, Map.insert t (x + 1) names)
          embed $ writeIORef ref m
          return (GenVar t n)
    )
    sem
