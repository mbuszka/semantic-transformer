module Syntax
  ( Annot (..),
    Bound (..),
    Def (..),
    FreshVar,
    Located (..),
    Pattern (..),
    Patterns (..),
    Pretty (..),
    Program (..),
    Record (..),
    Scope (..),
    Tag (..),
    Term (..),
    TermF (..),
    TestCase (..),
    Tp (..),
    Value (..),
    Var (..),
    extractNames,
    forget,
    freshVar,
    mkVar,
    insertNames,
    runFreshVar,
    scope,
  )
where

import Control.Monad.State
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Set as Set
import Pretty
import Util

data Program t
  = Program
      { programDefinitions :: Map Var (Def t),
        programDatatypes :: Map Tp [Record Tp],
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

data Record t = Record Tag [t]
  deriving (Eq, Ord, Functor, Foldable, Traversable, Show)

data TermF t
  = Var Var
  | Abs (Scope t)
  | App t [t]
  | Let t (Scope t)
  | Case t (Patterns t)
  | Cons (Record t)
  | Panic
  deriving (Functor, Foldable, Traversable)

data Pattern v
  = PVar v
  | PWild
  | PCons (Record (Pattern v))
  deriving (Functor, Foldable, Eq, Ord)

newtype Patterns t = Patterns [(Pattern (), Scope t)]
  deriving (Functor, Foldable, Traversable, Eq, Ord)

data Var = SrcVar Text | GenVar Int
  deriving (Eq, Ord, Show)

data Tag = SrcTag Text | GenTag Int | TopTag Var
  deriving (Eq, Ord, Show)

data Tp
  = TStruct Text
  | TInt
  | TStr
  deriving (Eq, Ord)

data Scope t = Scope [(Var, Maybe Tp)] t
  deriving (Eq, Ord, Functor, Foldable, Traversable)

data Value
  = Number Int
  | String Text
  | Struct (Record Value)

-- Various syntax representations
newtype Term = Term {unTerm :: TermF Term}
  deriving (Bound, Pretty)

data Located = Located {locatedLoc :: Loc, locatedTerm :: TermF Located}

forget :: Located -> Term
forget (Located _ t) = Term (fmap forget t)

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

instance Bound t => Bound (Record t) where
  freeVars (Record _ ts) = foldMap freeVars ts

  rename vars = fmap (rename vars)

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

instance Bound t => Bound (Patterns t) where
  freeVars (Patterns ps) = foldMap (freeVars . snd) ps

  rename vars (Patterns ps) = Patterns (fmap (fmap (rename vars)) ps)

-- Pattern helpers
extractNames :: Pattern Var -> (Pattern (), [Var])
extractNames p = (p $> (), toList p)

insertNames :: Pattern a -> [Var] -> Pattern Var
insertNames pattern vars = case go pattern vars of
  (p, []) -> p
  _ -> error "Mismatched variable count"
  where
    go (PVar _) (v : vs) = (PVar v, vs)
    go (PVar _) [] = error "Mismatched pattern variables"
    go PWild vs = (PWild, vs)
    go (PCons r) vs = runState (PCons <$> traverse (state . go) r) vs

mkVar :: Text -> Var
mkVar = SrcVar

-- Smart constructors
scope :: [Var] -> t -> Scope t
scope vs = Scope (fmap (,Nothing) vs)

-- Pretty printing
instance Pretty Var where
  pretty (SrcVar v) = pretty v
  pretty (GenVar n) = "gen-" <> pretty n

instance Pretty v => Pretty (Pattern v) where
  pretty (PVar v) = pretty v
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

instance Pretty Value where
  pretty (Number n) = pretty n
  pretty (String s) = escape s
  pretty (Struct r) = pretty r

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

instance Pretty t => Pretty (Record t) where
  pretty (Record c ts) = braces (pretty c <> (nested' 2 ts))

instance Pretty t => Pretty (Scope t) where
  pretty (Scope xs t) = variables xs <+> pretty t

instance Pretty Tag where
  pretty (SrcTag c) = pretty c
  pretty (GenTag lbl) = "lam-" <> pretty lbl
  pretty (TopTag v) = pretty v

instance Pretty Tp where
  pretty (TStruct t) = pretty t
  pretty TInt = "integer"
  pretty TStr = "string"

prettyTerm :: Pretty t => TermF t -> Doc ann
prettyTerm term = case term of
  Var v ->
    pretty v
  Abs (Scope vs t) ->
    parens ("fun" <> body (variables vs) (pretty t))
  App f ts ->
    parens (pretty f <> nested' 2 ts)
  Let t (Scope [v] b) ->
    parens ("let" <+> pretty v <> body (pretty t) (pretty b))
  Cons r ->
    pretty r
  Case t ps ->
    parens ("case" <> body (pretty t) (pretty ps))
  Panic -> "panic"
  _ -> error "Unexpected syntax"

variables :: [(Var, Maybe Tp)] -> Doc ann
variables = parens . aligned' . fmap aux
  where
    aux (v, Nothing) = pretty v
    aux (v, Just tp) = brackets . aligned' $ [pretty v, pretty tp]

-- Fresh variable generation
data FreshVar m a where
  FreshVar :: FreshVar m Var

$(makeSem ''FreshVar)

runFreshVar :: Member (Embed IO) r => Sem (FreshVar ': r) a -> Sem r a
runFreshVar sem = do
  ref <- embed $ newIORef (0 :: Int)
  interpret
    ( \case
        FreshVar -> do
          x <- embed $ readIORef ref
          embed $ writeIORef ref (x + 1)
          return (GenVar x)
    )
    sem
