module Syntax
  ( Annot (..),
    Bound (..),
    DataDecl (..),
    Def (..),
    FreshVar,
    Pattern (..),
    Patterns (..),
    Pretty,
    Program (..),
    Record (..),
    Scope (..),
    Tag (..),
    TermF (..),
    Var,
    extractNames,
    freshVar,
    mkVar,
    insertNames,
    runFreshVar,
  )
where

import Control.Lens
import qualified Data.Set as Set
import Data.Text.Prettyprint.Doc
import Data.IORef
import MyPrelude
import Control.Monad.State
import Polysemy

data Program t = Program [Def t] DataDecl
  deriving (Functor, Foldable, Traversable)

data Def t = Def (Set Annot) Var (Scope t)
  deriving (Functor, Foldable, Traversable)

data Annot
  = NoCps
  deriving (Eq, Ord, Show)

data DataDecl = DataDecl Text [Record Text]

data Record t = Record Tag [t]
  deriving (Eq, Ord, Functor, Foldable, Traversable)

data TermF t
  = Var Var
  | Abs (Scope t)
  | App t [t]
  | Let t (Scope t)
  | Case t (Patterns t)
  | Cons (Record t)
  | Error
  deriving (Functor, Foldable, Traversable)

data Pattern v
  = PVar v
  | PWild
  | PCons (Record (Pattern v))
  deriving (Functor, Foldable, Eq, Ord)

newtype Patterns t = Patterns [(Pattern (), Scope t)]
  deriving (Functor, Foldable, Traversable, Eq, Ord)

data Var = SrcVar Text | GenVar Int
  deriving (Eq, Ord)

data Tag = SrcTag Text | GenTag Int
  deriving (Eq, Ord)

data Scope t = Scope [Var] t
  deriving (Eq, Ord, Functor, Foldable, Traversable)

data FreshVar m a where
  FreshVar :: FreshVar m Var

$(makeSem ''FreshVar)

-- Handling of scoping rules
class Bound t where
  freeVars :: t -> Set Var

instance Bound t => Bound (Scope t) where
  freeVars (Scope vs t) = freeVars t Set.\\ Set.fromList vs

instance Bound t => Bound (Record t) where
  freeVars (Record _ ts) = foldMap freeVars ts

instance Bound t => Bound (TermF t) where
  freeVars term = case term of
    Var v -> Set.singleton v
    Abs s -> freeVars s
    App f xs -> foldMap freeVars (f : xs)
    Let t s -> freeVars t `Set.union` freeVars s
    Case t ps -> freeVars t `Set.union` foldMap freeVars ps
    Cons r -> freeVars r
    Error -> Set.empty

extractNames :: Pattern Var -> (Pattern (), [Var])
extractNames p = (p $> (), toList p)

insertNames :: Pattern a -> [Var] -> Pattern Var
insertNames pattern variables = case go pattern variables of
  (p, []) -> p
  _ -> error "Mismatched variable count"
  where
    go (PVar _) (v : vs) = (PVar v, vs)
    go (PVar _) [] = error "Mismatched pattern variables"
    go PWild vs = (PWild, vs)
    go (PCons r) vs = runState (PCons <$> traverse (state . go) r) vs

-- runStxT :: Monad m => StateT Metadata m a -> m a
-- runStxT a = evalStateT a initialMetadata

runFreshVar :: Member (Embed IO) r => Sem (FreshVar ': r) a -> Sem r a
runFreshVar sem = do
  ref <- embed $ newIORef (0 :: Int)
  interpret (\case
    FreshVar -> do
      x <- embed $ readIORef ref
      embed $ writeIORef ref (x+1)
      return (GenVar x)) sem

mkVar :: Text -> Var
mkVar = SrcVar

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

instance Pretty t => Pretty (Def t) where
  pretty (Def _ann v (Scope vs t)) =
    parens $
      "def" <+> pretty v <+> parens (hsep . map pretty $ vs)
        <> nest 2 (line <> pretty t)

instance Pretty t => Pretty (Program t) where
  pretty (Program defs _) =
    encloseSep mempty mempty line (pretty <$> defs)

instance Pretty t => Pretty (Patterns t) where
  pretty (Patterns ps) = vsep . map pat $ ps
    where
      pat (p, Scope vs t) = parens (pretty (insertNames p vs) <+> pretty t)

instance Pretty t => Pretty (Record t) where
  pretty (Record c ts) = encloseSep lbrace rbrace space (pretty c:map pretty ts)

instance Pretty Tag where
  pretty (SrcTag c) = pretty c
  pretty (GenTag lbl) = "gen-" <> pretty lbl

prettyTerm :: Pretty t => TermF t -> Doc ann
prettyTerm term = case term of
  Var v -> pretty v
  Abs (Scope vs t) ->
    parens ("fun" <+> parens (hsep . map pretty $ vs) <+> block (pretty t))
  App f ts -> parens (pretty f <+> block (vsep . map pretty $ ts))
  Let t (Scope [v] b) ->
    parens ("let" <+> pretty v <+> nest 2 (sep [pretty t, pretty b]))
  Cons r -> pretty r
  Case t ps ->
    parens ("case" <+> pretty t <> nest 2 (line <> pretty ps))
  Error -> "error"
  _ -> error "Unexpected syntax"

block :: Doc ann -> Doc ann
block x = group $ flatAlt (nest 2 $ hardline <> x) x
