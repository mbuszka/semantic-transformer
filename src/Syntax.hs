{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Syntax
  ( Annot (..),
    Bound (..),
    DataDecl (..),
    Def (..),
    Label,
    MonadStx,
    Pattern (..),
    Patterns (..),
    Program (..),
    Record (..),
    Scope (..),
    Tag (..),
    Term,
    TermF (..),
    Var,
    extractNames,
    freshVar,
    freshLabel,
    label,
    mkTerm,
    mkVar,
    runStx,
    runStxT,
    unTerm,
    insertNames,
  )
where

import Control.Lens
import qualified Data.Set as Set
import Data.Text.Prettyprint.Doc
import MyPrelude
import Control.Monad.State

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

data Var = Source Text | Gen Int
  deriving (Eq, Ord)

data Tag = SrcTag Text | GenTag Label
  deriving (Eq, Ord)

data Scope t = Scope [Var] t
  deriving (Eq, Ord, Functor, Foldable, Traversable)

type MonadStx m = MonadState Metadata m

newtype Label = Label {_unLabel :: Int}
  deriving (Eq, Ord)

data Fix f = Fix Label (Set Var) (f (Fix f))

data Metadata
  = Metadata
      { _mdVar :: Int,
        _mdLabel :: Label
      }

type Term = Fix TermF

$(makeLenses ''Metadata)

$(makeLenses ''Label)

-- Handling of scoping rules
class Bound t where
  freeVars :: t -> Set Var

instance Bound (Fix f) where
  freeVars (Fix _ fvs _) = fvs

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

runStxT :: Monad m => StateT Metadata m a -> m a
runStxT a = evalStateT a initialMetadata

runStx :: State Metadata a -> a
runStx a = evalState a initialMetadata

initialMetadata :: Metadata
initialMetadata = Metadata 0 (Label 0)

freshLabel :: MonadStx m => m Label
freshLabel = do
  lbl <- gets (view mdLabel)
  mdLabel . unLabel %= (+ 1)
  return lbl

freshVar :: MonadStx m => m Var
freshVar = do
  x <- gets (view mdVar)
  mdVar %= (+ 1)
  return (Gen x)

mkVar :: Text -> Var
mkVar = Source

mkTerm :: (Bound (f (Fix f)), MonadStx m) => f (Fix f) -> m (Fix f)
mkTerm t = do
  lbl <- freshLabel
  return $ Fix lbl (freeVars t) t

unTerm :: Fix f -> f (Fix f)
unTerm (Fix _ _ t) = t

label :: Fix f -> Label
label (Fix l _ _) = l

-- Pretty printing
instance Pretty Label where
  pretty (Label l) = pretty l

instance Pretty Var where
  pretty (Source v) = pretty v
  pretty (Gen n) = "gen-" <> pretty n

instance Pretty (f (Fix f)) => Pretty (Fix f) where
  pretty (Fix lbl _ t) = pretty lbl <> "@" <> pretty t

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
