module Syntax
  ( Annot (..),
    Bound (..),
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
    Patterns (..),
    PrimOp (..),
    Program (..),
    RefersTo (..),
    Scope (..),
    StructField (..),
    TermF (..),
    Term (..),
    Tp (..),
    ValueF (..),
    Var (..),
    extractNames,
    freshLabel,
    freshTag,
    freshVar,
    mkVar,
    insertNames,
    primOpNames,
    primOps,
    patternVars,
    patternVarsSet,
    programScopes,
    programTerms,
    runFreshLabel,
    runFreshVar,
    scope,
    scopeBody,
    scopeVars,
    term,
    tpToVar,
    varToTp,
  )
where

import Control.Monad.State.Strict
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Util.Pretty

data Program t
  = Program
      { programDefinitions :: [DefFun t],
        programDatatypes :: [DefData],
        programStructs :: [DefStruct],
        programMain :: DefFun t
      }
  deriving (Functor, Foldable, Traversable)

data DefFun t
  = DefFun
      { funName :: Var,
        funAnnot :: FunAnnot,
        funScope :: Scope t
      }
  deriving (Functor, Foldable, Traversable)

data DefData = DefData {dataName :: Tp, dataTypes :: [Either Tp DefStruct]}

data DefStruct = DefStruct {structName :: Tp, structFields :: [StructField]}

data StructField = FieldName Var | FieldType Tp | FieldBoth Tp Var

data Annot
  = Atomic
  deriving (Eq, Ord)

data FunAnnot
  = FunAnnot
      { funAtomic :: Bool
      }

data LetAnnot
  = LetAnnot
      { letGenerated :: Bool
      }

data PrimOp = Add | Sub | Mul | Div | Neg | And | Or | Not | Eq
  deriving (Eq, Ord)

newtype Label = Label {unLabel :: Int} deriving (Eq, Ord, Pretty)

type Fvs = Map Var RefersTo

data RefersTo = RefGlobal | RefPrimOp | RefLocal deriving (Eq, Ord)

data TermF t
  = Var Var
  | Abs FunAnnot (Scope t)
  | App t [t]
  | Let LetAnnot (Pattern Var) t t
  | Case t (Patterns t)
  | Cons (ValueF t)
  | Panic
  deriving (Functor, Foldable, Traversable)

data Term = Term {termTerm :: TermF Term, termLabel :: Label}

data Pattern v
  = PVar v
  | PType Tp v
  | PWild
  | PCons (ValueF (Pattern v))
  deriving (Functor, Foldable, Eq, Ord)

newtype Patterns t = Patterns [(Pattern (), Scope t)]
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
  freeVars :: t -> Set Var

  allVars :: t -> Set Var

  rename :: Map Var Var -> t -> t

instance Bound t => Bound (Scope t) where
  freeVars (Scope vs t) = freeVars t Set.\\ (Set.fromList . fmap fst $ vs)

  allVars (Scope vs t) = allVars t <> (Set.fromList . fmap fst $ vs)

  rename vars (Scope vs t) =
    let bound = Set.fromList (fmap fst vs)
        vars' = Map.withoutKeys vars bound
     in if Set.fromList (toList vars') `Set.disjoint` bound
          then Scope vs (rename vars' t)
          else error "Free variable would become bound by renaming"

instance Bound t => Bound (TermF t) where
  freeVars term = case term of
    Var v -> Set.singleton v
    Abs _ s -> freeVars s
    Let _ x t s -> freeVars t <> (freeVars s Set.\\ patternVarsSet x)
    Case t ps -> freeVars t `Set.union` freeVars ps
    _ -> foldMap freeVars term

  allVars term = case term of
    Var v -> Set.singleton v
    Abs _ s -> allVars s
    Let _ x t s -> patternVarsSet x <> allVars t <> allVars s
    Case t ps -> allVars t <> allVars ps
    _ -> foldMap allVars term

  rename vars term = case term of
    Var v -> Var $ Map.findWithDefault v v vars
    Abs a s -> Abs a $ rename vars s
    Let a x t s ->
      Let a x (rename vars t) (rename (Map.withoutKeys vars (patternVarsSet x)) s)
    Case t ps -> Case (rename vars t) (rename vars ps)
    _ -> fmap (rename vars) term

instance Bound v => Bound (ValueF v) where
  freeVars = foldMap freeVars

  allVars = foldMap allVars

  rename vars = fmap (rename vars)

instance Bound t => Bound (Patterns t) where
  freeVars (Patterns ps) = foldMap (freeVars . snd) ps

  allVars (Patterns ps) = foldMap (allVars . snd) ps

  rename vars (Patterns ps) = Patterns (fmap (fmap (rename vars)) ps)

instance Bound Term where
  freeVars Term {..} = freeVars termTerm

  allVars Term {..} = allVars termTerm

  rename vars Term {..} = Term {termTerm = rename vars termTerm, ..}

-- Pattern helpers

patternVarsSet :: Pattern Var -> Set Var
patternVarsSet p = foldMap Set.singleton p

patternVars :: Pattern Var -> [Var]
patternVars = toList

extractNames :: Pattern Var -> (Pattern (), [Var])
extractNames p = (p $> (), toList p)

insertNames :: Pattern a -> [Var] -> Pattern Var
insertNames pattern vars = case go pattern vars of
  (p, []) -> p
  _ -> error "Too many variables to insert into pattern"
  where
    go (PVar _) (v : vs) = (PVar v, vs)
    go (PType t _) (v : vs) = (PType t v, vs)
    go PWild vs = (PWild, vs)
    go (PCons r) vs = runState (PCons <$> traverse (state . go) r) vs
    go _ _ = error "Not enough variables to insert into pattern"

mkVar :: Text -> Var
mkVar = MkVar

-- Other helpers
scope :: [Var] -> t -> Scope t
scope vs = Scope (fmap (,Nothing) vs)

scopeVars :: Scope e -> [Var]
scopeVars (Scope xs _) = fmap fst xs

scopeBody :: Scope e -> e
scopeBody (Scope _ e) = e

varToTp :: Var -> Tp
varToTp (MkVar v) = MkTp (Text.toTitle v)

tpToVar :: Tp -> Var
tpToVar (MkTp t) = MkVar (Text.toLower t)

programScopes :: Program t -> Map Var (Scope t)
programScopes Program {..} =
  Map.fromList $ programDefinitions <&> \DefFun {..} -> (funName, funScope)

programTerms :: Program Term -> Map Label Term
programTerms = foldMap aux
  where
    aux term@Term {..} = Map.insert termLabel term $ foldMap aux termTerm

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

-- Pretty printing

-- instance Pretty Term where
--   pretty Term {..} = {- pretty termLabel <> "#" <> -} pretty termTerm

-- instance Pretty (Pattern Var) where
--   pretty (PVar v) = pretty v
--   pretty (PType tp v) = brackets $ pretty tp <+> pretty v
--   pretty PWild = "_"
--   pretty (PCons r) = pretty r

-- instance Pretty t => Pretty (TermF t) where
--   pretty = prettyTerm

-- instance Pretty Annot where
--   pretty Atomic = "#:atomic"

-- instance Pretty t => Pretty (Program t) where
--   pretty Program {..} =
--     types <> hardline <> main <> hardline <> defs <> hardline <> structs
--     where
--       defs = rows $ fmap (\d -> pretty d <> hardline) programDefinitions
--       types = rows $ fmap (\t -> pretty t <> hardline) programDatatypes
--       main = pretty programMain
--       structs = rows $ programStructs <&> \s -> parens ("def-struct" <+> pretty s)

-- instance Pretty t => Pretty (DefFun t) where
--   pretty DefFun {..} =
--     parens $ "def" <+> pretty funName <> prettyBody (variables vs) (pretty body)
--     where
--       Scope vs body = funScope

-- instance Pretty DefData where
--   pretty DefData {..} =
--     parens $ "def-data" <+> pretty dataName <> nested 2 types
--     where
--       types = rows $ fmap (either pretty pretty) dataTypes

-- instance Pretty v => Pretty (ValueF v) where
--   pretty (Number n) = pretty n
--   pretty (String s) = escape s
--   pretty (Record c ts) = braces (pretty c <> (nested' 2 ts))
--   pretty (Boolean True) = "#t"
--   pretty (Boolean False) = "#f"

-- instance Pretty t => Pretty (Patterns t) where
--   pretty (Patterns ps) = case ps of
--     [] -> mempty
--     [p] -> pat p
--     _ -> rows . fmap pat $ ps
--     where
--       pat (p, Scope vs t) =
--         parens (pretty (insertNames p . fmap fst $ vs) <> nested 1 (pretty t))

-- instance Pretty t => Pretty (Scope t) where
--   pretty (Scope xs t) = variables xs <+> pretty t

-- instance Pretty StructField where
--   pretty (FieldName v) = pretty v
--   pretty (FieldType t) = pretty t
--   pretty (FieldBoth t v) = brackets $ pretty t <+> pretty v

-- instance Pretty DefStruct where
--   pretty DefStruct {..} =
--     braces $ pretty structName <+> aligned structFields

instance Pretty PrimOp where
  pretty op =
    let ops = Map.fromList . fmap swap . Map.toList $ primOps
     in pretty $ ops Map.! op

-- prettyTerm :: Pretty t => TermF t -> Doc ann
-- prettyTerm term = case term of
--   Var v -> pretty v
--   Cons v -> pretty v
--   Abs _ (Scope vs t) ->
--     parens ("fun" <> prettyBody (variables vs) (pretty t))
--   App f ts ->
--     parens (pretty f <> nested' 2 ts)
--   Let _ x t b ->
--     parens ("let" <> prettyBody (parens (brackets (aligned' [pretty x, pretty t]))) (pretty b))
--   Case t ps ->
--     parens ("match" <> prettyBody (pretty t) (pretty ps))
--   Panic -> parens $ "error" <+> pretty (String @() "panic")

-- variable :: (Var, Maybe Tp) -> Doc ann
-- variable (v, Nothing) = pretty v
-- variable (v, Just tp) = brackets . aligned' $ [pretty tp, pretty v]

-- variables :: [(Var, Maybe Tp)] -> Doc ann
-- variables = parens . aligned' . fmap variable

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

term :: Member FreshLabel r => TermF Term -> Sem r Term
term termTerm = do
  termLabel <- freshLabel
  pure Term {..}
