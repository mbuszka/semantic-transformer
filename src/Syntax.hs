{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Syntax where

import           Bind
import           Control.Monad.State
import qualified Data.List                     as List
import qualified Data.List.NonEmpty            as NE
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Data.Maybe
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Text.Prettyprint.Doc
import           Debug.Trace

newtype Cons = MkCons String deriving (Eq, Show)

instance Pretty Cons where
  pretty (MkCons s) = pretty s

type Bind a = Scope Int Expr a

data Expr a
  = Var a
  | Const Const
  | Err String
  | Lambda (NonEmpty String) (Bind a)
  | App (Expr a) (NonEmpty (Expr a))
  | CApp Cons (NonEmpty (Expr a))
  | Case (Expr a) [Pattern Expr a]
  deriving (Functor, Foldable, Traversable)

deriving instance Show a => Show (Expr a)

instance Applicative Expr where
  pure  = Var
  (<*>) = ap

instance Monad Expr where
  Var   a      >>= k = k a
  Const c      >>= _ = Const c
  Err   s      >>= _ = Err s
  Lambda as b  >>= k = Lambda as (subst k b)
  App    f  xs >>= k = App (f >>= k) (fmap (>>= k) xs)
  CApp   c  xs >>= k = CApp c (fmap (>>= k) xs)
  Case   e  ps >>= k = Case (e >>= k) (fmap (subst k) ps)

instance Pretty a => Pretty (Expr a) where
  pretty = prettyExpr NoParens . fmap pretty

data Pattern f a
  = PatConst Const (f a)
  | PatCons Cons (NonEmpty String) (Scope Int f a)
  | PatWild (f a)
  deriving (Functor, Foldable, Traversable)

deriving instance (Show (f a), Show (f (Var Int a))) => Show (Pattern f a)

instance Subst Pattern where
  subst k (PatConst c e  ) = PatConst c (e >>= k)
  subst k (PatCons t ns s) = PatCons t ns (subst k s)
  subst k (PatWild e     ) = PatWild (e >>= k)

prettyPat :: Pattern Expr (Doc ann) -> Doc ann
prettyPat (PatConst c e) =
  pretty "|" <+> pretty c <+> pretty "->" <+> prettyExpr NoParens e
prettyPat (PatCons t ns b) =
  let ns' = NE.toList (fmap pretty ns)
  in  pretty "|" <+> pretty t <+> hsep ns' <+> pretty "->" <+> prettyExpr
        NoParens
        (instantiate (Var . (ns' !!)) b)
prettyPat (PatWild e) =
  pretty "|" <+> pretty "_" <+> pretty "->" <+> prettyExpr NoParens e

prettyExpr :: Parenthesise -> Expr (Doc ann) -> Doc ann
prettyExpr _ (Var   v) = v
prettyExpr _ (Const c) = pretty c
prettyExpr p (Err   e) = parenApp p $ pretty "err" <+> pretty (String e)
prettyExpr p (Lambda names bind) =
  parenLam p
    $   pretty "fun"
    <+> (sep . fmap pretty . NE.toList) names
    <+> pretty "->"
    <+> nest 2 (prettyExpr NoParens (putNames names bind))
prettyExpr p (App  f xs) = parenApp p $ prettyExpr ParenAll f <+> prettyApp xs
prettyExpr p (CApp c xs) = parenApp p $ pretty c <+> prettyApp xs
prettyExpr p (Case e ps) =
  parenLam p $ pretty "match" <+> prettyExpr NoParens e <+> nest
    0
    (sep . fmap prettyPat $ ps)

prettyApp (x      :| []) = prettyExpr ParenApp x
prettyApp (x :| y :  ys) = prettyExpr ParenAll x <+> prettyApp (y :| ys)

data Parenthesise = NoParens | ParenApp | ParenAll

prettyParens x = pretty "(" <> x <> pretty ")"

parenApp NoParens = id
parenApp _        = prettyParens

parenLam ParenAll = prettyParens
parenLam _        = id

putNames :: NonEmpty String -> Bind (Doc ann) -> Expr (Doc ann)
putNames names = instantiate (fmap (Var . pretty) names NE.!!)

data TopLevel a
  = DefFun a (NonEmpty String) (Bind a)
  deriving (Functor)

prettyTopLevel :: TopLevel (Doc a) -> Doc a
prettyTopLevel (DefFun name args bind) =
  pretty "def"
    <+> name
    <+> (sep . fmap pretty . NE.toList) args
    <+> pretty "->"
    <+> prettyExpr NoParens (putNames args bind)

instance Pretty a => Pretty (TopLevel a) where
  pretty t = prettyTopLevel $ fmap pretty t

-- data DefConstructor = DefConstructor String [String]

data Const = Int Int | String String | Cons Cons
  deriving (Eq)

instance Show Const where
  show (Int    i         ) = show i
  show (String s         ) = show s
  show (Cons   (MkCons c)) = c

instance Pretty Const where
  pretty (Int    x) = pretty x
  pretty (String x) = pretty x
  pretty (Cons   c) = pretty c

elemIndex :: Eq a => NonEmpty a -> a -> Maybe Int
elemIndex (x :| xs) y | x == y    = Just 0
                      | otherwise = (+ 1) <$> List.elemIndex x xs

bind :: NonEmpty String -> Expr String -> Bind String
bind args = abstract (elemIndex args)

unbind :: NonEmpty String -> Bind String -> Expr String
unbind xs = instantiate (Var . (xs NE.!!))
