{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Syntax
  ( Tag(..)
  , Const(..)
  , Expr(..)
  , Pattern(..)
  , TopLevel(..)
  , bind
  , mapPattern
  , unbind
  , pprint
  , pprintLn
  )
where

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
import           Data.Text.Prettyprint.Doc.Render.String
                                                ( renderString )
import           Debug.Trace

newtype Tag = MkTag String
  deriving (Eq, Ord, Show)

instance Pretty Tag where
  pretty (MkTag s) = pretty s


data Const = Int Int | String String | Tag Tag
  deriving (Eq, Ord)

instance Show Const where
  show (Int    i         ) = show i
  show (String s         ) = show s
  show (Tag   (MkTag c)) = c

instance Pretty Const where
  pretty (Int    x) = pretty x
  pretty (String x) = pretty (show x)
  pretty (Tag   c) = pretty c


data Pattern f a
  = PatConst Const (f a)
  | PatConstructor Tag (NonEmpty String) (Scope Int f a)
  | PatWild (f a)
  deriving (Functor, Foldable, Traversable)

deriving instance (Show (f a), Show (f (Var Int a))) => Show (Pattern f a)

instance Subst Pattern where
  subst k (PatConst c e  ) = PatConst c (e >>= k)
  subst k (PatConstructor t ns s) = PatConstructor t ns (subst k s)
  subst k (PatWild e     ) = PatWild (e >>= k)


type Bind a = Scope Int Expr a

data Expr a
  = Var a
  | Const Const
  | Err String
  | Lambda (NonEmpty String) (Bind a)
  | App (Expr a) (NonEmpty (Expr a))
  | CApp Tag (NonEmpty (Expr a))
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


data TopLevel a = DefFun a (NonEmpty String) (Bind a)
  deriving (Functor)

instance Pretty a => Pretty (TopLevel a) where
  pretty t = prettyTopLevel $ fmap pretty t

-- Exported Functions --

bind :: Eq a => NonEmpty a -> Expr a -> Bind a
bind args = abstract (elemIndex args)

unbind :: NonEmpty a -> Bind a -> Expr a
unbind xs = instantiate (Var . (xs NE.!!))

mapPattern :: (forall a . f a -> f a) -> Pattern f a -> Pattern f a
mapPattern f (PatWild e             ) = PatWild (f e)
mapPattern f (PatConst c e          ) = PatConst c (f e)
mapPattern f (PatConstructor t ns (Scope e)) = PatConstructor t ns (Scope (f e))


-- Pretty Printing Helpers --

prettyPat :: Pattern Expr (Doc ann) -> Doc ann
prettyPat (PatConst c e) =
  pretty "|" <+> pretty c <+> pretty "->" <+> prettyExpr NoParens e
prettyPat (PatConstructor t ns b) =
  let ns' = fmap pretty ns
  in  pretty "|"
        <+> pretty t
        <+> hsep (NE.toList ns')
        <+> pretty "->"
        <+> prettyExpr NoParens (unbind ns' b)
prettyPat (PatWild e) =
  pretty "|" <+> pretty "_" <+> pretty "->" <+> prettyExpr NoParens e

prettyExpr :: Parenthesise -> Expr (Doc ann) -> Doc ann
prettyExpr _ (Var   v) = v
prettyExpr _ (Const c) = pretty c
prettyExpr p (Err   e) = parenApp p $ pretty "err" <+> pretty (String e)
prettyExpr p (Lambda names bind) =
  parenLam p
    $   pretty "fun"
    <+> (hsep . fmap pretty . NE.toList) names
    <+> pretty "->"
    <+> block (prettyExpr NoParens (putNames names bind))
prettyExpr p (App  f xs) = parenApp p $ prettyExpr ParenAll f <+> prettyApp xs
prettyExpr p (CApp c xs) = parenApp p $ pretty c <+> prettyApp xs
prettyExpr p (Case e ps) =
  parenLam p $ pretty "match" <+> prettyExpr NoParens e <> hardline <> vsep ps'
  where ps' = fmap prettyPat ps

prettyApp (x      :| []) = prettyExpr ParenApp x
prettyApp (x :| y :  ys) = prettyExpr ParenAll x <+> prettyApp (y :| ys)

data Parenthesise = NoParens | ParenApp | ParenAll

parenApp NoParens = id
parenApp _        = parens

parenLam ParenAll = parens
parenLam _        = id

putNames :: NonEmpty String -> Bind (Doc ann) -> Expr (Doc ann)
putNames names = instantiate (fmap (Var . pretty) names NE.!!)

prettyTopLevel :: TopLevel (Doc a) -> Doc a
prettyTopLevel (DefFun name args bind) =
  pretty "def"
    <+> name
    <+> (hsep . fmap pretty . NE.toList) args
    <+> pretty "->"
    <+> block (prettyExpr NoParens (putNames args bind))

elemIndex :: Eq a => NonEmpty a -> a -> Maybe Int
elemIndex (x :| xs) y | x == y    = Just 0
                      | otherwise = (+ 1) <$> List.elemIndex x xs

block :: Doc ann -> Doc ann
block x = flatAlt x (nest 2 $ hardline <> x)

pprint :: Pretty a => a -> String
pprint = renderString . layoutSmart defaultLayoutOptions . pretty

pprintLn :: Pretty a => a -> IO ()
pprintLn = putStrLn . pprint
