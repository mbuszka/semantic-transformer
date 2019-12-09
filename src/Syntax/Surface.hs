{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Syntax.Surface
  ( Expr(..)
  , pprintExpr
  , pprintPgm
  )
where

import           Control.Monad.State
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
import           Syntax.Scope
import           Syntax.Base


data Expr a
  = Var Label a
  | Const Label Const
  | Err Label String
  | Lambda Label (Scope Expr a)
  | App Label (Expr a) (NonEmpty (Expr a))
  | CApp Label Cons (NonEmpty (Expr a))
  | Case Label (Expr a) [Pattern Expr a]
  | Let Label (Expr a) (Scope Expr a)
  deriving (Functor, Foldable, Traversable)

-- Exported Functions --

pprintExpr :: Pretty a => Expr a -> String
pprintExpr e = renderString . layoutSmart defaultLayoutOptions $ prettyExpr
  NoParens
  (Global <$> e)

pprintPgm :: Program Expr -> String
pprintPgm p = renderString . layoutSmart defaultLayoutOptions $ doc
 where
  doc = vsep . fmap (\d -> prettyDef d <> line) . definitions $ p


-- Pretty Printing Helpers --
data PVar a = Local Int Int | Global a

toPVar :: Var (PVar a) -> PVar a
toPVar (F (Global a )) = Global a
toPVar (F (Local i r)) = Local (i + 1) r
toPVar (B r          ) = Local 0 r

type Env a b = a -> PVar b

extend :: Env (PVar a) b -> Env (Var (PVar a)) b
extend e = e . toPVar

instance Pretty a => Pretty (PVar a) where
  pretty (Local idx b) = pretty idx <> pretty "#" <> pretty b
  pretty (Global a   ) = pretty a

prettyPat :: Pretty a => Pattern Expr (PVar a) -> Doc ann
prettyPat (PatConst c e) =
  pretty "|" <+> pretty c <+> pretty "->" <+> prettyExpr NoParens e
prettyPat (PatCons t s) =
  pretty "|" <+> pretty t <+> prettyScope (prettyExpr NoParens . fmap toPVar) s
prettyPat (PatWild e) =
  pretty "|" <+> pretty "_" <+> pretty "->" <+> prettyExpr NoParens e

prettyScope :: Pretty a => (f (Var a) -> Doc ann) -> Scope f a -> Doc ann
prettyScope f (Scope l cnt e) = pretty l <> pretty "@" <> pretty cnt <+> pretty "->" <+> f e

prettyExpr :: Pretty a => Parenthesise -> Expr (PVar a) -> Doc ann
prettyExpr _ (Var   _ v) = pretty v
prettyExpr _ (Const _ c) = pretty c
prettyExpr p (Err   _ e) = parenApp p $ pretty "err" <+> pretty (String e)
prettyExpr p (Lambda _ s) =
  parenLam p
    $   pretty "fun"
    <+> prettyScope (block . prettyExpr NoParens . fmap toPVar) s
prettyExpr p (App  l f xs) = pretty l <> pretty "@" <> (parens $ prettyExpr ParenAll f <+> prettyApp xs)
prettyExpr p (CApp _ c xs) = parenApp p $ pretty c <+> prettyApp xs
prettyExpr p (Let _ e (Scope _ _ b)) =
  parenLam p
    .   group
    $   pretty "let"
    <+> prettyExpr NoParens e
    <+> pretty "in"
    <>  line
    <>  prettyExpr NoParens (toPVar <$> b)
prettyExpr p (Case _ e ps) =
  parenLam p $ pretty "match" <+> prettyExpr NoParens e <> hardline <> vsep ps'
  where ps' = fmap prettyPat ps

prettyApp (x      :| []) = prettyExpr ParenApp x
prettyApp (x :| y :  ys) = prettyExpr ParenAll x <+> prettyApp (y :| ys)

data Parenthesise = NoParens | ParenApp | ParenAll

parenApp NoParens = id
parenApp _        = parens

parenLam ParenAll = parens
parenLam _        = id

prettyDef :: Def Expr String -> Doc ann
prettyDef (Def name scope) = pretty "def" <+> pretty name <+> prettyScope
  (prettyExpr NoParens . fmap toPVar)
  (Global <$> scope)

block :: Doc ann -> Doc ann
block x = flatAlt x (nest 2 $ hardline <> x)
