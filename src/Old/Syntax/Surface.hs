{-# LANGUAGE NoOverloadedStrings #-}
  
module Old.Syntax.Surface
  ( Expr (..),
    pprintExpr,
    pprintPgm,
  )
where

import Control.Lens
import Control.Monad.State
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
  ( renderString,
  )
import Old.Syntax.Base

data Expr
  = Var ELabel Var
  | Constant ELabel Constant
  | Err ELabel String
  | Lambda ELabel (Scope Expr)
  | App ELabel Expr (NonEmpty Expr)
  | CApp ELabel Tag (NonEmpty Expr)
  | Case ELabel Expr [Pattern Expr]
  | Let ELabel Expr (Scope Expr)

-- Exported Functions --

pprintExpr :: Expr -> String
pprintExpr e =
  renderString . layoutSmart defaultLayoutOptions $
    prettyExpr
      NoParens
      e

pprintPgm :: Program Expr -> String
pprintPgm p = renderString . layoutSmart defaultLayoutOptions $ doc
  where
    doc = vsep . fmap (\d -> prettyDef d <> line) $ p ^. definitions

prettyPat :: Pattern Expr -> Doc ann
prettyPat (PatConst c e) =
  pretty "|" <+> pretty c <+> pretty "->" <+> prettyExpr NoParens e
prettyPat (PatConstructor t s) =
  pretty "|" <+> pretty t <+> prettyScope (prettyExpr NoParens) s
prettyPat (PatWild e) =
  pretty "|" <+> pretty "_" <+> pretty "->" <+> prettyExpr NoParens e

prettyScope :: (e -> Doc ann) -> Scope e -> Doc ann
prettyScope f (Scope l cnt e) = pretty l <> pretty "@" <> pretty cnt <+> pretty "->" <+> f e

prettyExpr :: Parenthesise -> Expr -> Doc ann
prettyExpr _ (Var _ v) = pretty v
prettyExpr _ (Constant _ c) = pretty c
prettyExpr p (Err _ e) = parenApp p $ pretty "err" <+> pretty (String e)
prettyExpr p (Lambda _ s) =
  parenLam p $
    pretty "fun"
      <+> prettyScope (block . prettyExpr NoParens) s
prettyExpr p (App l f xs) = pretty l <> pretty "@" <> (parens $ prettyExpr ParenAll f <+> prettyApp xs)
prettyExpr p (CApp _ c xs) = parenApp p $ pretty c <+> prettyApp xs
prettyExpr p (Let _ e (Scope _ _ b)) =
  parenLam p
    . group
    $ pretty "let"
      <+> prettyExpr NoParens e
      <+> pretty "in"
      <> line
      <> prettyExpr NoParens b
prettyExpr p (Case _ e ps) =
  parenLam p $ pretty "match" <+> prettyExpr NoParens e <> hardline <> vsep ps'
  where
    ps' = fmap prettyPat ps

prettyApp (x :| []) = prettyExpr ParenApp x
prettyApp (x :| y : ys) = prettyExpr ParenAll x <+> prettyApp (y :| ys)

data Parenthesise = NoParens | ParenApp | ParenAll

parenApp NoParens = id
parenApp _ = parens

parenLam ParenAll = parens
parenLam _ = id

prettyDef :: Def Expr -> Doc ann
prettyDef (Def name scope) =
  pretty "def" <+> pretty name
    <+> prettyScope
      (prettyExpr NoParens)
      scope

block :: Doc ann -> Doc ann
block x = flatAlt x (nest 2 $ hardline <> x)
