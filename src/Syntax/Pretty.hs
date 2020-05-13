module Syntax.Pretty
  ( prettyProgram,
    debugProgram,
  )
where

import Syntax
import Util.Pretty

data Options = Options {addLabels :: Bool}

prettyProgram :: Program Term -> Doc ann
prettyProgram = prettyProgram' Options {addLabels = False}

debugProgram :: Program Term -> Doc ann
debugProgram = prettyProgram' Options {addLabels = True}

prettyProgram' :: Options -> Program Term -> Doc ann
prettyProgram' opts Program {..} =
  let defs = rows $ fmap (\d -> prettyDefFun opts d <> hardline) programDefinitions
      types = rows $ fmap (\t -> prettyDefData opts t <> hardline) programDatatypes
      main = prettyDefFun opts programMain
      structs = rows $ programStructs <&> prettyStructDef opts
   in types
        <> hardline
        <> structs
        <> hardline
        <> defs
        <> hardline
        <> main
        <> hardline

prettyStruct :: Options -> DefStruct -> Doc ann
prettyStruct opts DefStruct {..} =
  braces $ pretty structName <+> aligned' (structFields <&> prettyStructField opts)

prettyStructDef :: Options -> DefStruct -> Doc ann
prettyStructDef opts d = parens $ "def-struct" <+> prettyStruct opts d

prettyStructField :: Options -> StructField -> Doc ann
prettyStructField _opts field = case field of
  FieldName n -> pretty n
  FieldType t -> pretty t
  FieldBoth tp n -> brackets $ pretty tp <+> pretty n

prettyDefData :: Options -> DefData -> Doc ann
prettyDefData opts DefData {..} =
  let types = rows $ fmap (either pretty (prettyStruct opts)) dataTypes
   in parens $ "def-data" <+> pretty dataName <> nested 2 types

prettyDefFun :: Options -> DefFun Term -> Doc ann
prettyDefFun opts DefFun {..} =
  parens $ "def" <+> pretty funName <> prettyBody (variables funVars) (prettyBlock opts funBody)

prettyBlock :: Options -> Term -> Doc ann
prettyBlock opts@Options {..} tm@Term {..} = case termF of
  Let _ x t b ->
    let s = parens ("let" <+> prettyPattern opts x <> nested 2 (prettyTerm opts t))
     in (if addLabels then pretty termLabel <> "#" else mempty) <> s <> hardline <> prettyBlock opts b
  _ -> prettyTerm opts tm

prettyTerm :: Options -> Term -> Doc ann
prettyTerm opts@Options {..} Term {..} = prefix <> rest
  where
    prefix = if addLabels then pretty termLabel <> "#" else mempty
    rest = case termF of
      Var v -> pretty v
      Cons v -> prettyValue opts (prettyTerm opts) v
      Abs FunAnnot {..} vs t ->
        parens ("fun" <> prettyBody (parens $ aligned vs) (prettyBlock opts t))
      App t ts ->
        parens (prettyTerm opts t <> nested 2 (aligned' $ fmap (prettyTerm opts) ts))
      Case t ps ->
        parens ("match" <> prettyBody (prettyTerm opts t) (prettyCases opts ps))
      Error err -> parens ("error" <+> escape err)
      Let{} -> "ERROR"

prettyCases :: Options -> Branches Term -> Doc ann
prettyCases opts bs = rows $ aux bs
  where 
    aux BNil = []
    aux (Branch p t bs') =
      parens (prettyPattern opts p <> nested 2 (prettyBlock opts t)) : aux bs'

prettyValue :: Options -> (a -> Doc ann) -> ValueF a -> Doc ann
prettyValue _opts prettyInner v = case v of
  Number n -> pretty n
  String s -> escape s
  Record t ts -> braces (pretty t <> nested 2 (aligned' $ fmap prettyInner ts))
  Boolean True -> "#t"
  Boolean False -> "#f"

prettyPattern :: Options -> Pattern -> Doc ann
prettyPattern opts pat = case pat of
  PVar v -> pretty v
  PType tp v -> brackets $ pretty tp <+> pretty v
  PWild -> "_"
  PCons v -> prettyValue opts (prettyPattern opts) v

variable :: (Maybe Tp, Var) -> Doc ann
variable (Nothing, v) = pretty v
variable (Just tp, v) = brackets . aligned' $ [pretty tp, pretty v]

variables :: [(Maybe Tp, Var)] -> Doc ann
variables = parens . aligned' . fmap variable