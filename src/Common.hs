module Common
  ( Err (..),
    Loc (..),
    Label (..),
    throwLabeled,
    throwMsg,
  )
where

import Util.Pretty
import Polysemy.Error

data Loc = Loc !Int !Int

newtype Label = Label {unLabel :: Int} deriving (Eq, Ord, Pretty)

data Err
  = ParseError Text
  | Err (Maybe Loc) (Maybe Label) Text

instance Pretty Loc where
  pretty (Loc row col) = "line:" <+> pretty row <+> "col:" <+> pretty col

instance Pretty Err where
  pretty (ParseError err) = pretty err
  pretty (Err loc lbl err) =
    "Error:" <+> pretty err
      <+> "at"
      <+> pretty loc
      <+> maybe mempty (\l -> "label: " <> pretty l) lbl

throwLabeled :: Member (Error Err) r => Label -> Text -> Sem r a
throwLabeled lbl err = throw $ Err Nothing (Just lbl) err

throwMsg :: Member (Error Err) r => Text -> Sem r a
throwMsg err = throw $ Err Nothing Nothing err