module Util
  ( Err (..),
    Loc (..),
  )
where

import Util.Pretty
import Data.Text.Prettyprint.Doc

data Loc = Loc !Int !Int

data Err
  = ParseError Text
  | ModuleError Text
  | ScopeError (Maybe Loc) Text
  | EvalError Text
  | InternalError Text

instance Pretty Loc where
  pretty (Loc row col) = "line:" <+> pretty row <+> "col:" <+> pretty col

instance Pretty Err where
  pretty (ParseError err) = "Error:" <+> pretty err
  pretty (ModuleError err) = "Error:" <+> pretty err
  pretty (ScopeError l err) =
    "Error:" <+> pretty err <> softline <> "at" <+> pretty l
  pretty (EvalError err) = "Error:" <+> pretty err
  pretty (InternalError err) = "Internal error:" <+> pretty err