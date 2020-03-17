module Syntax.Term where

import Syntax
import Data.Text.Prettyprint.Doc (Pretty)

newtype Term = Term {unTerm :: TermF Term}
  deriving (Bound, Pretty)

