module Syntax.Term where

import Syntax hiding (Term)

newtype Term = Term {unTerm :: TermF Term} deriving (Pretty, Bound)