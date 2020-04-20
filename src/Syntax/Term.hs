module Syntax.Term where

import Syntax

newtype Term = Term {unTerm :: TermF Term} deriving (Pretty, Bound)