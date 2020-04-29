module Syntax.Scoped where

import Syntax hiding (Term (..))
import Syntax.Source (SrcTerm (..))
import Util

data Term
  = Term
      { term :: TermF Term,
        loc :: Maybe Loc,
        fvs :: Fvs
      }

fromSource :: SrcTerm -> Fvs -> TermF Term -> Sem r Term
fromSource SrcTerm {..} fvs new =
  pure $
    Term {term = new, loc = Just srcLoc, fvs}
