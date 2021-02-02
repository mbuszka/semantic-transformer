module Syntax.Source where

import Common
import Syntax
import Import

data SrcTerm = SrcTerm
  { srcTerm :: TermF SrcTerm,
    srcLoc :: Loc
  }

data TopLevel
  = TFun Loc (DefFun SrcTerm)
  | TData Loc DefData
  | TStruct Loc DefStruct

data SrcProgram = SrcProgram
  { srcPrologue :: Text,
    srcProgram :: [TopLevel],
    srcEpilogue :: Text
  }

wrap :: SrcTerm -> Fvs -> TermF SrcTerm -> Sem r SrcTerm
wrap SrcTerm {..} _fvs new = pure SrcTerm {srcTerm = new, ..}

unwrap :: SrcTerm -> Sem r (TermF SrcTerm, Maybe Loc)
unwrap SrcTerm {..} = pure (srcTerm, Just srcLoc)