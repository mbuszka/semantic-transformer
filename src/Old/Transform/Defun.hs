{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Old.Transform.Defun where

import qualified Old.Analysis.ControlFlow as Cfa
import Control.Lens
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Old.Syntax.Anf
import Old.Syntax.Base

data DefunState
  = DefunState
      { _lambdas :: Map SLabel (Tag, [Var], Scope Anf),
        _tagCounter :: Int
      }
  deriving (Show)

$(makeLenses ''DefunState)

nextTag :: MonadState DefunState m => m Tag
nextTag = do
  x <- use tagCounter
  tagCounter %= (+ 1)
  return . MkTag $ "Clo" ++ show x

go :: MonadState DefunState m => Atom Anf -> m (Atom Anf)
go l@(Lambda scope) = do
  let fv = Set.toList $ fvsA l
  t <- nextTag
  lambdas %= Map.insert (scope ^. sLabel) (t, fv, scope)
  return $ case fv of
    [] -> Constant (Tag t)
    v : vs -> CApp t (Var v :| fmap Var vs)
go a = pure a

defun :: MonadState DefunState m => Anf -> m Anf
defun = transformAnf (mapMOf (aExpr . eAtoms) go)

initState :: DefunState
initState = DefunState Map.empty 0

transform :: Program Anf -> (Program Anf, DefunState)
transform p =
  runState (mapMOf (definitions . traversed . traversed) defun p) initState

-- createApplications :: 