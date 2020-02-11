{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Syntax.Base where

import           Control.Lens
import           Data.Data
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text.Prettyprint.Doc

newtype Tag = MkTag String
  deriving (Eq, Ord, Show, Data, Typeable)

newtype SLabel = SLabel Int
  deriving (Eq, Ord, Show, Data, Typeable)

newtype ELabel = ELabel Int
  deriving (Eq, Ord, Show, Data, Typeable)

data Var = Local SLabel Int | Global String
  deriving (Eq, Ord, Show, Data, Typeable)

data Constant = Int Int | String String | Tag Tag
  deriving (Eq, Ord, Show, Data, Typeable)

data Scope e = Scope
  { _sLabel  :: SLabel
  , _sArgCnt :: Int
  , _sBody   :: e
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Data, Typeable)

data Pattern e
  = PatConst Constant e
  | PatConstructor Tag (Scope e)
  | PatWild e
  deriving (Functor, Foldable, Traversable, Data, Typeable)

data Def e = Def String (Scope e)
  deriving (Functor, Foldable, Traversable, Data, Typeable)

data Metadata = Metadata
  { _mdNextExpression :: ELabel
  , _mdNextScope      :: SLabel
  , _mdScopeBindings  :: Map SLabel (NonEmpty String)
  }
  deriving (Eq, Ord, Show, Data, Typeable)


data Program e = Program
  { _definitions :: [Def e]
  , _metadata    :: Metadata
  }

$(makeLenses ''Scope)
$(makePrisms ''Pattern)
$(makeLenses ''Metadata)
$(makeLenses ''Program)

instance Pretty Tag where
  pretty (MkTag s) = pretty s

instance Pretty SLabel where
  pretty (SLabel s) = pretty s

instance Pretty ELabel where
  pretty (ELabel l) = pretty l

instance Pretty Var where
  pretty (Local idx b) = pretty idx <> pretty "#" <> pretty b
  pretty (Global str ) = pretty str

instance Pretty Constant where
  pretty (Int    x) = pretty x
  pretty (String x) = pretty (show x)
  pretty (Tag    c) = pretty c

initMetadata :: Metadata
initMetadata = Metadata (ELabel 0) (SLabel 0) Map.empty

nextLabel :: Metadata -> (ELabel, Metadata)
nextLabel (Metadata (ELabel n) s m) =
  let l = ELabel (n + 1) in (l, Metadata l s m)

nextScope :: Maybe (NonEmpty String) -> Metadata -> (SLabel, Metadata)
nextScope xs (Metadata l (SLabel n) m) =
  let s = SLabel (n + 1)
  in  case xs of
        Just xs -> (s, Metadata l s (Map.insert s xs m))
        Nothing -> (s, Metadata l s m)

appendFreshVar :: Scope e -> (Scope e, Var)
appendFreshVar (Scope l cnt e) = (Scope l (cnt + 1) e, Local l cnt)

