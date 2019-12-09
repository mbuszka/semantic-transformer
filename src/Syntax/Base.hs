{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}

module Syntax.Base where

import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.Map                       ( Map )
import qualified Data.Map as Map
import           Data.Text.Prettyprint.Doc
import           Syntax.Scope

newtype Cons = MkCons String
  deriving (Eq, Ord, Show)

instance Pretty Cons where
  pretty (MkCons s) = pretty s


data Metadata =
  Metadata Label ScopeLabel (Map ScopeLabel (NonEmpty String))

data Const = Int Int | String String | Cons Cons
  deriving (Eq, Ord)

instance Show Const where
  show (Int    i         ) = show i
  show (String s         ) = show s
  show (Cons   (MkCons c)) = c

instance Pretty Const where
  pretty (Int    x) = pretty x
  pretty (String x) = pretty (show x)
  pretty (Cons   c) = pretty c


data Pattern f a
  = PatConst Const (f a)
  | PatCons Cons (Scope f a)
  | PatWild (f a)
  deriving (Functor, Foldable, Traversable)

data Def f a = Def String (Scope f a)
  deriving (Functor)

newtype Label = Label Int
  deriving (Eq, Ord, Show)

instance Pretty Label where
  pretty (Label l) = pretty l

instance MFunctor Pattern where
  mmap f (PatWild e   ) = PatWild (f e)
  mmap f (PatConst c e) = PatConst c (f e)
  mmap f (PatCons  t e) = PatCons t (mmap f e)

  mtraverse f (PatWild e   ) = PatWild <$> f e
  mtraverse f (PatConst c e) = PatConst c <$> f e
  mtraverse f (PatCons  t e) = PatCons t <$> mtraverse f e


instance MFunctor Def where
  mmap f (Def a s) = Def a $ mmap f s

  mtraverse f (Def a s) = Def a <$> mtraverse f s


data Program f = Program
  { definitions :: [Def f String]
  , metadata ::  Metadata
  }


initMetadata :: Metadata
initMetadata = Metadata (Label 0) (ScopeLabel 0) Map.empty

nextLabel :: Metadata -> (Label, Metadata)
nextLabel (Metadata (Label n) s m) =
  let l = Label (n+1) in (l, Metadata l s m)

nextScope :: Maybe (NonEmpty String) -> Metadata -> (ScopeLabel, Metadata)
nextScope xs (Metadata l (ScopeLabel n) m) =
  let s = ScopeLabel (n+1) in case xs of
    Just xs -> (s, Metadata l s (Map.insert s xs m))
    Nothing -> (s, Metadata l s m)
