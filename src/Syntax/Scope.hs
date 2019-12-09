{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

module Syntax.Scope where

import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List                     as List
import Data.Text.Prettyprint.Doc

data Var a = B Int | F a
  deriving (Eq, Ord, Functor, Foldable, Traversable)

newtype ScopeLabel = ScopeLabel Int
  deriving (Eq, Ord, Show)

instance Pretty ScopeLabel where
  pretty (ScopeLabel s) = pretty s

data Scope f a = Scope ScopeLabel Int (f (Var a))
  deriving (Functor, Foldable, Traversable)

instance MFunctor Scope where
  mmap f (Scope l cnt e) = Scope l cnt (f e)

  mtraverse f (Scope l cnt e) = Scope l cnt <$> f e

class MFunctor t where
  mmap :: (forall a. f a -> g a) -> t f a -> t g a
  mtraverse :: Monad m => (forall a. f a -> m (g a)) -> t f a -> m (t g a)

unscope :: Functor f => Scope f a -> (Int -> b) -> (a -> b) -> f b
unscope (Scope _ cnt e) b f = fmap k e
 where
  k (B x) = b x
  k (F x) = f x

bind :: (Eq a, Functor f) => ScopeLabel -> NonEmpty a -> f a -> Scope f a
bind l args = Scope l (length args) . fmap f
 where
  f x = case elemIndex args x of
    Just k  -> B k
    Nothing -> F x

weaken :: Functor f => f a -> f (Var a)
weaken = fmap F

elemIndex :: Eq a => NonEmpty a -> a -> Maybe Int
elemIndex (x :| xs) y | x == y    = Just 0
                      | otherwise = (+ 1) <$> List.elemIndex y xs

