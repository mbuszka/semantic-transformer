{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}

module Bind where

import Data.Bifunctor

data Var b a = B b | F a
  deriving (Eq, Show, Ord, Functor, Traversable, Foldable)

instance Bifunctor Var where
  bimap f _ (B b) = B (f b)
  bimap _ g (F a) = F (g a)

newtype Scope b f a = Scope { unscope :: f (Var b a) }

class Subst t where
  subst :: Monad f => (a -> f b) -> t f a -> t f b

instance (Functor f) => Functor (Scope b f) where
  fmap f (Scope t) = Scope $ fmap (fmap f) t

instance Foldable f => Foldable (Scope b f) where
  foldMap f (Scope t) = foldMap (foldMap f) t

instance Traversable f => Traversable (Scope b f) where
  traverse f (Scope t) = Scope <$> traverse (traverse f) t

instance Subst (Scope b) where
  subst k (Scope t) = Scope $ t >>= k'
   where
    k' (F a) = F <$> k a
    k' (B b) = pure $ B b

instantiate :: Monad f => (b -> f a) -> Scope b f a -> f a
instantiate k (Scope t) = t >>= k'
 where
  k' (B b) = k b
  k' (F x) = pure x

abstract :: Monad f => (a -> Maybe b) -> f a -> Scope b f a
abstract k t = Scope $ fmap k' t
 where
  k' x = case k x of
    Just b -> B b
    Nothing -> F x

weaken :: Functor f => f a -> f (Var b a)
weaken = fmap F