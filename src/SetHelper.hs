{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SetHelper where

import Data.Maybe (fromJust)
import qualified FingerTree as Base

data Max a
  = NegInfinity
  | Max a
  deriving (Eq, Ord, Show)

newtype Size = Size
  { unSize :: Integer
  }
  deriving (Eq, Ord, Show)

instance Semigroup (Max a) where
  x <> NegInfinity = x
  _ <> x = x

instance Monoid (Max a) where
  mempty = NegInfinity

instance Functor Max where
  fmap _ NegInfinity = NegInfinity
  fmap f (Max x) = Max $ f x

instance Semigroup Size where
  Size x <> Size y = Size (x + y)

instance Monoid Size where
  mempty = Size 0
