{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IntervalTree
  ( IntervalTree (Empty),
    Interval (..),
    empty,
    singleton,
    null,
    size,
  )
where

import qualified CommonTypes as Common
import qualified FingerTree as Base
import Prelude hiding (null)

data SizeLastMax a = SizeLastMax
  { getSize :: Common.Size,
    getLast :: Common.Last a,
    getMax :: Common.Max a
  }
  deriving (Eq, Show)

data Interval a = Interval
  { low :: a,
    high :: a
  }
  deriving (Eq, Show)

newtype IntervalTree a = IntervalTree (Base.FingerTree (SizeLastMax a) (Interval a))

instance Ord a => Semigroup (SizeLastMax a) where
  x <> y =
    SizeLastMax
      { getSize = getSize x <> getSize y,
        getLast = getLast x <> getLast y,
        getMax = getMax x <> getMax y
      }

instance Ord a => Monoid (SizeLastMax a) where
  mempty =
    SizeLastMax
      { getSize = mempty,
        getLast = mempty,
        getMax = mempty
      }

instance Ord a => Base.Measured (Interval a) (SizeLastMax a) where
  measure x =
    SizeLastMax
      { getSize = Common.Size 1,
        getLast = Common.Last $ low x,
        getMax = Common.Max $ high x
      }

empty :: IntervalTree a
empty = IntervalTree Base.Empty

singleton :: Ord a => Interval a -> IntervalTree a
singleton = IntervalTree . Base.singleton

pattern Empty :: IntervalTree a
pattern Empty = IntervalTree Base.Empty

{- O(1) -}
null :: IntervalTree a -> Bool
null Empty = True
null _ = False

{- O(1) -}
size :: Ord a => IntervalTree a -> Int
size (IntervalTree xs) = size' xs

-- Helper functions
size' :: forall a. Ord a => Base.FingerTree (SizeLastMax a) (Interval a) -> Int
size' xs =
  let meas = Base.measure xs :: SizeLastMax a
   in Common.unSize . getSize $ meas
