{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- An implementation of ordered multi sets -}
module PriorityQueue
  ( PriorityQueue (Empty),
    empty,
    singleton,
    null,
    size,
    -- numUniqueElems,
    -- toList,
    -- fromList,
    -- fromAscList,
    -- fromDescList,
    -- fromDistinctAscList,
    -- fromDistinctDescList,
    -- insert,
    -- deleteOnce,
    -- deleteEach,
    -- count,
    -- map,
    -- mapMonotonic,
    -- union,
    -- intersection,
    -- difference,
    -- areDisjoint,
    -- isSubsetOf,
    -- isSupsetOf,
    -- support,
    -- smallestElem,
    -- kthSmallestElem,
    -- kthSmallestUniqueElem,
    -- largestElem,
    -- kthLargestElem,
    -- kthLargestUniqueElem,
    -- fromFoldable,
    -- fromAscFoldable,
    -- fromDescFoldable,
    -- fromDistinctAscFoldable,
    -- fromDistinctDescFoldable,
  )
where

import qualified CommonTypes as Common
import qualified Data.Bifunctor as Bifunc
import Data.Function (on)
import qualified FingerTree as Base
import Prelude hiding (map, null)

data SizeMax a = SizeMax
  { getSize :: Common.Size,
    getMax :: Common.Max a
  }
  deriving (Eq, Show)

newtype Elem a = Elem
  { unElem :: a
  }
  deriving (Eq, Show)

newtype PriorityQueue a
  = PriorityQueue (Base.FingerTree (SizeMax a) (Elem a))

instance Ord a => Semigroup (SizeMax a) where
  x <> y =
    SizeMax
      { getSize = getSize x <> getSize y,
        getMax = getMax x <> getMax y
      }

instance Ord a => Monoid (SizeMax a) where
  mempty =
    SizeMax
      { getSize = mempty,
        getMax = mempty
      }

instance Functor SizeMax where
  fmap f x =
    SizeMax
      { getSize = getSize x,
        getMax = fmap f . getMax $ x
      }

instance Foldable Elem where
  foldr f z x = f (unElem x) z

instance Functor Elem where
  fmap f x =
    Elem
      { unElem = f $ unElem x
      }

instance Ord a => Base.Measured (Elem a) (SizeMax a) where
  measure x =
    SizeMax
      { getSize = Common.Size 1,
        getMax = Common.Max $ unElem x
      }

empty :: PriorityQueue a
empty = PriorityQueue Base.Empty

singleton :: Ord a => a -> PriorityQueue a
singleton = PriorityQueue . Base.singleton . Elem

pattern Empty :: PriorityQueue a
pattern Empty = PriorityQueue Base.Empty

{- O(1) -}
null :: PriorityQueue a -> Bool
null Empty = True
null _ = False

{- O(1) -}
size :: Ord a => PriorityQueue a -> Integer
size (PriorityQueue xs) = size' xs

-- Helper functions
size' :: forall a. Ord a => Base.FingerTree (SizeMax a) (Elem a) -> Integer
size' xs =
  let meas = Base.measure xs :: SizeMax a
   in Common.unSize . getSize $ meas
