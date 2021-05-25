{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}

{- An implementation of ordered multi sets -}
module MultiSet where

import Data.Function (on)
import qualified FingerTree as Base
import Prelude hiding (null)

newtype MultiSet a
  = MultiSet (Base.FingerTree (MultiSizeMax a) (MultiElem a))

data MultiElem a = MultiElem
  { getElem :: a,
    multiplicity :: Integer
  }
  deriving (Eq, Show)

data MultiSizeMax a = MultiSizeMax
  { cardinality :: Size, -- sum of all multiplicities
    supportSize :: Size, -- number of unique elements
    getMax :: Max a
  }
  deriving (Eq, Show)

data Max a
  = NegInfinity
  | Max
      { unMax :: a
      }
  deriving (Eq, Ord, Show)

newtype Size = Size
  { unSize :: Integer
  }
  deriving (Eq, Ord, Show)

instance Semigroup (MultiSizeMax a) where
  x <> y =
    MultiSizeMax
      { cardinality = cardinality x <> cardinality y,
        supportSize = supportSize x <> supportSize y,
        getMax = getMax x <> getMax y
      }

instance Monoid (MultiSizeMax a) where
  mempty =
    MultiSizeMax
      { cardinality = mempty,
        supportSize = mempty,
        getMax = mempty
      }

instance Semigroup Size where
  Size x <> Size y = Size (x + y)

instance Monoid Size where
  mempty = Size 0

instance Semigroup (Max a) where
  x <> NegInfinity = x
  _ <> x = x

instance Monoid (Max a) where
  mempty = NegInfinity

instance Base.Measured (MultiElem a) (MultiSizeMax a) where
  measure x =
    MultiSizeMax
      { cardinality = Size $ multiplicity x,
        supportSize = Size 1,
        getMax = Max $ getElem x
      }

instance Foldable MultiElem where
  foldr f z x = nTimes (fromInteger $ multiplicity x) (f $ getElem x) z

instance Foldable MultiSet where
  foldr f z (MultiSet xs) = foldr f' z xs
    where
      f' = flip $ foldr f
  foldl f z (MultiSet xs) = foldl f' z xs
    where
      f' = foldl f

instance (Show a) => Show (MultiSet a) where
  showsPrec p xs =
    showParen (p > 10) $ showString "fromList " . shows (toList xs)

empty :: MultiSet a
empty = MultiSet Base.Empty

singleton :: a -> MultiSet a
singleton = MultiSet . Base.singleton . singleElem

pattern Empty :: MultiSet a
pattern Empty = MultiSet Base.Empty

{- O(1) -}
null :: MultiSet a -> Bool
null Empty = True
null _ = False

{- O(n) -}
toList :: MultiSet a -> [a]
toList = foldr (:) []

{- See fromFoldable -}
fromList :: Ord a => [a] -> MultiSet a
fromList = fromFoldable

{- See fromAscFoldable -}
fromAscList :: Eq a => [a] -> MultiSet a
fromAscList = fromAscFoldable

{- See fromDescFoldable -}
fromDescList :: Eq a => [a] -> MultiSet a
fromDescList = fromDescFoldable

{- See fromDistinctAscFoldable -}
fromDistinctAscList :: [a] -> MultiSet a
fromDistinctAscList = fromDistinctAscFoldable

{- See fromDistinctDescFoldable -}
fromDistinctDescList :: [a] -> MultiSet a
fromDistinctDescList = fromDistinctDescFoldable

{- O(log(i)), where i <= n/2 is distance from
   insert point to nearest end -}
insert :: (Ord a) => a -> MultiSet a -> MultiSet a
insert a Empty = singleton a
insert a mset@(MultiSet xs) =
  case r of
    Base.Empty -> MultiSet $ l Base.:|> singleElem a
    x Base.:<| r' ->
      if a == getElem x
        then MultiSet $ l Base.>< (incrementElem x Base.:<| r')
        else MultiSet $ l Base.>< (singleElem a Base.:<| r)
  where
    (l, r) = Base.split ((Max a <=) . getMax) xs

-- Generalized functions
{- O(nlog(n)) -}
fromFoldable :: (Foldable f, Ord a) => f a -> MultiSet a
fromFoldable = foldr insert empty

{- O(n) -}
fromAscFoldable :: (Foldable f, Eq a) => f a -> MultiSet a
fromAscFoldable =
  MultiSet . foldr _incrInsertElemLeft Base.empty
  where
    _incrInsertElemLeft a Base.Empty = Base.singleton $ singleElem a
    _incrInsertElemLeft a xs@(x Base.:<| r) =
      if a == getElem x
        then incrementElem x Base.:<| r
        else singleElem a Base.:<| xs

{- O(n) -}
fromDescFoldable :: (Foldable f, Eq a) => f a -> MultiSet a
fromDescFoldable =
  MultiSet . foldr _incrInsertElemRight Base.empty
  where
    _incrInsertElemRight a Base.Empty = Base.singleton $ singleElem a
    _incrInsertElemRight a xs@(l Base.:|> x) =
      if a == getElem x
        then l Base.:|> incrementElem x
        else xs Base.:|> singleElem a

{- O(n) -}
fromDistinctAscFoldable :: Foldable f => f a -> MultiSet a
fromDistinctAscFoldable = MultiSet . foldr _insertElemLeft Base.empty
  where
    _insertElemLeft a xs = singleElem a Base.:<| xs

{- O(n) -}
fromDistinctDescFoldable :: Foldable f => f a -> MultiSet a
fromDistinctDescFoldable = MultiSet . foldr _insertElemRight Base.empty
  where
    _insertElemRight a xs = xs Base.:|> singleElem a

-- Helper functions
singleElem :: a -> MultiElem a
singleElem a = MultiElem {getElem = a, multiplicity = 1}

incrementElem :: MultiElem a -> MultiElem a
incrementElem x =
  MultiElem
    { getElem = getElem x,
      multiplicity = multiplicity x + 1
    }

nTimes :: Int -> (a -> a) -> a -> a
nTimes n f = foldr1 (.) $ replicate n f
